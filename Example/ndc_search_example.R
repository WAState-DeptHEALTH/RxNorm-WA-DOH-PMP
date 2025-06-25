# ---- About this script -----
# Script: RxNorm NDC Search
# Author: Fan Xiong, fan.xiong@doh.wa.gov
# Contributors:
#
# Created: June 6, 2025
#
# Purpose: This script does the following to enhance PMP data:
#   * Uses National Drug Codes (NDCs) to find RxCUIs (RxNorm Concept Unique Identifiers) via the RxNorm API,  
#   * Maps RxCUIs to NDCs (National Drug Codes)
#   * Retrieves drug classification information based on RxCUIs
#   * Combines results into a single dataset for easy analysis 
#
# Usage Note:
# This product uses publicly available data from the U.S. National Library of Medicine (NLM), 
# National Institutes of Health, Department of Health and Human Services; 
# NLM is not responsible for the product and does not endorse or recommend this 
# or any other product.

# ----------------- Load required libraries -----------------
library(tidyverse)
library(tidyr)
library(data.table)
library(xml2)
library(hash)
library(ratelimitr)
library(vroom)
library(lubridate)
library(doParallel)
library(purrr)
library(furrr)
library(tidytext) 
library(jsonlite)
library(fs)
library(janitor)
library(glue)
library(jsonlite)
library(digest)
library(stringr)

# ----------------- Configuration ------------------------------------------------------------------

# Allow deep recursion to handle nested structures
options(expressions = 100000)  

# Prevent large/small numbers from being displayed in scientific notation 
# Ensures numbers are displayed in a more readable, standard format
options(scipen = 999)  

# Prevent character data from being converted to factors (improves performance for text data) 
# Clarifies the purpose and benefit of this option
options(stringsAsFactors = FALSE)

# ----------------- Process Logging -----------------
print(paste0("Begin RxNorm NDC Search on ", format(Sys.Date(), "%Y-%m-%d")))
cat("System Details:\n")
sessionInfo()

# ----------------- Load Functions --------------------------------------------------------------
source("R/func_RxNorm.R")
source("R/func_utilities.R")

# Define the NDC master file name: Assumes the file contains a list of NDCs
ndc_input_file <- fs::path_abs(path = "./Data/ndc_20250602.csv")

# Define field separator: Assumes the NDC master file is a pipe-delimited file
ndc_input_file_separator <- '|' 

# ----------------- Create and Check cache environment --------------------------------------------------------------

# Create a local folder to store results in a readable 'cache'
local_cache <- fs::path(Sys.getenv("USERPROFILE"),"ndc_cache")
cache_dir <- cache_directory(local_cache)
assign("cache_dir", cache_dir, envir = .GlobalEnv)

# Create an Output Folder to store the final results 
output_sub_dir <- fs::path(Sys.getenv("USERPROFILE"),"Output")
fs::dir_create(output_sub_dir)

## ---------------- Load and preprocess input -----------------------------------------------------------------

# Load NDC data from a combined FDA file (includes both product and package info).
# FDA NDC product and package file is downloadable from: https://www.fda.gov/drugs/drug-approvals-and-databases/national-drug-code-directory?elqTrackId=b2f8af5cd98146b19b56b47feab2f6a0&elq=b28e6c325c6748e1bc1f24989a3eb0d6&elqaid=4255&elqat=1&elqCampaignId=3344
# You can use any datasets with NDC with the 11-digit NDC format.
ndc_input <- readRDS("Data/ndc_toy.rds")

# Create a list of unique NDCs for use later in the workflow
ndc_list <- unique(ndc_input$ndc)

# -----------------  Main Execution   ---------------------------------------------------

## ---------------- Execution start ---------------------------------------------------------------------------
exec_start_time <- Sys.time()
print(paste0('Script execution started at ', exec_start_time))
print(paste0('Using ', ndc_input_file, '.'))

## ---------------- Retrieve Related NDC from RxNorm -----------------------------------------------------------------
ndc_input_related <- purrr::map_dfr(ndc_list,get_related_ndcs)

# Append All Input NDC and Related NDCs
# Only use `all_ndc_list` if you are certain about wanting all related results
# This could take a long time.
all_ndc_list <- unique(c(ndc_list,ndc_input_related$ndc11))

## ---------------- Retrieve NDC Properties from RxNorm -----------------------------------------------------------------

# Use the input ndc_list for a more reasonable result
ndc_rxcui <- purrr::map_dfr(ndc_list,get_ndc_properties)
ndc_rxcui[ndc_rxcui== ""] <- NA
rxcui_list <- unique(ndc_rxcui$rxcui[is.na(ndc_rxcui$rxcui) == F])

## ---------------- Retrieve Ingredient Properties from RxNorm -----------------------------------------------------------------
ndc_rxnorm_ingredients <- purrr::map_dfr(rxcui_list,get_ingredients_dose_from_rxcui)
ndc_rxnorm_ingredients <- ndc_rxnorm_ingredients %>% dplyr::select(input_rxcui, 
                                                                   starts_with("ingredient_"),
                                                                   starts_with("precise"),
                                                                   starts_with("multiple"),
                                                                   starts_with("dose_form_group"),
                                                                   starts_with("dose_form"))

## ---------------- Retrieve Brand Name from RxNorm -----------------------------------------------------------------
ndc_rxnorm_brand_name <- purrr::map_dfr(rxcui_list,get_brand_name_from_rxcui)
ndc_rxnorm_brand_name <- ndc_rxnorm_brand_name %>% dplyr::select(input_rxcui, starts_with("brand_name"))

## ---------------- Retrieve RxTerms from Rxcui -----------------------------------------------------------------
ndc_rxterm <- purrr::map_dfr(rxcui_list,get_rxterm_from_rxcui)

## ---------------- Retrieve RxClass from Rxcui -----------------------------------------------------------------

# query RxNorm API for RxClass Information
ndc_rxclass <- purrr::map_dfr(rxcui_list,get_rxclass_from_rxcui)

# Keep only unique columns and rows
ndc_rxclass_tidy <- ndc_rxclass %>% dplyr::distinct(input_rxcui,classId,className,classType,classUrl,relaSource)

# Keep only selected results
ndc_rxclass_tidy <- 
  ndc_rxclass_tidy %>% 
  dplyr::filter(
    relaSource %in% c("MEDRT","ATC","ATCPROD","RXNORM","VA","SNOMEDCT","FDASPL","DAILYMED")
  ) %>%
  dplyr::group_by(input_rxcui,classType) %>% 
  dplyr::mutate(
    classType = janitor::make_clean_names(classType),
    classSequence = dplyr::row_number(),
    classVarSequence = paste0(classType,"_",classSequence)) %>%
  dplyr::arrange(input_rxcui,classType,classSequence)

# pivot wider to deduplicate by class names
ndc_rxclass_name <- 
  ndc_rxclass_tidy %>% 
  tidyr::pivot_wider(
    id_cols = "input_rxcui",
    names_from = "classVarSequence",
    values_from = "className",
    names_prefix = "class_name_",
    names_sort = TRUE
  )

# pivot wider to deduplicate by class IDs (useful for looking up things)
ndc_rxclass_id <- 
  ndc_rxclass_tidy %>% 
  tidyr::pivot_wider(
    id_cols = "input_rxcui",
    names_from = "classVarSequence",
    values_from = "classId",
    names_prefix = "class_id_"
  )

## ---------------- Join All Information -----------------------------------------------------------------
ndc_all <- NULL

ndc_all <- 
  ndc_input %>% 
  dplyr::inner_join(
    ndc_rxcui, 
    by = c("ndc" = "ndc_item" ),
    relationship = "many-to-many"
  ) %>%
  dplyr::inner_join(
    ndc_rxterm, 
    by = c("rxcui"),
    relationship = "many-to-many"
  ) %>% 
  dplyr::inner_join(
    ndc_rxclass_name, by = c("input_rxcui"),
    relationship = "many-to-many"
  ) %>%
  dplyr::inner_join(
    ndc_rxclass_id, by = c("input_rxcui"),
    relationship = "many-to-many"
  ) %>%
  dplyr::inner_join(
    ndc_rxnorm_ingredients,
    by = c("input_rxcui"),
    relationship = "many-to-many"
  ) %>%
  dplyr::inner_join(
    ndc_rxnorm_brand_name ,
    by = c("input_rxcui"),
    relationship = "many-to-many"
  ) %>%
  dplyr::distinct() %>%
  dplyr::distinct() %>%
  dplyr::mutate(across(everything(),clean_string)) %>%
  dplyr::mutate(across(everything(),blank_to_na)) %>%
  janitor::clean_names() 

glimpse(ndc_all)

## ----------------  Write the final map to a CSV file. ----------------  

if(nrow(ndc_all) > 0) {
  csv_outfile <- fs::path(output_sub_dir,"rxnorm_ndc_all",ext = "csv")
  readr::write_delim(ndc_all, csv_outfile, delim = ndc_input_file_separator)
}

## ----------------  Execution end -------------------------------------------------------------------------------
exec_end_time <- Sys.time()
print(paste0('Script execution completed at ', exec_end_time))
print(round(exec_end_time-exec_start_time, 1))
