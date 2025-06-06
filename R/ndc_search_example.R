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
library(furrr)
library(tidytext) 
library(jsonlite)
library(fs)
library(janitor)

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

# Create an Output Folder to store the final results 
output_sub_dir <- fs::path_abs("./Output")
fs::dir_create(output_sub_dir)

# Define the NDC master file name: Assumes the file contains a list of NDCs
ndc_input_file <- fs::path_abs(path = "./Data/ndc_20250602.csv")

# Define field separator: Assumes the NDC master file is a pipe-delimited file
ndc_input_file_separator <- '|' 

# ----------------- Create and Check cache environment --------------------------------------------------------------

# Create a local folder to store results in a readable 'cache'
local_cache <- fs::path_abs("./ndc_cache")
cache_dir <- cache_directory(local_cache)

assign("cache_dir", cache_dir, envir = .GlobalEnv)


## ---------------- Load and preprocess input -----------------------------------------------------------------

# Load NDC data from a combined FDA file (includes both product and package info).
# `ndc_input_file` is the path to this file, and `ndc_input_file_separator` is the delimiter used in the file.
# FDA NDC product and package file is downloadable from: https://www.fda.gov/drugs/drug-approvals-and-databases/national-drug-code-directory?elqTrackId=b2f8af5cd98146b19b56b47feab2f6a0&elq=b28e6c325c6748e1bc1f24989a3eb0d6&elqaid=4255&elqat=1&elqCampaignId=3344
# You can use any datasets with NDC with the 11-digit NDC format.

ndc_input <- readr::read_delim(file = ndc_input_file, delim = ndc_input_file_separator, 
                               col_types = c("NDCPACKAGECODE" = "character",  # Ensure NDC codes stay as text
                                             "STARTMARKETINGDATE" = "character",
                                             "ENDMARKETINGDATE" = "character")) %>% 
  janitor::clean_names() %>%  # Clean column names: makes them lowercase, snake_case, and syntactically valid
  dplyr::mutate(
    # Convert marketing date strings into proper Date objects using the format YYYYMMDD
    startmarketingdate = as.Date(startmarketingdate, "%Y%m%d"),
    endmarketingdate = as.Date(endmarketingdate, "%Y%m%d")
  ) %>% 
  # Keep only rows that have a DEA schedule listed (controlled substances)
  dplyr::filter(!is.na(deaschedule)) %>%  
  # Split the full NDC package code into its three segments
  tidyr::separate(
    col = ndcpackagecode,  
    into = c("ndcsegment1", "ndcsegment2", "ndcsegment3"),
    # Hyphen was used as the delimiter between segments in the FDA NDC files.
    sep = "-",             
    # Keep the original column too
    remove = FALSE        
  ) %>%
  dplyr::mutate(
    # Concatenate segments into a single NDC code, after cleaning
    ndc = clean_string(paste0(ndcsegment1, ndcsegment2, ndcsegment3))
  ) %>%
  dplyr::mutate(
    # Pad each NDC segment with leading zeroes to ensure standardized formatting
    ndcsegment1 = stringr::str_pad(ndcsegment1, width = 5, pad = "0"),
    ndcsegment2 = stringr::str_pad(ndcsegment2, width = 4, pad = "0"),
    ndcsegment3 = stringr::str_pad(ndcsegment3, width = 2, pad = "0"),
    
    # Recreate the full NDC by concatenating the padded segments
    ndc = paste0(ndcsegment1, ndcsegment2, ndcsegment3)
  ) %>% 
  # Randomly sample 100 rows for further processing (useful for testing/debugging)
  # Only used for the purpose of testing this code.
  dplyr::sample_n(size = 100)  %>%  
  

# Filter the master list to remove any entries with an empty or missing NDC
ndc_input <- ndc_input %>% dplyr::filter(stringr::str_length(ndc) > 0)

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
