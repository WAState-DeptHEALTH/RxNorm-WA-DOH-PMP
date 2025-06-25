# RxNorm-WA-DOH-PMP

The purpose of this repository is to share tools to access and retrieve standardized terminology and definitions for drugs and substances with a medical use, which includes the RxNorm, RxClass, and RxTerms data. The data sources are available from companion APIs to the U.S. National Library of Medicine (NLM)'s [RxNav](https://lhncbc.nlm.nih.gov/RxNav/index.html "RxNav") browser. Other sources of data includes the World Health Organization's Anatomical Therapeutic Chemical (ATC) Classification, openFDA API, PubChem, and DrugBank.

The Washington State Prescription Monitoring Program (PMP) epidemiology team uses these APIs to get more detailed and standardized drug information for tracking and studying controlled substance prescriptions. This helps with public health surveillance, research, and program evaluation. While RxNorm includes a lot of additional information that could be useful for other public health work, this R code only pulls the specific data that the Washington PMP team uses for their own purposes—so it doesn’t grab everything RxNorm offers.

-   [**RxNorm API**](https://lhncbc.nlm.nih.gov/RxNav/APIs/RxNormAPIs.html "RxNorm API")**:** This API is used to retrieve comprehensive RxNorm data. A standardized vocabulary for clinical drugs (a pharmaceutical product given to or taken by a patient with a therapeutic intent). Sources of data for normalized names and codes include: Anatomical Therapeutic Chemical Classification System (ATC), Vaccines Adminstered (CVX), DrugBank (DRUGBANK), Gold Standard Drug Database (GS), Multium MediSource Lexicon (MMSL), Micromedex Red Book (MMX), Medical Subject Headings (MESH) subset (MSH), CMS Formulary Reference File (MTHCMSFRF), FDA Structured Product Labels (MTHSPL), FDB MedKNowledge (NDDF), US Edition of SNOMED CT [drug information subset] (SNOMEDCT_US), USP Compendial Nomenclature (USP), and Veterans Health Administration National Drug File.
-   [**Prescribable RxNorm API**](https://lhncbc.nlm.nih.gov/RxNav/APIs/PrescribableAPIs.html "Prescribable RxNorm API")**:** Filter for drugs relevant to current prescription practices from the RxNorm dataset.
-   [**RxTerms API**](https://lhncbc.nlm.nih.gov/RxNav/APIs/RxTermsAPIs.html "RxTerms API")**:** Leverage user-friendly prescribing terminology linked to RxNorm. A drug terminology tool designed specifically for prescribing medications and recording medication history. It provides a user-friendly interface linked to the RxNorm standard, ensuring clear and accurate medication identification.
-   [**RxClass API**](https://lhncbc.nlm.nih.gov/RxNav/APIs/RxClassAPIs.html "RxClass API")**:** Explore drug classifications and relationships from the RxNorm dataset.
-   [**WHO ATC**](https://www.whocc.no/atc_ddd_index/)**:** A searchable version of the complete ATC index. Code will be added in later updates.

This product uses publicly available data from the U.S. National Library of Medicine (NLM), National Institutes of Health, Department of Health and Human Services; NLM is not responsible for the product and does not endorse or recommend this or any other product.

# Getting Started

RxNorm Concept Unique Identifier (rxcui) is a unique number assigned to each drug concept within the RxNorm database. An easy way to find rxcuis is to start with NDCs.

Load R libraries and functions needed.
```{r setup-environment,error = FALSE, message = F }
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

# ----------------- Load Functions --------------------------------------------------------------
source("R/func_RxNorm.R")
source("R/func_utilities.R")
```

First, the R codes requires that you create a cache directory. RxNorm only allows a maximum of 20 queries per second, so caching your result will prevent you from having to make the same request twice.

```{r setup-cache,error = FALSE, message = F }
# specify a cache directory
cache_dir <- cache_directory(fs::path_temp())
assign("cache_dir", cache_dir, envir = .GlobalEnv)
```

Next, specify a list of National Drug Code (NDC) to search RxNorm for rxcui using the `get_rxcui_from_ndc` function.

```{r example-1,error = FALSE, message = F}
 
# List of NDC to search
ndc_tibble <-
  tibble::tibble(
    ndc = c("72887068303")
  )
# unique list of ndc
ndc_list <- unique(ndc_tibble$ndc)

# find rxcui for specific ndc
ndc_rxcui <- purrr::map(ndc_list,get_rxcui_from_ndc)
ndc_rxcui <- dplyr::bind_rows(ndc_rxcui)
```

Generate a list of unique rxcui and pass it through the get_rxclass_from_rxcui function to get RxClass data for each rxcui concepts.

```{r}
# unique list of rxcui
rxcui_list <- unique(ndc_rxcui$rxcui)

# find rxclass for rxcui
rxcui_rxclass <- purrr::map(rxcui_list,get_rxclass_from_rxcui)
rxcui_rxclass <- dplyr::bind_rows(rxcui_rxclass)
```
