#' R functions to build and retrieve from RxNorm APIs

#' @title rxnav_rxnorm_url
#'
#' @description
#' `rxnav_rxnorm_url` uses `glue` to concatenate suffixes to API endpoints at the
#' RxNorm API from 'https://rxnav.nlm.nih.gov/REST/'.
#'
#' @param ... Any API endpoints and suffixes.
#'
#' @returns
#' A url to an API endpoint from 'https://rxnav.nlm.nih.gov/REST/'
#' @export
rxnav_rxnorm_url <- function(...) {
  tryCatch({ 
    # Construct the full query URL
    full_query_url <- glue::glue("https://rxnav.nlm.nih.gov/REST/", ...)
    return(full_query_url)
  }, error = function(e) {
    message("Error in function rxnav_rxnorm_url(): ", e)
    stop(e)
  })
}


#' @title get_rxnorm_version
#'
#' @description
#' `get_rxnorm_version` retrieves the current version of RxNorm at execution.
#'
#' @returns
#' The RxNorm version from 'https://rxnav.nlm.nih.gov/REST/'.
#'
#' @export
get_rxnorm_version <- function(){
  tryCatch({ 
    # Construct the full query URL
    full_query_url <-  URLencode(rxnav_rxnorm_url("version.json"))
    rxnorm_version <- get_json(url = full_query_url)
    return(
      tibble::tibble(
        version = format(as.Date(rxnorm_version$version,"%d-%b-%Y"),"%Y-%m%-%d"),
        api_version = rxnorm_version$apiVersion
      )
    )
  }, error = function(e) {
    message("Error in function get_rxnorm_version(): ", e)
    stop(e)
  })
}


#' @title rxnav_prescribe_url
#'
#' @description
#' `rxnav_prescribe_url` uses `glue` to concatenate suffixes to API endpoints at the
#' Prescrible RxNorm API from `https://rxnav.nlm.nih.gov/REST/Prescribe/`
#'
#' @param ... Any API endpoints and suffixes.
#'
#' @returns
#' A url to an API endpoint from 'https://rxnav.nlm.nih.gov/REST/Prescribe'
#'
#' @export
rxnav_prescribe_url <- function(...) {
  # Construct the full query URL
  full_query_url <- glue::glue("https://rxnav.nlm.nih.gov/REST/Prescribe/", ...)
  return(full_query_url)
}

#' @title rxnav_rxterms_url

#' @description
#' `rxnav_rxterms_url` uses `glue` to concatenate suffixes to API endpoints at the
#' RxTerm API from `https://rxnav.nlm.nih.gov/REST/RxTerms/`
#'
#' @param ... Any API endpoints and suffixes.
#'
#' @returns
#' A url to an API endpoint from 'https://rxnav.nlm.nih.gov/RxTerms'
#'
#' @export
rxnav_rxterms_url <- function(...) {
  tryCatch({ 
    # Construct the full query URL
    full_query_url <- glue::glue("https://rxnav.nlm.nih.gov/REST/RxTerms/", ...)
    return(full_query_url)
  }, error = function(e) {
    message("Error in function rxnav_rxterms_url(): ", e)
    stop(e)
  })
}

#' @title get_rxterms_version
#'
#' @description
#' `get_rxterms_version` retrieves the current version of RxTerm at execution.
#'
#' @returns
#' The RxTerm version from 'https://rxnav.nlm.nih.gov/RxTerms'.
#'
#' @export
get_rxterms_version <- function() {
  tryCatch({ 
    # Construct the full query URL
    full_query_url <- URLencode(rxnav_rxterms_url("version.json"))
    rxterms_version <- get_json(url = full_query_url)
    return(rxterms_version$rxtermsVersion)
  }, error = function(e) {
    message("Error in function get_rxterms_version(): ", e)
    stop(e)
  })
}

#' @title rxnav_rxclass_url
#'
#' @description
#' `rxnav_rxclass_url` uses `glue` to concatenate suffixes to API endpoints at the
#' RxClass API from `https://rxnav.nlm.nih.gov/REST/rxclass/`
#'
#' @param ... Any API endpoints and suffixes.
#'
#' @returns
#' A url to an API endpoint from 'https://rxnav.nlm.nih.gov/rxclass'
#'
#' @export
rxnav_rxclass_url <- function(...) {
  tryCatch({ 
    # Construct the full query URL
    full_query_url <- glue::glue("https://rxnav.nlm.nih.gov/REST/rxclass/", ...)
    return(full_query_url)
  }, error = function(e) {
    message("Error in function rxnav_rxclass_url(): ", e)
    stop(e)
  })
}

#' @title get_rxclass_data_version
#'
#' @description
#' `get_rxclass_data_version` retrieves the current version of of data used to create the
#' RxClass dataset for API endpoints at `https://rxnav.nlm.nih.gov/REST/rxclass/`.
#'
#' @param rxclass_data A quoted character string with the following values:
#'  ATC,ATCPROD,DAILYMED,FDASPL,FMTSME,MEDRT,RXNORM,SNOMEDCT,or VA.
#'
#' @returns
#' The current version of a data source used create the RxClass dataset and associated API endpoints.
#' @export
get_rxclass_data_version <- function(rxclass_data) {
  tryCatch({ 
    
    if(!data_source %in% c('ATC','ATCPROD','DAILYMED','FDASPL','FMTSME','MEDRT','RXNORM','SNOMEDCT','VA'))
    {
      stop("rxclass_data must be one of 'ATC','ATCPROD','DAILYMED','FDASPL','FMTSME','MEDRT','RXNORM','SNOMEDCT','VA'")
    }
    # Construct the full query URL
    full_query_url <- URLencode(rxnav_rxclass_url("version/",rxclass_data,".json"))
    rxclass_data_version <- get_json(url = full_query_url)
    return(rxclass_data_version$relaSourceVersion)
  }, error = function(e) {
    message("Error in function get_rxclass_data_version(): ", e)
    stop(e)
  })
}

#' @title get_brand_names
#'
#' @description
#' `get_brand_names` retrieves all brand name products from API endpoints at the RxNorm API.
#'
#' @returns
#' A url to the json API endpoint at 'https://rxnav.nlm.nih.gov/REST/allconcepts.json?' with the
#' suffix: 'tty=BN' (i.e. 'https://rxnav.nlm.nih.gov/REST/allconcepts.json?tty=BN)
#'
#' @export
get_brand_names <- function() {
  tryCatch({ 
    
    # Construct the full query URL
    return(rxnav_rxnorm_url("allconcepts.json?tty=BN"))
  }, error = function(e) {
    message("Error in function get_brand_names(): ", e)
    stop(e)
  })
}

#' @title get_all_rxcui_any_status
#' @description
#' `get_all_rxcui_any_status` retrieves all rxcui concepts with any status from the RxNorm API.
#'
#' @returns
#' A url to the json API endpoint at 'https://rxnav.nlm.nih.gov/REST/allstatus.json?' with the
#' suffix: 'status=any' (i.e. 'https://rxnav.nlm.nih.gov/REST/allstatus.json?status=any)
#'
#' @export
get_all_rxcui_any_status <- function() {
  tryCatch({ 
    # Construct the full query URL
    return(rxnav_rxnorm_url("allstatus.json?status=any"))
  }, error = function(e) {
    message("Error in function get_all_rxcui_any_status(): ", e)
    stop(e)
  })
}


#' @title get_all_ndcs
#'
#' @description
#' `get_all_ndcs` retrieves all NDC concepts from the RxNorm API.
#'
#' @returns
#' A url to the json API endpoint at 'https://rxnav.nlm.nih.gov/REST/allNDCstatus.json?' with the
#' suffix: 'status=ALL' (i.e. 'https://rxnav.nlm.nih.gov/REST/allNDCstatus.json?status=ALL)
#'
#' @export
get_all_ndcs <- function() {
  tryCatch({ 
    # Construct the full query URL
    return(rxnav_rxnorm_url("allNDCstatus.json?status=ALL"))
  }, error = function(e) {
    message("Error in function get_all_ndcs(): ", e)
    stop(e)
  })
}

#' @title get_ingredient
#'
#' @description
#' `get_ingredient` retrieves all ingredients concepts from the RxNorm API.
#'
#' @returns
#' A url to the json API endpoint at 'https://rxnav.nlm.nih.gov/REST/allconcepts.json?' with the
#' suffix: 'tty=IN' (i.e. 'https://rxnav.nlm.nih.gov/REST/allconcepts.json?tty=IN)
#'
#' @export
get_ingredient <- function() {
  tryCatch({ 
    # Construct the full query URL
    return(rxnav_rxnorm_url("allconcepts.json?tty=IN"))
  }, error = function(e) {
    message("Error in function get_ingredient(): ", e)
    stop(e)
  })
}

#' @title get_precise_ingredient
#'
#' @description
#' `get_precise_ingredient` retrieves all precise ingredients concepts from the RxNorm API, which
#' includes salt isomers.
#'
#' @returns
#' A url to the json API endpoint at 'https://rxnav.nlm.nih.gov/REST/allconcepts.json?' with the
#' suffix: 'tty=PIN' (i.e. 'https://rxnav.nlm.nih.gov/REST/allconcepts.json?tty=PIN)
#'
#' @export
get_precise_ingredient <- function() {
  tryCatch({ 
    # Construct the full query URL
    return(rxnav_rxnorm_url("allconcepts.json?tty=PIN"))
  }, error = function(e) {
    message("Error in function get_precise_ingredient(): ", e)
    stop(e)
  })
}

#' @title get_multiple_ingredient
#'
#' @description
#' `get_precise_ingredient` retrieves all multiple ingredients concepts from the RxNorm API.
#'
#' @returns
#' A url to the json API endpoint at 'https://rxnav.nlm.nih.gov/REST/allconcepts.json?' with the
#' suffix: 'tty=MIN' (i.e. 'https://rxnav.nlm.nih.gov/REST/allconcepts.json?tty=MIN)
#'
#' @export
get_multiple_ingredient <- function() {
  tryCatch({ 
    # Construct the full query URL
    return(rxnav_rxnorm_url("allconcepts.json?tty=MIN"))
  }, error = function(e) {
    message("Error in function get_multiple_ingredient(): ", e)
    stop(e)
  })
}

#' @title get_brand_name_from_rxcui
#'
#' @description
#' `get_brand_name_from_rxcui` retrieves all brand name concepts from the RxNorm API for an input rxcui.
#'
#' @param input_rxcui an rxcui concept.
#'
#' @returns
#' A tibble with a column for the `input_rxcui` and the brand name and ingredient concepts.
#'
#' @export
get_brand_name_from_rxcui <- function(input_rxcui) {
  tryCatch({ 
    api_tibble <- tibble::tibble(input_rxcui = input_rxcui, brand_name_1 = NA_character_,
                                 ingredient_1 = NA_character_)
    # Handle empty or NA inputs
    if (is.null(input_rxcui) || all(is.na(input_rxcui))) {
      stop("Input rxcui cannot be missing.")
    }
    
    # Construct the API query URL
    query_address <- URLencode(rxnav_rxnorm_url("rxcui/", input_rxcui, "/related?tty=BN+IN"))
    
    # Create a unique identifier for the cache file
    cache_key <- digest::digest(list(query_address, input_rxcui), algo = "sha256", serialize = TRUE)
    cache_file <- fs::path(cache_dir,cache_key)
    
    # Check if cached data exists and is fresh
    if(fs::file_exists(cache_file)) {
      api_tibble <- readRDS(file = cache_file)
    } else {
      # Retrieve the JSON response from the API
      api_response <- get_json(url = query_address)
      
      # Process API response and construct output
      api_tibble <- purrr::list_rbind(api_response$relatedGroup)
      
      if (all(colnames(api_tibble) == "tty") == T) {
        return(api_tibble <- tibble::tibble(input_rxcui = input_rxcui, brand_name_1 = NA_character_,
                                            ingredient_1 = NA_character_) %>%
                 dplyr::select(input_rxcui, starts_with("brand_name"), starts_with("ingredient")))
      } else {
        api_tibble <- dplyr::bind_rows(api_response$relatedGroup$conceptGroup$conceptProperties)
        
        api_tibble <- dplyr::mutate(api_tibble, input_rxcui = input_rxcui)
        api_tibble <- dplyr::arrange(api_tibble, input_rxcui)
        api_tibble <- dplyr::mutate(api_tibble, tty = dplyr::case_match(tty, "BN" ~
                                                                          "Brand Name", "IN" ~ "Ingredient", .default = tty), tty = janitor::make_clean_names(tty))
        
        api_tibble <- dplyr::mutate(api_tibble, tty = dplyr::case_match(tty, "brand_name" ~
                                                                          "brand_name_1", "ingredient" ~ "ingredient_1", .default = tty))
        
        api_tibble <- dplyr::mutate(api_tibble, across(everything(), clean_string))
        
        api_tibble <- tidyr::pivot_wider(api_tibble, id_cols = "input_rxcui", names_from = "tty",
                                         values_from = "name")
        api_tibble <- janitor::clean_names(api_tibble)
        
        # Add 'brand_name_1' if it's missing
        if (!"brand_name_1" %in% colnames(api_tibble)) {
          api_tibble <- dplyr::mutate(api_tibble, brand_name_1 = NA_character_)
        }
        
        api_tibble <- dplyr::select(api_tibble, input_rxcui, starts_with("brand_name"),
                                    starts_with("ingredient"))
        
        # Save the result to the cache file
        saveRDS(api_tibble, cache_file)
      }
    }
    return(api_tibble)
  }, error = function(e) {
    message("Error in function get_brand_name_from_rxcui(): ", e)
    stop(e)
  })
}

#' @title get_ingredients_dose_from_rxcui
#'
#' @description
#' `get_ingredients_dose_from_rxcui` retrieves the ingredients, precise ingredients, multiple ingredients, and dosage form from the RxNorm API
#' for an input rxcui.
#'
#' @param input_rxcui an rxcui concept.
#'
#' @returns
#' A tibble with a column for the `input_rxcui` and the found ingredients, precise ingredients, multiple ingredients, and dosage form concepts.
#'
#' @export
get_ingredients_dose_from_rxcui <- function(input_rxcui) {
  tryCatch({ 
    # Handle empty or NA inputs
    if (is.null(input_rxcui) || all(is.na(input_rxcui))) {
      stop("Input rxcui cannot be missing.")
    }
    
    # Construct the API query URL
    query_address <- URLencode(rxnav_rxnorm_url("rxcui/", input_rxcui, "/related?tty=IN+MIN+PIN+DF+DFG"))
    
    # Create a unique identifier for the cache file
    cache_key <- digest::digest(list(query_address, input_rxcui), algo = "sha256", serialize = TRUE)
    cache_file <- fs::path(cache_dir,cache_key)
    
    # Check if cached data exists and is fresh
    if(fs::file_exists(cache_file)) {
      api_tibble <- readRDS(file = cache_file)
    } else {
      # Retrieve the JSON response from the API
      api_response <- get_json(url = query_address)
      
      # Process API response and construct output
      api_tibble <- purrr::list_rbind(api_response$relatedGroup)
      
      if (all(colnames(api_tibble) == "tty") == T) {
        return(api_tibble <- tibble::tibble(input_rxcui = input_rxcui, ingredient_1 = NA_character_,
                                            multiple_ingredient_1 = NA_character_, precise_ingredient_1 = NA_character_,
                                            dose_form_1 = NA_character_, dose_form_group_1 = NA_character_) %>%
                 dplyr::select(input_rxcui, starts_with("ingredient"), starts_with("multiple_ingredient"),
                               starts_with("precise_ingredient"), starts_with("dose_form_group"),
                               starts_with("dose_form")))
      } else {
        
        api_tibble <- bind_rows(lapply(api_tibble$conceptProperties, clean_purrr_list))
        api_tibble <- dplyr::mutate(api_tibble, input_rxcui = input_rxcui)
        api_tibble <- dplyr::arrange(api_tibble, input_rxcui)
        
        api_tibble <- dplyr::mutate(api_tibble, tty = dplyr::case_match(tty, "IN" ~
                                                                          "Ingredient", "PIN" ~ "Precise Ingredient", "MIN" ~ "Multiple Ingredient",
                                                                        "DF" ~ "Dose Form", "DFG" ~ "Dose Form Group", .default = tty), tty = janitor::make_clean_names(tty))
        
        api_tibble <- dplyr::mutate(api_tibble, tty = dplyr::case_match(janitor::make_clean_names(tty),
                                                                        "ingredient" ~ "ingredient_1", "precise_ingredient" ~ "precise_ingredient_1",
                                                                        "multiple_ingredient" ~ "multiple_ingredient_1", "dose_form" ~ "dose_form_1",
                                                                        "dose_form_group" ~ "dose_form_group_1", .default = tty))
        
        api_tibble <- dplyr::mutate(api_tibble, across(everything(), clean_string))
        
        api_tibble <- dplyr::filter(api_tibble, is.na(name) == F)
        api_tibble <- tidyr::pivot_wider(api_tibble, id_cols = "input_rxcui", names_from = "tty",
                                         values_from = "name")
        api_tibble <- janitor::clean_names(api_tibble)
        
        # Add 'ingredient_1' if it's missing
        if (!"ingredient_1" %in% colnames(api_tibble)) {
          api_tibble <- dplyr::mutate(api_tibble, ingredient_1 = NA_character_)
        }
        
        # Add 'multiple_ingredient_1' if it's missing
        if (!"multiple_ingredient_1" %in% colnames(api_tibble)) {
          api_tibble <- dplyr::mutate(api_tibble, multiple_ingredient_1 = NA_character_)
        }
        
        # Add 'precise_ingredient_1' if it's missing
        if (!"precise_ingredient_1" %in% colnames(api_tibble)) {
          api_tibble <- dplyr::mutate(api_tibble, precise_ingredient_1 = NA_character_)
        }
        
        # Add 'dose_form_1' if it's missing
        if (!"dose_form_group_1" %in% colnames(api_tibble)) {
          api_tibble <- dplyr::mutate(api_tibble, dose_form_1 = NA_character_)
        }
        
        # Add 'dose_form_1' if it's missing
        if (!"dose_form_1" %in% colnames(api_tibble)) {
          api_tibble <- dplyr::mutate(api_tibble, dose_form_group_1 = NA_character_)
        }
        api_tibble <- dplyr::select(api_tibble, input_rxcui, starts_with("ingredient"),
                                    starts_with("multiple_ingredient"), starts_with("precise_ingredient"),
                                    starts_with("dose_form_group"), starts_with("dose_form"))
        
        
        # Save the result to the cache file
        saveRDS(api_tibble, cache_file)
      }
    }
    return(api_tibble)
  }, error = function(e) {
    message("Error in function get_ingredients_dose_from_rxcui(): ", e)
    stop(e)
  })
}

#' @title get_ndc_from_rxcui
#'
#' @description
#' `get_ndc_from_rxcui` retrieves the associated National Drug Code (NDC) concepts for an input rxcui.
#'
#' @param input_rxcui an rxcui concept.
#'
#' @returns
#' A tibble with a column for the `input_rxcui` and the found NDC.
#' There is one row per NDC and each `input_rxcui`.
#'
#' @export
get_ndc_from_rxcui <- function(input_rxcui) {
  tryCatch({ 
    
    # Handle empty or NA inputs
    if (is.null(input_rxcui) || all(is.na(input_rxcui))) {
      stop("Input rxcui cannot be missing.")
    }
    
    query_address <- URLencode(rxnav_rxnorm_url("rxcui/", input_rxcui, "/ndcs.json"))
    
    # Create a unique identifier for the cache file
    cache_key <- digest::digest(list(query_address, input_rxcui), algo = "sha256", serialize = TRUE)
    cache_file <- fs::path(cache_dir,cache_key)
    
    # Check if cached data exists and is fresh
    if(fs::file_exists(cache_file)) {
      api_tibble <- readRDS(file = cache_file)
    } else {
      api_response <- get_json(url = query_address)
      
      ndc_response <- api_response$ndcGroup$ndcList$ndc
      
      ndc_tibble <- tibble::tibble(rxcui = input_rxcui, ndc = NA_character_)
      
      if(is.null(ndc_response) == F){
        ndc_tibble <- tibble::tibble(rxcui = rep(input_rxcui,length(ndc_response)), ndc = rep(ndc_response,length(input_rxcui)))
      }
      
      # Construct the output tibble
      api_tibble <- dplyr::bind_rows(ndc_tibble)
      api_tibble <- dplyr::filter(api_tibble, is.na(ndc) == F)
      api_tibble <- dplyr::distinct(api_tibble,rxcui,ndc)
      
      # Save the result to the cache file
      saveRDS(api_tibble, cache_file)
    }
    return(api_tibble)
  }, error = function(e) {
    message("Error in function get_ndc_from_rxcui(): ", e)
    stop(e)
  })
}

#' @title get_historical_ndc_from_rxcui
#'
#' @description
#' `get_historical_ndc_from_rxcui` retrieves the associated National Drug Code (NDC) concepts for an input rxcui.
#'
#' @param input_rxcui an rxcui concept.
#'
#' @returns
#' A tibble with a column for the `input_rxcui` and the found NDC,status, start_date, and end_date
#' There is one row per NDC and each `input_rxcui`.
#'
#' @export
get_historical_ndc_from_rxcui <- function(input_rxcui) {
  tryCatch({ 
    
    # Handle empty or NA inputs
    if (is.null(input_rxcui) || all(is.na(input_rxcui))) {
      stop("Input rxcui cannot be missing.")
    }
    
    query_address <- URLencode(rxnav_rxnorm_url("rxcui/", input_rxcui, "/allhistoricalndcs.json"))
    
    # Create a unique identifier for the cache file
    cache_key <- digest::digest(list(query_address, input_rxcui), algo = "sha256", serialize = TRUE)
    cache_file <- fs::path(cache_dir,cache_key)
    
    # Check if cached data exists and is fresh
    if(fs::file_exists(cache_file)) {
      api_tibble <- readRDS(file = cache_file)
    } else {
      api_response <- get_json(url = query_address)
      
      api_tibble <- tibble::as_tibble(api_response[["historicalNdcConcept"]][["historicalNdcTime"]])
      api_tibble <- tibble::as_tibble(unnest(api_tibble,ndcTime))
      api_tibble <- api_tibble %>% dplyr::mutate(across(everything(),as.character))
      api_tibble <- api_tibble %>% janitor::clean_names()
      api_tibble <- api_tibble %>% dplyr::rename(c("rxcui_association" = "status"))
      # Save the result to the cache file
      saveRDS(api_tibble, cache_file)
    }
    return(api_tibble)
  }, error = function(e) {
    message("Error in function get_historical_ndc_from_rxcui(): ", e)
    stop(e)
  })
}

#' @title Retrieve NDC Status
#' @description Retrieves the status of a National Drug Code (NDC) using the RxNorm API.
#' @export
get_ndc_status <- function(input_ndc,cache_load = FALSE) {
  tryCatch({ 
    
    # Validate input to ensure it is neither null nor NA
    if (is.null(input_ndc) || anyNA(input_ndc)) {
      stop("Input NDC cannot be missing.")
    }
    
    # Main logic flow
    query_address <- URLencode(rxnav_rxnorm_url("ndcstatus.json?ndc=", input_ndc,"&ndcstatus=ALL"))
    # Create a unique identifier for the cache file
    cache_key <- digest::digest(list(query_address, input_ndc), algo = "sha256", serialize = TRUE)
    cache_file <- fs::path(cache_dir,cache_key)
    
    # Check if cached data exists and is fresh
    if(fs::file_exists(cache_file)) {
      api_tibble <- readRDS(file = cache_file)
    } else {
      # Fetch and process API data
      api_response <- jsonlite::fromJSON(query_address, simplifyVector = TRUE)
      if (pluck_exists(api_response,"ndcStatus")) {
        
        # Extract the main NDC status data
        ndc_status <- 
          purrr::pluck(api_response,"ndcStatus") %>% 
          purrr::discard_at("sourceList") %>%
          purrr::discard_at("ndcSourceMapping") %>%
          purrr::discard_at("ndcHistory") %>%
          dplyr::bind_rows()
        
        # Extract and unnest ndcHistory
        if(pluck_exists(api_response$ndcStatus,"ndcHistory")){
          ndc_history <- purrr::pluck(api_response$ndcStatus,"ndcHistory")
          api_tibble <- ndc_history %>% dplyr::bind_cols(ndc_status)
          return(api_tibble)
        }
        else {
          api_tibble <- tibble::tibble(
            activeRxcui = NA_character_,
            originalRxcui = NA_character_,
            startDate = NA_character_,
            endDate = NA_character_
          ) %>% 
            dplyr::bind_cols(ndc_status)
        }
        api_tibble$input_ndc <- input_ndc
        api_tibble <- janitor::clean_names(api_tibble)
        api_tibble <- dplyr::arrange(api_tibble, ndc11)
        return(api_tibble)
      } else {
        api_tibble <- 
          tibble::tibble(
            activeRxcui = NA_character_,
            originalRxcui = NA_character_,
            startDate = NA_character_,
            endDate = NA_character_,
            ndc11 = NA_character_,
            status = NA_character_,
            active = NA_character_,
            rxnormNdc = NA_character_,
            conceptName = NA_character_,
            conceptStatus = NA_character_,
            altNdc = NA_character_,
            input_ndc = input_ndc
          ) %>% 
          janitor::clean_names()
        return(api_tibble)
      }
    }
    saveRDS(api_tibble, cache_file)
    
    return(api_tibble)
  }, error = function(e) {
    message("Error in function get_ndc_status(): ", e)
    stop(e)
  })
}

#' @title get_ndc_properties
#' @description
#' Retrieves RxNorm Concepts for a given NDC
#' @export
get_ndc_properties <- function(input_ndc) {
  tryCatch({ 
    
    # Handle empty or NA inputs
    if (is.null(input_ndc) || all(is.na(input_ndc))) {
      stop("Input ndc cannot be missing.")
    }
    
    # Construct the API query URL
    query_address <- URLencode(rxnav_rxnorm_url("ndcproperties.json?id=", input_ndc,"&ndcstatus=all"))
    
    # Create a unique identifier for the cache file
    cache_key <- digest::digest(list(query_address, input_ndc), algo = "sha256", serialize = TRUE)
    cache_file <- fs::path(cache_dir,cache_key)
    
    
    api_tibble <- 
      tibble::tibble(
        ndc_item = input_ndc, 
        ndc9 = NULL,
        ndc10 = NULL,
        rxcui = NULL,
        spl_set_id_item = NULL,
        anda = NULL,
        colortext = NULL,
        color = NULL,
        dcsa = NULL,
        dm_spl_id = NULL,
        imprint_code = NULL,
        labeler = NULL,
        label_type = NULL,
        marketing_category = NULL,
        marketing_effective_time_low = NULL,
        marketing_status = NULL,
        score = NULL,
        shapetext = NULL,
        shape = NULL,
        size = NULL
      )
    
    # Check if cached data exists and is fresh
    if(fs::file_exists(cache_file)) {
      api_tibble <- readRDS(file = cache_file)
    } else {
      #Query Data
      # Retrieve the JSON response from the API
      api_response <- get_json(url = query_address)
      
      if(pluck_exists(api_response,"ndcPropertyList")){
        # Extract the NDC properties (with clear variable names)
        ndcProperty <- api_response[["ndcPropertyList"]][["ndcProperty"]]
        ndcProperty <- dplyr::bind_rows(ndcProperty)
        ndcProperty <- janitor::clean_names(ndcProperty)
        
        # Check if "property_concept_list" doesn't exist in the ndc_drug data frame
        # If it doesn't, create the column filled with NA_character_
        if (("property_concept_list" %in% colnames(ndcProperty))) {
          ndcProperty <- tidyr::unnest(ndcProperty,property_concept_list)
          ndcProperty <- tidyr::unnest(ndcProperty,propertyConcept)
          ndcProperty <- ndcProperty %>% dplyr::mutate(across(everything(),as.character))
          ndcProperty <- janitor::clean_names(ndcProperty)
          ndcProperty <- tidyr::pivot_wider(ndcProperty,
                                            id_cols = c("ndc_item","ndc9","ndc10",
                                                        "rxcui","spl_set_id_item","source"),
                                            names_from = "prop_name",
                                            values_from = "prop_value")
        }
        
        # Check if "source" doesn't exist in the ndc_drug data frame
        # If it doesn't, create the column filled with NA_character_
        if (("source" %in% colnames(ndcProperty))) {
          ndcProperty <- dplyr::select(ndcProperty,-source)
        }
        
        ndcProperty <- janitor::clean_names(ndcProperty)
        ndcProperty <- ndcProperty %>% dplyr::mutate(across(everything(),as.character))
        
        # Convert into tibble
        api_tibble <- tibble::as_tibble(ndcProperty)
        api_tibble <- dplyr::arrange(api_tibble, ndc_item)
        
        # Save the result to the cache file
        saveRDS(api_tibble, cache_file)
      }
    }
    return(api_tibble)
  }, error = function(e) {
    message("Error in function get_ndc_properties(): ", e)
    stop(e)
  })
}

#' @title get_related_ndcs
#' @description
#'Retrieves NDCs related by NDC product, RxNorm concept, or RxNorm drug product
#' @export
get_related_ndcs <- function(input_ndc) {
  tryCatch({ 
    
    api_tibble <- tibble(input_ndc = NA_character_, ndc11 = NA_character_, rxcui = NA_character_,
                         status = NA_character_, conceptName = NA_character_, conceptStatus = NA_character_, tty = NA_character_)

    # Handle empty or NA inputs
    if (is.null(input_ndc) || all(is.na(input_ndc))) {
      stop("Input ndc cannot be missing.")
    }
    
    # Check if NDC is 11 character length
    if (stringr::str_length(input_ndc) != 11) {
      stop("Input ndc must be in 11 digit format.")
    }
    
    
    # Construct the API query URL
    query_concept <- URLencode(rxnav_rxnorm_url("relatedndc.json?&relation=concept&ndcstatus=all&ndc=", input_ndc))
    query_product <- URLencode(rxnav_rxnorm_url("relatedndc.json?&relation=product&ndcstatus=all&ndc=", input_ndc))
    query_drug <- URLencode(rxnav_rxnorm_url("relatedndc.json?&relation=drug&ndcstatus=all&ndc=", input_ndc))
    
    # Create a unique identifier for the cache file
    cache_key <- digest::digest(list(c(query_product, query_product, query_drug), input_ndc), algo = "sha256", serialize = TRUE)
    cache_file <- fs::path(cache_dir,cache_key)
    
    # Check if cached data exists and is fresh
    if(fs::file_exists(cache_file)) {
      api_tibble <- readRDS(file = cache_file)
    } else {
      
      # Retrieve the JSON response from the API
      api_response_concept <- get_json(url = query_concept)
      api_response_product <- get_json(url = query_product)
      api_response_drug <- get_json(url = query_drug)
      
      # Convert JSON response into a tibble
      api_tibble <- dplyr::bind_rows(api_response_concept$ndcInfoList$ndcInfo, api_response_product$ndcInfoList$ndcInfo,
                                     api_response_drug$ndcInfoList$ndcInfo)
      
      # If there is an RxNorm result only
      if(nrow(api_tibble) >0){
        api_tibble <- dplyr::distinct(api_tibble, ndc11, status, rxcui, conceptName,
                                      conceptStatus, tty)
        api_tibble <- dplyr::mutate(api_tibble, input_ndc = input_ndc)
        api_tibble <- dplyr::arrange(api_tibble, input_ndc)
        api_tibble <- dplyr::select(api_tibble, input_ndc, ndc11, rxcui, status,conceptName, conceptStatus, tty )
        
        # Create cache directory if it doesn't exist
        dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
        
        # Save the result to the cache file
        saveRDS(api_tibble, cache_file)
      }
    }
    return(api_tibble)
  }, error = function(e) {
    message("Error in function get_related_ndcs(): ", e)
    stop(e)
  })
}

#' @title get_rxclass_from_drug_name
#' @description
#'Retrieves RxClass information from the name of a drug.
#' @export
get_rxclass_from_drug_name <- function(input_drug) {
  tryCatch({ 
    
    # Create empty tibble for consistent output
    api_tibble <- tibble(rxcui = NA_character_, name = NA_character_, tty = NA_character_,
                         class_id = NA_character_, class_name = NA_character_, class_type = NA_character_,
                         class_url = NA_character_, rela = NA_character_, rela_source = NA_character_,
                         input_drug = input_drug)
    
    # Handle empty or NA inputs
    if (is.null(input_drug) || all(is.na(input_drug))) {
      stop("Input rxcui cannot be missing.")
    }
    
    query_address <- URLencode(rxnav_rxclass_url("class/byDrugName.json?drugName=", input_drug,
                                                 "&relaSource=ALL&relas=ALL"))
    # Create a unique identifier for the cache file
    cache_key <- digest::digest(list(query_address, input_drug), algo = "sha256", serialize = TRUE)
    cache_file <- fs::path(cache_dir,cache_key)
    
    # Check if cached data exists and is fresh
    if(fs::file_exists(cache_file)) {
      api_tibble <- readRDS(file = cache_file)
    } else {
      # Retrieve the JSON response from the API
      api_response <- get_json(url = query_address)
      
      # Process API response and construct output
      api_tibble <- dplyr::bind_cols(api_response$rxclassDrugInfoList$rxclassDrugInfo$minConcept,
                                     api_response$rxclassDrugInfoList$rxclassDrugInfo$rxclassMinConceptItem, rela = api_response$rxclassDrugInfoList$rxclassDrugInfo$rela,
                                     relaSource = api_response$rxclassDrugInfoList$rxclassDrugInfo$relaSource)
      api_tibble <- dplyr::mutate(api_tibble, input_drug = input_drug)
      api_tibble <- dplyr::arrange(api_tibble, input_drug)
      
      # Save the result to the cache file
      saveRDS(api_tibble, cache_file)
    }
    return(api_tibble)
  }, error = function(e) {
    message("Error in function get_rxclass_from_drug_name(): ", e)
    stop(e)
  })
}

#' @title get_rxclass_from_rxcui
#' @description
#'Retrieves RxClass information for a specified RxNorm concept.
#' @export
get_rxclass_from_rxcui <- function(input_rxcui) {
  tryCatch({ 
    
    # Create empty tibble for consistent output
    api_tibble <- tibble(rxcui = NA_character_, name = NA_character_, tty = NA_character_,
                         class_id = NA_character_, class_name = NA_character_, class_type = NA_character_,
                         class_url = NA_character_, rela = NA_character_, rela_source = NA_character_,
                         input_rxcui = input_rxcui)
    
    # Handle empty or NA inputs
    if (is.null(input_rxcui) || all(is.na(input_rxcui))) {
      stop("Input rxcui cannot be missing.")
    }
    
    # Construct the API query URL
    query_address <- URLencode(rxnav_rxclass_url("class/byRxcui.json?rxcui=", input_rxcui,
                                                 "&relaSource=ALL"))
    
    # Create a unique identifier for the cache file
    cache_key <- digest::digest(list(query_address, input_rxcui), algo = "sha256", serialize = TRUE)
    cache_file <- fs::path(cache_dir,cache_key)
    
    # Check if cached data exists and is fresh
    if(fs::file_exists(cache_file)) {
      api_tibble <- readRDS(file = cache_file)
    } else {
      # Retrieve the JSON response from the API
      api_response <- get_json(url = query_address)
      
      # Process API response and construct output
      api_tibble <- dplyr::bind_cols(api_response$rxclassDrugInfoList$rxclassDrugInfo$minConcept,
                                     api_response$rxclassDrugInfoList$rxclassDrugInfo$rxclassMinConceptItem, rela = api_response$rxclassDrugInfoList$rxclassDrugInfo$rela,
                                     relaSource = api_response$rxclassDrugInfoList$rxclassDrugInfo$relaSource)
      api_tibble <- dplyr::mutate(api_tibble, input_rxcui = input_rxcui)
      api_tibble <- dplyr::arrange(api_tibble, input_rxcui)
      
      # Save the result to the cache file
      saveRDS(api_tibble, cache_file)
    }
    return(api_tibble)
  }, error = function(e) {
    message("Error in function get_rxclass_from_rxcui(): ", e)
    stop(e)
  })
}

#' @title get_va_from_rxcui
#' @description
#' Retrieves VA National Formulary from RxClass 
#' @export
get_va_from_rxcui <- function(input_rxcui) {
  tryCatch({ 
    
    # Create empty tibble for consistent output
    min_concept <- tibble::tibble(rxcui = NA_character_,name = NA_character_,tty = NA_character_)
    rxclass_min_concept_item <- tibble::tibble(class_id = NA_character_,class_name = NA_character_,class_type = NA_character_)
    api_tibble <- dplyr::bind_cols(min_concept,rxclass_min_concept_item) %>% dplyr::mutate(version = NA_character_)
    
    # Handle empty or NA inputs
    if (is.null(input_rxcui) || all(is.na(input_rxcui))) {
      stop("Input rxcui cannot be missing.")
    }
    # Main logic flow
    query_address <- URLencode(rxnav_rxclass_url("class/byRxcui.json?rxcui=", input_rxcui,"&relaSource=VA"))
    version_address <- URLencode(rxnav_rxclass_url("version/VA.json"))
    
    # Create a unique identifier for the cache file
    cache_key <- digest::digest(list(query_address, version_address, input_rxcui), algo = "sha256", serialize = TRUE)
    cache_file <- fs::path(cache_dir,cache_key)
    
    # Check if cached data exists and is fresh
    if(fs::file_exists(cache_file)) {
      api_tibble <- readRDS(file = cache_file)
    } else {  
      #Query Data
      api_response <- jsonlite::fromJSON(query_address, simplifyVector = TRUE)
      version_response <- jsonlite::fromJSON(version_address, simplifyVector = TRUE)
      
      if (pluck_exists(api_response, "rxclassDrugInfoList") & is.null(api_tibble)) {
        if (pluck_exists(api_response$rxclassDrugInfoList, "rxclassDrugInfo")) {
          
          if (pluck_exists(api_response$rxclassDrugInfoList$rxclassDrugInfo, "minConcept")) {
            min_concept <- purrr::pluck(api_response$rxclassDrugInfoList$rxclassDrugInfo, "minConcept")
            min_concept <- janitor::clean_names(min_concept)
          }
          if (pluck_exists(api_response$rxclassDrugInfoList$rxclassDrugInfo, "rxclassMinConceptItem")) {
            rxclass_min_concept_item <- purrr::pluck(api_response$rxclassDrugInfoList$rxclassDrugInfo, "rxclassMinConceptItem")
            rxclass_min_concept_item <- janitor::clean_names(rxclass_min_concept_item)
          }
          
          rxclass_drug_info <- dplyr::bind_cols(min_concept,rxclass_min_concept_item)
          api_tibble <- janitor::clean_names(rxclass_drug_info)
          api_tibble <- api_tibble %>% dplyr::mutate(version = version_response$relaSourceVersion)
          saveRDS(api_tibble, cache_file)
        }
      }
    }
    return(api_tibble)
  }, error = function(e) {
    message("Error in function get_va_from_rxcui(): ", e)
    stop(e)
  })
}


#' @title get_medrt_from_rxcui
#' @description
#' Retrieves MED-RT (Medication Reference Terminology) - Source Representation from RxClass 
#' @export
get_medrt_from_rxcui <- function(input_rxcui) {
  tryCatch({ 
    
    # Create empty tibble for consistent output
    min_concept <- tibble::tibble(rxcui = NA_character_,name = NA_character_,tty = NA_character_)
    rxclass_min_concept_item <- tibble::tibble(class_id = NA_character_,class_name = NA_character_,class_type = NA_character_)
    api_tibble <- dplyr::bind_cols(min_concept,rxclass_min_concept_item) %>% dplyr::mutate(version = NA_character_)
    
    # Handle empty or NA inputs
    if (is.null(input_rxcui) || all(is.na(input_rxcui))) {
      stop("Input rxcui cannot be missing.")
    }
    rxnav_rxclass_url()
    # Main logic flow
    query_address <- URLencode(rxnav_rxclass_url("class/byRxcui.json?rxcui=", input_rxcui,"&relaSource=MEDRT"))
    version_address <- URLencode(rxnav_rxclass_url("version/MEDRT.json"))

    # Create a unique identifier for the cache file
    cache_key <- digest::digest(list(query_address, version_address, input_rxcui), algo = "sha256", serialize = TRUE)
    cache_file <- fs::path(cache_dir,cache_key)
    
    # Check if cached data exists and is fresh
    if(fs::file_exists(cache_file)) {
      api_tibble <- readRDS(file = cache_file)
    } else {  
      #Query Data
      api_response <- jsonlite::fromJSON(query_address, simplifyVector = TRUE)
      version_response <- jsonlite::fromJSON(version_address, simplifyVector = TRUE)
      
      if (pluck_exists(api_response, "rxclassDrugInfoList") & is.null(api_tibble)) {
        if (pluck_exists(api_response$rxclassDrugInfoList, "rxclassDrugInfo")) {
          
          if (pluck_exists(api_response$rxclassDrugInfoList$rxclassDrugInfo, "minConcept")) {
            min_concept <- purrr::pluck(api_response$rxclassDrugInfoList$rxclassDrugInfo, "minConcept")
            min_concept <- janitor::clean_names(min_concept)
          }
          if (pluck_exists(api_response$rxclassDrugInfoList$rxclassDrugInfo, "rxclassMinConceptItem")) {
            rxclass_min_concept_item <- purrr::pluck(api_response$rxclassDrugInfoList$rxclassDrugInfo, "rxclassMinConceptItem")
            rxclass_min_concept_item <- janitor::clean_names(rxclass_min_concept_item)
          }
          
          rxclass_drug_info <- dplyr::bind_cols(min_concept,rxclass_min_concept_item)
          api_tibble <- janitor::clean_names(rxclass_drug_info)
          api_tibble <- api_tibble %>% dplyr::mutate(version = version_response$relaSourceVersion)
          saveRDS(api_tibble, cache_file)
        }
      }
    }
    return(api_tibble)
  }, error = function(e) {
    message("Error in function get_medrt_from_rxcui(): ", e)
    stop(e)
  })
}


#' @title get_atc_product_from_rxcui
#' @description
#' Retrieves Drug Schedule and RxNorm Name from RxClass 
#' @export
get_atc_product_from_rxcui <- function(input_rxcui) {
  tryCatch({ 
    # Create empty tibble for consistent output
    min_concept <-
      tibble::tibble(rxcui = NA_character_,
                     name = NA_character_,
                     tty = NA_character_)
    rxclass_min_concept_item <-
      tibble::tibble(class_id = NA_character_,
                     class_name = NA_character_,
                     class_type = NA_character_)
    api_tibble <-
      dplyr::bind_cols(min_concept, rxclass_min_concept_item) %>% dplyr::mutate(version = NA_character_)
    
    # Handle empty or NA inputs
    if (is.null(input_rxcui) || all(is.na(input_rxcui))) {
      stop("Input rxcui cannot be missing.")
    }
    rxnav_rxclass_url()
    # Main logic flow
    query_address <-
      URLencode(
        rxnav_rxclass_url(
          "class/byRxcui.json?rxcui=",
          input_rxcui,
          "&relaSource=ATCPROD"
        )
      )
    version_address <-
      URLencode(rxnav_rxclass_url("version/ATCPROD.json"))
    
    
    # Create a unique identifier for the cache file
    cache_key <- digest::digest(list(query_address, version_address, input_rxcui), algo = "sha256", serialize = TRUE)
    cache_file <- fs::path(cache_dir,cache_key)
    
    # Check if cached data exists and is fresh
    if(fs::file_exists(cache_file)) {
      api_tibble <- readRDS(file = cache_file)
    } else {
      #Query Data
      api_response <-
        jsonlite::fromJSON(query_address, simplifyVector = TRUE)
      version_response <-
        jsonlite::fromJSON(version_address, simplifyVector = TRUE)
      
      if (pluck_exists(api_response, "rxclassDrugInfoList") &
          is.null(api_tibble)) {
        if (pluck_exists(api_response$rxclassDrugInfoList, "rxclassDrugInfo")) {
          if (pluck_exists(api_response$rxclassDrugInfoList$rxclassDrugInfo,
                                  "minConcept")) {
            min_concept <-
              purrr::pluck(api_response$rxclassDrugInfoList$rxclassDrugInfo,
                           "minConcept")
            min_concept <- janitor::clean_names(min_concept)
          }
          if (pluck_exists(
            api_response$rxclassDrugInfoList$rxclassDrugInfo,
            "rxclassMinConceptItem"
          )) {
            rxclass_min_concept_item <-
              purrr::pluck(
                api_response$rxclassDrugInfoList$rxclassDrugInfo,
                "rxclassMinConceptItem"
              )
            rxclass_min_concept_item <-
              janitor::clean_names(rxclass_min_concept_item)
          }
          
          rxclass_drug_info <-
            dplyr::bind_cols(min_concept, rxclass_min_concept_item)
          api_tibble <- janitor::clean_names(rxclass_drug_info)
          api_tibble <-
            api_tibble %>% dplyr::mutate(version = version_response$relaSourceVersion)
          saveRDS(api_tibble, cache_file)
        }
      }
    }
    return(api_tibble)
  }, error = function(e) {
    message("Error in function get_atc_product_from_rxcui(): ", e)
    stop(e)
  })
}

#' @title get_drug_schedule_from_rxcui
#' @description
#' Retrieves Drug Schedule and RxNorm Name from RxClass 
#' @export
get_drug_schedule_from_rxcui <- function(input_rxcui) {
  tryCatch({ 
    
    # Create empty tibble for consistent output
    min_concept <- tibble::tibble(rxcui = NA_character_,name = NA_character_,tty = NA_character_)
    rxclass_min_concept_item <- tibble::tibble(class_id = NA_character_,class_name = NA_character_,class_type = NA_character_)
    api_tibble <- dplyr::bind_cols(min_concept,rxclass_min_concept_item) %>% dplyr::mutate(version = NA_character_)
    
    # Handle empty or NA inputs
    if (is.null(input_rxcui) || all(is.na(input_rxcui))) {
      stop("Input rxcui cannot be missing.")
    }
    
    # Main logic flow
    query_address <- URLencode(rxnav_rxclass_url("class/byRxcui.json?rxcui=", input_rxcui,"&relaSource=RXNORM&relas=has_schedule"))
    version_address <- URLencode(rxnav_rxclass_url("version/RXNORM.json"))
    
    # Create a unique identifier for the cache file
    cache_key <- digest::digest(list(query_address, version_address, input_rxcui), algo = "sha256", serialize = TRUE)
    cache_file <- fs::path(cache_dir,cache_key)
    
    # Check if cached data exists and is fresh
    if(fs::file_exists(cache_file)) {
      api_tibble <- readRDS(file = cache_file)
    } else {
      #Query Data
      api_response <- jsonlite::fromJSON(query_address, simplifyVector = TRUE)
      version_response <- jsonlite::fromJSON(version_address, simplifyVector = TRUE)
      
      if (pluck_exists(api_response, "rxclassDrugInfoList") & is.null(api_tibble)) {
        if (pluck_exists(api_response$rxclassDrugInfoList, "rxclassDrugInfo")) {
          
          if (pluck_exists(api_response$rxclassDrugInfoList$rxclassDrugInfo, "minConcept")) {
            min_concept <- purrr::pluck(api_response$rxclassDrugInfoList$rxclassDrugInfo, "minConcept")
            min_concept <- janitor::clean_names(min_concept)
          }
          if (pluck_exists(api_response$rxclassDrugInfoList$rxclassDrugInfo, "rxclassMinConceptItem")) {
            rxclass_min_concept_item <- purrr::pluck(api_response$rxclassDrugInfoList$rxclassDrugInfo, "rxclassMinConceptItem")
            rxclass_min_concept_item <- janitor::clean_names(rxclass_min_concept_item)
          }
          
          rxclass_drug_info <- dplyr::bind_cols(min_concept,rxclass_min_concept_item)
          api_tibble <- janitor::clean_names(rxclass_drug_info)
          api_tibble <- api_tibble %>% dplyr::mutate(version = version_response$relaSourceVersion)
          saveRDS(api_tibble, cache_file)
        }
      }
    }
    return(api_tibble)
  }, error = function(e) {
    message("Error in function get_drug_schedule_from_rxcui(): ", e)
    stop(e)
  })
}


#' @title get_rxcui_from_drug_approximate
#' @description
#' Retrieves RxNorm rxcui by approximate drug search term.
#' @export
get_rxcui_from_drug_approximate <- function(input_drug) {
  tryCatch({ 
    
    # Handle empty or NA inputs
    if (is.null(input_drug) || all(is.na(input_drug))) {
      stop("Input search cannot be empty or contain only NA values.")
    }
    
    query_address1 <- URLencode(rxnav_rxnorm_url("approximateTerm.json?term=", input_drug,"&maxEntries=100&option=1"))
    query_address2 <- URLencode(rxnav_rxnorm_url("approximateTerm.json?term=", input_drug,"&maxEntries=100&option=0"))
    
    # Create a unique identifier for the cache file
    cache_key <- digest::digest(list(query_address1, query_address2, input_drug), algo = "sha256", serialize = TRUE)
    cache_file <- fs::path(cache_dir,cache_key)
    
    # Check if cached data exists and is fresh
    if(fs::file_exists(cache_file)) {
      api_tibble <- readRDS(file = cache_file)
    } else {
      
      api_response1 <- get_json(url = query_address1)
      api_response2 <- get_json(url = query_address2)
      
      tibble_1 <- api_response1$approximateGroup$candidate
      tibble_2 <- api_response2$approximateGroup$candidate
      
      api_list <- list(tibble_1, tibble_2)
      api_tibble <- clean_purrr_list(api_list)
      api_tibble <- dplyr::mutate(api_tibble, input_drug = input_drug)
      api_tibble <- dplyr::distinct(api_tibble)
      
      # Save the result to the cache file
      saveRDS(api_tibble, cache_file)
    }
    return(api_tibble)
  }, error = function(e) {
    message("Error in function get_rxcui_from_drug_approximate(): ", e)
    stop(e)
  })
}

#' @title get_rxcui_from_drug
#' @description
#' Retrieves RxNorm xCUIs by a drug name.
#' @export
get_rxcui_from_drug <- function(input_drug) {
  tryCatch({ 
    
    # Handle empty or NA inputs
    if (is.null(input_drug) || all(is.na(input_drug))) {
      stop("Input drug cannot be missing.")
    }
    
    query_address <- URLencode(rxnav_rxnorm_url("drugs.json?name=", input_drug))
    
    # Create a unique identifier for the cache file
    cache_key <- digest::digest(list(query_address, input_drug), algo = "sha256", serialize = TRUE)
    cache_file <- fs::path(cache_dir,cache_key)
    
    # Check if cached data exists and is fresh
    if(fs::file_exists(cache_file)) {
      api_tibble <- readRDS(file = cache_file)
    } else {
      
      api_response <- get_json(url = query_address)
      api_tibble <- dplyr::bind_rows(api_response$approximateGroup$candidate)
      api_tibble <- dplyr::mutate(api_tibble, input_drug = input_drug)
      
      # Save the result to the cache file
      saveRDS(api_tibble, cache_file)
    }
    return(api_tibble)
  }, error = function(e) {
    message("Error in function get_rxcui_from_drug(): ", e)
    stop(e)
  })
}

#' @title Get All Properties from RxCUI 
#' @description Retrieves concept details via the RxNorm API getAllProperties endpoint (https://lhncbc.nlm.nih.gov/RxNav/APIs/api-RxNorm.getAllProperties.html)
#' @export
get_allproperties_rxcui <- function(input_rxcui) {
  tryCatch({ 
    
    # Validate input to ensure it is neither null nor NA
    if (is.null(input_rxcui) || anyNA(input_rxcui)) {
      stop("Input rxcui cannot be missing.")
    }
    
    # Main logic flow
    query_address <- URLencode(rxnav_rxnorm_url("rxcui/", input_rxcui, "/allProperties.json?prop=names+attributes"))
    
    # Create a unique identifier for the cache file
    cache_key <- digest::digest(list(query_address, input_rxcui), algo = "sha256", serialize = TRUE)
    cache_file <- fs::path(cache_dir,cache_key)
    
    # Check if cached data exists and is fresh
    if(fs::file_exists(cache_file)) {
      api_tibble <- readRDS(file = cache_file)
    } else {
      allproperties_df <- 
        tibble::tibble(
          input_rxcui = NA_character_,
          available_strength = NA_character_,
          general_cardinality = NA_character_,
          human_drug = NA_character_,
          prescribable = NA_character_,
          rxnav_human_drug = NA_character_,
          schedule = NA_character_,
          tty = NA_character_,
          rxnav_str = NA_character_,
          rx_norm_name = NA_character_,
          rx_norm_synonym = NA_character_
        )
      
      #Query Data
      api_response <-
        jsonlite::fromJSON(query_address, simplifyVector = TRUE)
      
      if (pluck_exists(api_response, "propConceptGroup")) {
        if (pluck_exists(api_response$propConceptGroup, "propConcept")) {
          api_tibble <- 
            purrr::pluck(api_response$propConceptGroup, "propConcept") %>% 
            dplyr::mutate(input_rxcui = input_rxcui,
                          propName = janitor::make_clean_names(propName)) %>%
            dplyr::select(input_rxcui,propName,propValue) %>%
            tidyr::pivot_wider(id_cols = input_rxcui,names_from = propName,values_from = propValue) %>%
            dplyr::bind_rows(allproperties_df) %>%
            janitor::remove_empty(which = "rows",cutoff = 1)
        }
        else {
          api_tibble <- allproperties_df
        }
      }
    }
    return(api_tibble)
  }, error = function(e) {
    message("Error in function get_allproperties_rxcui(): ", e)
    stop(e)
  })
}

#' @title Get RxCUI History Status
#' @description Retrieves status, history, and other attributes of a concept via the RxNorm API.
#' @export
get_rxcui_history_status <- function(input_rxcui) {
  tryCatch({ 
    
    # Define default values for each data structure
    meta_data <-
      tibble::tibble(
        status = NA_character_,
        source = NA_character_,
        release_start_date = NA_character_,
        release_end_date = NA_character_,
        is_current = NA_character_,
        active_start_date = NA_character_,
        active_end_date = NA_character_,
        remapped_date = NA_character_
      )
    
    attributes <-
      tibble::tibble(
        rxcui = NA_character_,
        name = NA_character_,
        tty = NA_character_,
        is_multiple_ingredient = NA_character_,
        is_branded = NA_character_
      )
    
    ingredient_and_strength <-
      tibble::tibble(
        base_rxcui = NA_character_,
        base_name = NA_character_,
        boss_rxcui = NA_character_,
        boss_name = NA_character_,
        active_ingredient_rxcui = NA_character_,
        active_ingredient_name = NA_character_,
        moiety_rxcui = NA_character_,
        moiety_name = NA_character_,
        numerator_value = NA_character_,
        numerator_unit = NA_character_,
        denominator_value = NA_character_,
        denominator_unit = NA_character_
      )
    
    dose_form_concept <-
      tibble::tibble(dose_form_rxcui = NA_character_,
                     dose_form_name = NA_character_)
    
    definitional_features <-
      tibble::tibble(
        base_rxcui = NA_character_,
        base_name = NA_character_,
        boss_rxcui = NA_character_,
        boss_name = NA_character_,
        active_ingredient_rxcui = NA_character_,
        active_ingredient_name = NA_character_,
        moiety_rxcui = NA_character_,
        moiety_name = NA_character_,
        numerator_value = NA_character_,
        numerator_unit = NA_character_,
        denominator_value = NA_character_,
        denominator_unit = NA_character_,
        dose_form_rxcui = NA_character_,
        dose_form_name = NA_character_
      )
    
    remapped_concept <-
      tibble::tibble(
        remapped_rxcui = NA_character_,
        remapped_name = NA_character_,
        remapped_tt7 = NA_character_,
        remapped_active = NA_character_
      )
    
    # Validate input to ensure it is neither null nor NA
    if (is.null(input_rxcui) || anyNA(input_rxcui)) {
      stop("Input rxcui cannot be missing.")
    }
    
    # Main logic flow
    query_address <- URLencode(rxnav_rxnorm_url("rxcui/", input_rxcui, "/historystatus.json"))
    
    # Create a unique identifier for the cache file
    cache_key <- digest::digest(list(query_address, input_rxcui), algo = "sha256", serialize = TRUE)
    cache_file <- fs::path(cache_dir,cache_key)
    
    # Check if cached data exists and is fresh
    if(fs::file_exists(cache_file)) {
      api_tibble <- readRDS(file = cache_file)
    } else {
      api_response <- jsonlite::fromJSON(query_address, simplifyVector = TRUE)
      
      if (pluck_exists(api_response, "rxcuiStatusHistory")){
        
        meta_data <-
          extract_and_clean(api_response$rxcuiStatusHistory, "metaData", meta_data)
        
        attributes <-
          extract_and_clean(api_response$rxcuiStatusHistory,
                            "attributes",
                            attributes)
        
        if (pluck_exists(api_response$rxcuiStatusHistory, "definitionalFeatures")) {
          
          ingredient_and_strength <-
            extract_and_clean(
              api_response$rxcuiStatusHistory$definitionalFeatures,
              "ingredientAndStrength",
              ingredient_and_strength
            )
          dose_form_concept <-
            extract_and_clean(
              api_response$rxcuiStatusHistory$definitionalFeatures,
              "doseFormConcept",
              dose_form_concept
            )
        }
        
        if (pluck_exists(api_response$rxcuiStatusHistory, "derivedConcepts")) {
          
          # Extract and clean data for derived concepts
          
          remapped_concept <-
            extract_and_clean(
              api_response$rxcuiStatusHistory$derivedConcepts,
              "remappedConcept",
              remapped_concept
            )
        }
      }
      
      #tidy data
      df_list <- tibble::tibble(
        meta_data = list(meta_data), 
        attributes = list(attributes),
        ingredient_and_strength  = list(ingredient_and_strength), 
        dose_form_concept = list(dose_form_concept), 
        remapped_concept = list(remapped_concept))
      
      tidy_df <- 
        df_list %>% 
        tidyr::unnest(c(meta_data)) %>% 
        tidyr::unnest(c(attributes)) %>% 
        tidyr::unnest(c(dose_form_concept))%>% 
        tidyr::unnest(c(ingredient_and_strength)) %>% 
        tidyr::unnest(c(remapped_concept))
      
      api_tibble <- tidy_df
      saveRDS(api_tibble, cache_file)
    }
    return(api_tibble)
  }, error = function(e) {
    message("Error in function get_rxcui_history_status(): ", e)
    stop(e)
  })
}


#' @title get_rxcui_from_ndc
#' @description
#' Retrieves RxNorm RxCUIs by NDC
#' @export
get_rxcui_from_ndc <- function(input_ndc) {
  tryCatch({ 
    # Validate input to ensure it is neither null nor NA
    if (is.null(input_ndc) || all(is.na(input_ndc))) {
      stop("Input ndc cannot be missing.")
    }
    
    # Save cache_key to cache_dir
    if (exists("cache_dir") == F) {
      stop("Error: You must specify a cache_dir global environment variable. Did you run cache_directory()?")
    }
    
    # Main logic flow
    query_address <- URLencode(rxnav_rxnorm_url("rxcui.json?idtype=NDC&id=", input_ndc, "&ndcstatus=all"))
    
    # Create a unique identifier for the cache file
    cache_key <- digest::digest(list(query_address, input_ndc), algo = "sha256", serialize = TRUE)
    cache_file <- fs::path(cache_dir,cache_key)
    
    # Check if cached data exists and is fresh
    if(fs::file_exists(cache_file)) {
      api_tibble <- readRDS(file = cache_file)
    } else {
      # Retrieve the json response from the API
      api_response <- get_json(url = query_address)
      if(length(api_response) > 0) {
      # Extract the RxCUI(s) from the json response
      api_response_rxcui <- purrr::pluck(api_response, "idGroup")
      api_response_rxcui <- dplyr::bind_rows(api_response_rxcui)
      api_response_rxcui <- janitor::clean_names(api_response_rxcui)
      
      # Handle cases where no RxCUI is found
      if (nrow(api_response_rxcui) > 0) {
        api_tibble <- api_response_rxcui %>% dplyr::mutate(input_ndc = input_ndc, rxcui = rxnorm_id) %>% dplyr::select(input_ndc, rxcui)
        # Save the result to the cache file
        saveRDS(api_tibble, cache_file)
      } else {
        api_tibble <- tibble(input_ndc = input_ndc, rxcui = NA_character_)
      }
    }
    return(api_tibble)
  }
    }, error = function(e) {
    message("Error in function get_rxcui_from_ndc(): ", e)
    stop(e)
  })
}

#' @title get_rxterm_from_rxcui
#' @description
#' Retrieves Retrieves RxTerms information for a specified RxNorm concept.
#' @export
get_rxterm_from_rxcui <- function(input_rxcui) {
  tryCatch({ 
    # Create an empty tibble to store API results (even if no results found)
    api_tibble <- tibble(brandName = NA_character_, displayName = NA_character_, synonym = NA_character_,
                         fullName = NA_character_, fullGenericName = NA_character_, strength = NA_character_,
                         rxtermsDoseForm = NA_character_, route = NA_character_, termType = NA_character_,
                         rxcui = NA_character_, genericRxcui = NA_integer_, rxnormDoseForm = NA_character_,
                         suppress = NA_character_, input_rxcui = NA_character_)
    
    # Handle empty or NA inputs
    if (is.null(input_rxcui) || all(is.na(input_rxcui))) {
      stop("Input rxcui cannot be missing.")
    }
    
    # Construct the API query URL
    query_address <- URLencode(rxnav_rxterms_url("rxcui/", input_rxcui, "/allinfo.json"))
    
    # Create a unique identifier for the cache file
    cache_key <- digest::digest(list(query_address, input_rxcui), algo = "sha256", serialize = TRUE)
    cache_file <- fs::path(cache_dir,cache_key)
    
    # Check if cached data exists and is fresh
    if(fs::file_exists(cache_file)) {
      api_tibble <- readRDS(file = cache_file)
    }  else {
      # Retrieve the JSON response from the API
      api_response <- get_json(url = query_address)
      
      # Convert JSON response into a tibble
      api_tibble <- dplyr::bind_rows(api_response$rxtermsProperties)
      api_tibble <- dplyr::mutate(api_tibble, input_rxcui = input_rxcui)
      api_tibble <- dplyr::arrange(api_tibble, input_rxcui)
      
      # Save the result to the cache file
      saveRDS(api_tibble, cache_file)
    }
    return(api_tibble)
  }, error = function(e) {
    message("Error in function get_rxterm_from_rxcui(): ", e)
    stop(e)
  })
}

#' @title get_schedule_from_rxcui
#' @description
#' Retrieves Drug Schedule from RxClass information for a specified RxNorm concept.
#' @export
get_schedule_from_rxcui <- function(input_rxcui) {
  tryCatch({ 
    
    # Create empty tibble for consistent output
    api_tibble <- tibble(rxcui = NA_character_, name = NA_character_, tty = NA_character_,
                         class_id = NA_character_, class_name = NA_character_, class_type = NA_character_,
                         class_url = NA_character_, rela = NA_character_, rela_source = NA_character_,
                         input_rxcui = input_rxcui)
    
    # Handle empty or NA inputs
    if (is.null(input_rxcui) || all(is.na(input_rxcui))) {
      stop("Input rxcui cannot be missing.")
    }
    
    # Construct the API query URL
    query_address <- URLencode(rxnav_rxclass_url("class/byRxcui.json?rxcui=", input_rxcui,"&relaSource=RXNORM&relas=has_schedule"))
    
    # Create a unique identifier for the cache file
    cache_key <- digest::digest(list(query_address, input_rxcui), algo = "sha256", serialize = TRUE)
    cache_file <- fs::path(cache_dir,cache_key)
    
    # Check if cached data exists and is fresh
    if(fs::file_exists(cache_file)) {
      api_tibble <- readRDS(file = cache_file)
    } else {
      # Retrieve the JSON response from the API
      api_response <- get_json(url = query_address)
      
      # Process API response and construct output
      api_tibble <- dplyr::bind_cols(api_response$rxclassDrugInfoList$rxclassDrugInfo$minConcept,
                                     api_response$rxclassDrugInfoList$rxclassDrugInfo$rxclassMinConceptItem, rela = api_response$rxclassDrugInfoList$rxclassDrugInfo$rela,
                                     relaSource = api_response$rxclassDrugInfoList$rxclassDrugInfo$relaSource)
      api_tibble <- dplyr::mutate(api_tibble, input_rxcui = input_rxcui)
      api_tibble <- dplyr::arrange(api_tibble, input_rxcui)
      
      # Save the result to the cache file
      saveRDS(api_tibble, cache_file)
    }
    return(api_tibble)
  }, error = function(e) {
    message("Error in function get_schedule_from_rxcui(): ", e)
    stop(e)
  })
}

#' @title get_spelling
#' @description
#' Retrieves  spelling suggestions for an input drug name.
#' @export
get_spelling <- function(input_drug) {
  tryCatch({ 
    
    suggestion <- NA_character_
    
    # Handle empty or NA inputs
    if (is.null(input_drug) || all(is.na(input_drug))) {
      stop("Input drug cannot be missing.")
    }
    
    query_address <- URLencode(rxnav_rxnorm_url("spellingsuggestions.json?name=", input_drug))
    
    # Create a unique identifier for the cache file
    cache_key <- digest::digest(list(query_address, input_ndc), algo = "sha256", serialize = TRUE)
    cache_file <- fs::path(cache_dir,cache_key)
    
    # Check if cached data exists and is fresh
    if(fs::file_exists(cache_file)) {
      api_tibble <- readRDS(file = cache_file)
    }  else {
      api_response <- get_json(url = query_address)
      spelling_suggestion <- as.character(api_response$suggestionGroup$suggestionList$suggestion)
      spelling_suggestion <- unique(spelling_suggestion)
      
      # Construct the output tibble
      api_tibble <- tibble::tibble(drug_term = input_drug, spelling_suggestion = ifelse(is.na(spelling_suggestion),
                                                                                        NA_character_, spelling_suggestion)) %>%
        dplyr::arrange(drug_term)
      # Save the result to the cache file
      saveRDS(api_tibble, cache_file)
    }
    return(api_tibble)
  }, error = function(e) {
    message("Error in function get_spelling(): ", e)
    stop(e)
  })
}