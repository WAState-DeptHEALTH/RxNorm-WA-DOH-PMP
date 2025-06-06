#' R helper functions

#' @export cache_directory       Create a cache directory to store results
cache_directory <- function(...){
  tryCatch({  # Construct path to a cache directory
    cache_folder <- glue::glue(...)
    fs::dir_create(path = cache_folder)
    return(cache_folder)
  }, error = function(e) {
    message("Error in function cache_directory(): ", e)
    stop(e)
  })
}

#' @export prepare_cache         
#' Prepares cache file paths and keys based on input query
prepare_cache <- function(query_address, input_string) {
  tryCatch({
    if(!exists("cache_dir")){
      stop("Error: You must specify a cache_dir global environment variable. Did you run cache_directory()?")
    }
    cache_key <- digest::digest(list(c(query_address), input_string), 
                                algo = "sha256", 
                                serialize = TRUE)
    cache_file <- fs::path(cache_dir, cache_key)
    return(
      list(cache_key = cache_key,
           cache_file = cache_file))
  }, error = function(e) {
    message("Error in function prepare_cache(): ", e)
    stop(e)
  })
}
#' @export check_and_load_cache         
#' Checks to see if cache file exists
check_and_load_cache <- function(cache_file){
  if(!exists("cache_dir")){
    stop("Error: You must specify a cache_dir global environment variable. Did you run cache_directory()?")
  }
  tryCatch({
    if(file.exists(fs::path_tidy(cache_file))){
      api_tibble <- readRDS(file = cache_file)
      return(api_tibble)
    } else {
      api_tibble <- NULL
      return(api_tibble)
    }
  }, error = function(e) {
    message("Error in function check_and_load_cache(): ", e)
    stop(e)
  })
}

#' @export rxnav_rxnorm_url      
#' Used to build RxNorm Endpoints url to RxNorm APIs
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

#' @export rxnav_prescribe_url   
# Used to build Prescrible RxNorm Endpoints url to Prescribable RxNorm APIs
rxnav_prescribe_url <- function(...) {
  tryCatch({
    
    # Construct the full query URL
    full_query_url <- glue::glue("https://rxnav.nlm.nih.gov/REST/Prescribe/", ...)
    return(full_query_url)
  }, error = function(e) {
    message("Error in function rxnav_prescribe_url(): ", e)
    stop(e)
  })
}

#' @export rxnav_rxterms_url     
#' Used to build RxTerm Endpointsurl to RxTerms API
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

#' @export rxnav_rxclass_url     
#' Used to build RxClass Endpoints url to RxClass API
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

#' @export get_json              
#' Used to retrive json data format from API endpoints
get_json <- ratelimitr::limit_rate(function(url, ...) {
  tryCatch({
    return(jsonlite::fromJSON(txt = url, ...))
  }, error = function(e) {
    message("Error in function get_json(): ", e)
    stop(e)
  })
},
ratelimitr::rate(n = 19, period = 1))

#' @export get_xml               
#' Used to retrive xml data format from API endpoints
get_xml <- ratelimitr::limit_rate(function(url, ...) {
  tryCatch({
    return(xml2::read_xml(url, ...))
  }, error = function(e) {
    message("Error in function get_xml(): ", e)
    stop(e)
  })
},
ratelimitr::rate(n = 19, period = 1))
#' @export get_brand_names       
#' Used to retrieve all brand names from RxNorm
get_brand_names <- function(){
  tryCatch({
    # Construct the full query URL
    return( rxnav_rxnorm_url("/allconcepts.json?tty=BN"))
  }, error = function(e) {
    message("Error in function get_brand_names(): ", e)
    stop(e)
  })
}

#' @export get_ingredient       
#'  Used to retrieve all ingredients from RxNorm
get_ingredient <- function(){
  tryCatch({
    # Construct the full query URL
    return( rxnav_rxnorm_url("allconcepts.json?tty=IN"))
  }, error = function(e) {
    message("Error in function get_ingredient(): ", e)
    stop(e)
  })
}

#' @export get_precise_ingredient 
#' Used to retrieve all precise ingredients from RxNorm
get_precise_ingredient <- function(){
  tryCatch({
    # Construct the full query URL
    return( rxnav_rxnorm_url("allconcepts.json?tty=PIN"))
  }, error = function(e) {
    message("Error in function get_precise_ingredient(): ", e)
    stop(e)
  })
}

#' @export get_multiple_ingredient 
#' Used to retrieve all multiple ingredients from RxNorm
get_multiple_ingredient <- function(){
  tryCatch({
    # Construct the full query URL
    return( rxnav_rxnorm_url("allconcepts.json?tty=MIN"))
  }, error = function(e) {
    message("Error in function get_multiple_ingredient(): ", e)
    stop(e)
  })
}

#' @export who_atc_url             
#' Used to build WHO ATC queries
who_atc_url <- function(...) {
  tryCatch({
    # Construct the full query URL
    full_query_url <- glue::glue("https://www.whocc.no/atc_ddd_index/", ...)
    return(full_query_url)
  }, error = function(e) {
    message("Error in function who_atc_url(): ", e)
    stop(e)
  })
}

#' @export who_atc_last_update     
#' Used to retrieve WHO ATC last update date

who_atc_last_update <- function() {
  tryCatch({
    # Build the URL for the WHO ATC webpage based on the input ATC code.
    query_address <- URLencode(who_atc_url())
    
    # Create a unique identifier for the cache file
    cache_key <- digest::digest(list(query_address, format(Sys.Date(), "%Y-%m-%d")))
    
    cache_file <- fs::path(cache_dir,cache_key)
    
    # Check if cached data exists and is fresh
    if(fs::file_exists(cache_file)) {
      api_tibble <- readRDS(file = cache_file)
      return(api_tibble)
    } else {
      # Scrape and store data
      atc_data  <- rvest::read_html(query_address)
      
      # Parse HTML Data
      last_updated_node <- rvest::html_nodes(atc_data, xpath = "//*[@id='last_updated']/p/i")
      last_updated_text <- rvest::html_text(last_updated_node,trim = TRUE)
      last_updated_text <- stringr::str_split(last_updated_text,"\n")
      last_updated_date <- as.Date(last_updated_text[[1]][2],"%Y-%m-%d")
      api_tibble <- tibble(last_updated = format(last_updated_date, "%Y-%m-%d"),
                           run_date = format(Sys.Date(), "%Y-%m-%d")
      )
      saveRDS(api_tibble,cache_file)
      return(api_tibble)
    }
  }, error = function(e) {
    message("Error in function who_atc_last_update(): ", e)
    stop(e)
  })
}

#' @export prepare_cache         
#' Prepares cache file paths and keys based on input query
prepare_cache <- function(query_address, input_string) {
  tryCatch({
    if(!exists("cache_dir")){
      stop("Error: You must specify a cache_dir global environment variable. Did you run cache_directory()?")
    }
    cache_key <- digest::digest(list(c(query_address), input_string), 
                                algo = "sha256", 
                                serialize = TRUE)
    cache_file <- fs::path(cache_dir, cache_key)
    return(
      list(cache_key = cache_key,
           cache_file = cache_file))
  }, error = function(e) {
    message("Error in function prepare_cache(): ", e)
    stop(e)
  })
}
#' @export check_and_load_cache         
#' Checks to see if cache file exists
check_and_load_cache <- function(cache_file){
  if(!exists("cache_dir")){
    stop("Error: You must specify a cache_dir global environment variable. Did you run cache_directory()?")
  }
  tryCatch({
    if(file.exists(fs::path_tidy(cache_file))){
      api_tibble <- readRDS(file = cache_file)
    return(api_tibble)
    } else {
      api_tibble <- NULL
      return(api_tibble)
    }
}, error = function(e) {
  message("Error in function check_and_load_cache(): ", e)
  stop(e)
})
}
#' @title validate_ndc
#' @description
#' Validate whether NDC is an 11 digit numbers
#' @export
validate_ndc <- function(ndc_code) {
  # Check if 11 digits and numeric
  return(stringr::str_detect(ndc_code, "^[[:digit:]]{11}$"))
}

#' @title clean_purrr_list
#' @description
#' filters out any elements in the list that are empty data frames (i.e., have zero rows).
#' @export
clean_purrr_list <- function(data) {
  data %>%
    purrr::compact() %>%
    dplyr::bind_rows() %>%
    dplyr::distinct()
}

#' @title get_json
# Retrieve json request with limit
#' @export
get_json <- ratelimitr::limit_rate(function(url, ...) {
  return(jsonlite::fromJSON(txt = url, ...))
}, ratelimitr::rate(n = 19, period = 1))

#' @title get_xml
# Retrieve xml request with limit
#' @export
get_xml <- ratelimitr::limit_rate(function(url, ...) {
  return(xml2::read_xml(url, ...))
}, ratelimitr::rate(n = 19, period = 1))

#' @title pluck_exists
# Use base R to do the equivalent of purrr::pluck_exists()
#' @export
pluck_exists <- function(lst, ...) {
  path <- list(...)
  tryCatch({
    # Traverse through the list to find the nested element
    for (p in path) {
      lst <- lst[[p]]
    }
    # If we successfully find a non-empty list, the element exists
    if(is.null(lst)){
      return(FALSE)
    } else {
      return(TRUE)
    }
  }, error = function(e) {
    # If an error occurs, the element does not exist
    return(FALSE)
  })
}

#' @title unnest_and_clean
#' @description
#' Helper function to unnest and clean data
#' @export
unnest_and_clean <- function(.data,col) {
  tryCatch({ 
    return(tidyr::unnest(.data , {{ col }}, names_sep = ".",keep_empty  = TRUE) %>% 
             janitor::clean_names() %>% 
             dplyr::mutate(dplyr::across(where(is.character), ~na_if(., ""))) %>% 
             dplyr::mutate(dplyr::across(where(is.numeric), ~na_if(., 0)))) 
  }, error = function(e) {
    message("Error in function unnest_and_clean(): ", e)
    stop(e)
  })
}

#' @title remove_commas
#' @description
#' Custom function to remove commas from a string
#' @export
remove_commas <- function(x) {
  tryCatch({  
    y <- gsub(",", "", x)
    return(y)
  }, error = function(e) {
    message("Error in function remove_commas(): ", e)
    stop(e)
  })
}

#' @title na_to_blank
#' @description
#' Converts NA to blank (primarily for file writing)
#' @export
na_to_blank <- function(x) {
  ifelse(is.na(as.character(x)), "", x)
}

#' @title blank_to_na
#' @description
#' Converts blank to NA
#' @export
blank_to_na <- function(x) {
  ifelse(x == "", NA, x)
}

#' @title remove_special_characters
#' @description
#' Remove All Special Characters
#' @export
remove_special_characters <- function(varname) {
  stringr::str_remove_all(varname, "[[:punct:]]")
}

#' @title remove_specific_character
#' @description
#' Remove a Specific Character
#' @export
remove_specific_character <- function(varname, character) {
  stringr::str_replace_all(varname, fixed(character), "")
}

#' @title remove_punctuation
#' @description
#' Remove All Punctuation
#' @export
remove_punctuation <- function(varname) {
  stringr::str_remove_all(varname, "[[:punct:]]")
}

#' @title remove_digits
#' @description
#' Remove All Digits
#' @export
remove_digits <- function(varname) {
  stringr::str_remove_all(varname, "[[:digit:]]")
}

#' @title remove_spaces
#' @description
#' Remove All Spaces
#' @export
remove_spaces <- function(varname) {
  stringr::str_replace_all(varname, fixed(" "), "")
}

#' @title remove_leading_trailing_spaces
#' @description
#' Remove Leading and Trailing Spaces
#' @export
remove_leading_trailing_spaces <- function(varname) {
  stringr::str_trim(varname)
}

#' @title compress_spaces
#' @description
#' Remove Extra Spaces with a Single Space
#' @export
compress_spaces <- function(varname) {
  stringr::str_squish(varname)
}
#'
#' @title extract_substring
#' @description
#' Extracting with substring
#' @export
extract_substring <- function(varname, start, count) {
  stringr::str_sub(varname, start, start + count - 1)
}
#'
#' @title replace_string
#' @description
#' Search for and Replace a String
#' @export
replace_string <- function(varname, to_replace, replacement) {
  stringr::str_replace_all(varname, fixed(to_replace), replacement)
}

#' @title make_uppercase
#' @description
#'  Make All Characters in a String Uppercase
#' @export
make_uppercase <- function(varname) {
  stringr::str_to_upper(varname)
}
#'
#' @title make_lowercase
#' @description
#'  Make All Characters in a String Lowercase
#' @export
make_lowercase <- function(varname) {
  stringr::str_to_lower(varname)
}

#' @title make_proper_case
#' @description
#'  Make All Characters in a String Proper Case
#' @export
make_proper_case <- function(varname) {
  tools::toTitleCase(varname)
}

#' @title concatenate_strings
#' @description
#' Concatenate Two Strings
#' @export
concatenate_strings <- function(var1, var2) {
  paste0(var1, var2)
}

#' @title parse_string
#' @description
#' Parse Out Pieces in a String
#' @export
parse_string <- function(varname, piece_number, delimiter = " ") {
  stringr::str_split(varname, delimiter, simplify = TRUE)[, piece_number]
}

#' @title phonics_string
#' @description
#' Convert string to specified phonetics
#' @export
phonics_string <- function(varname, method = "soundex") {
  phonics::phonics(varname,method = method)
}

#' @title calculate_editing_distance
#' @description
#' Calculate stringdistance based on specified method
#' @export
calculate_editing_distance <-
  function(name1, name2, method = "osa") {
    stringdist::stringdist(name1, name2, method = method)
  }

#' @title clean_string
#' @description
#' Function to upcase and remove trailing and leading spaces
#' @export
clean_string <- function(var) {
  # Remove Leading and Trailing Spaces
  remove_leading_trailing_spaces(# Make All Characters in a String Uppercase
    make_uppercase(# Remove Extra Spaces with a Single Space
      compress_spaces(var)))
}

#' @title extract_numbers_with_precision
#' @description
#' extract numbers with decimal precision
#' @export
extract_numbers_with_precision <- function(text) {
  pattern <- "[+-]?(\\d+(\\.\\d+)?)"
  matches <- stringr::str_extract_all(text, pattern)
  matches <- unlist(matches)
  matches <- as.numeric(matches)
  if (length(matches) == 0) {
    return(NA_real_)  # Return "NA" if no matches
  } else {
    return(matches)
  }
}

#' @title calculate_similarity_scores
#' @description
#' Perform fuzzy string matching between two strings
#' @export
calculate_similarity_scores <- function(str1, str2) {
  require(stringdist)  # Install 'stringdist' if necessary
  
  # Handle empty or NA values
  is_valid <- !(str1 == "" | str2 == "" | is.na(str1) | is.na(str2))
  
  # Exact comparison (with vectorization)
  exact_match <- str1 == str2
  
  # Fuzzy comparison (with vectorization and simplification)
  jw_sim_score <-
    stringdist::stringsim(str1[exact_match == FALSE], str2[exact_match ==
                                                             FALSE], method = "jw")
  
  # Combine scores
  score <- ifelse(exact_match, 1, jw_sim_score)
  
  # Preserve original validity
  score[is_valid == FALSE] <- NA
  
  return(score)
}

#' @title string_contains
#' @description
#' Perform fuzzy string matching between two strings, determining if one is likely contained within the other.
#' @export
string_contains <- function(str1, str2) {
  # Handle empty or NA values
  is_valid <- !(str1 == "" | str2 == "" | is.na(str1) | is.na(str2))
  
  score <- is_valid  # Initialize with valid matches
  
  # Exact comparison
  score[str1 == str2] <- 1
  
  # Potential fuzzy match candidates
  fuzzy_match_idx <- is_valid & (str1 != str2)
  
  # Determine the longer and shorter names (vectorized)
  longname <- ifelse(nchar(str1[fuzzy_match_idx]) >= nchar(str2[fuzzy_match_idx]),
                     str1[fuzzy_match_idx], str2[fuzzy_match_idx])
  shortname <- ifelse(nchar(str1[fuzzy_match_idx]) < nchar(str2[fuzzy_match_idx]),
                      str1[fuzzy_match_idx], str2[fuzzy_match_idx])
  
  # Check for containment using grepl (vectorized)
  score[fuzzy_match_idx] <- grepl(shortname, longname)
  
  return(score)
}