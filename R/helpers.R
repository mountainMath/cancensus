# Internal functions that do useful things frequently required in other functions

cancensus_base_url <- function(){
  url <- getOption("cancensus.base_url")
  if (is.null(url)) url <- "https://censusmapper.ca"
  if (!is.character(url) || length(url) != 1 || is.na(url) || !nzchar(url)) {
    stop("`cancensus.base_url` must be a non-empty string.", call. = FALSE)
  }
  sub("/+$", "", url)
}

cm_url <- function(...) {
  parts <- unlist(list(...), use.names = FALSE)
  parts <- parts[!is.na(parts) & nzchar(parts)]
  parts <- gsub("^/+|/+$", "", parts)
  paste(c(cancensus_base_url(), parts), collapse = "/")
}

cm_api_url <- function(...) {
  cm_url("api/v1", ...)
}

valid_api_key <- function(api_key){
  !is.null(api_key) && is.character(api_key) && substr(api_key,1,13)=="CensusMapper_"
}

robust_api_key <- function(api_key){
  api_key <- if (!valid_api_key(api_key) && nchar(Sys.getenv("CM_API_KEY")) > 1) { Sys.getenv("CM_API_KEY") } else { api_key }
  api_key <- if (!valid_api_key(api_key) && !is.null(getOption("cancensus.api_key"))) { getOption("cancensus.api_key") } else { api_key }
  api_key
}

# Append arguments to the path of the local cache directory.
cache_path <- function(...) {
  cache_dir <- Sys.getenv("CM_CACHE_PATH")
  if (nchar(cache_dir)==0) {
    if (!is.null(getOption("cancensus.cache_path"))) {
      cache_dir <- getOption("cancensus.cache_path")
    } else {
      cache_dir <- tempdir()
      message(cm_no_cache_path_message)
    }
  }
  if (!is.character(cache_dir)) {
    stop("Corrupt 'CM_CACHE_PATH' environment variable or 'cancensus.cache_path' option. Must be a path.",
         .call = FALSE)
  }
  if (!file.exists(cache_dir)) {
    dir.create(cache_dir, showWarnings = FALSE)
  }
  cache_key <- paste0(...)
  if (!identical(cache_key, character(0)))
    cache_dir <- file.path(cache_dir, paste0(...))
  cache_dir
}


translate_dataset <- function(dataset) {
  dataset <- as.character(dataset)
  translations <- c("1996"="CA1996",
                    "2001"="CA01",
                    "2006"="CA06",
                    "2011"="CA11",
                    "2016"="CA16",
                    "2021"="CA21")
  #dataset <- toupper(dataset)
  if (dataset %in% names(translations)) dataset=as.character(translations[dataset])
  dataset
}

clean_vector_list <- function(vector_list,dataset=NULL){
  if (!inherits(vector_list,"data.frame")) {
    if (inherits(vector_list,"character")) {
      if (is.null(dataset))  dataset <- dataset_from_vector_list(vector_list)
      vector_list = list_census_vectors(dataset) %>%
        dplyr::filter(vector %in% vector_list)
    } else
      stop(paste0("Don't know how to parse vector list: ",vector_list))
  }
  vector_list
}

dataset_from_vector_list <- function(vector_list){
  dataset <- attr(vector_list,'dataset')
  if (is.null(dataset)) {
    vectors = ifelse(inherits(vector_list,"character"),vector_list,vector_list$vector)
    dataset <- vectors %>%
      as.character() %>%
      lapply(function(d)unlist(strsplit(d,"_"))[2]) %>%
      unlist() %>%
      unique()
    if (length(dataset)!=1) stop("Unable to determine dataset")
  }
  dataset
}

cancensus_na_strings <- c("x", "X", "F", "...", "..", "-","N","*","**")

#' Retry an API call with exponential backoff
#'
#' @param call_fn A function that performs the API call and returns an httr response
#' @param max_attempts Maximum number of total attempts (default: 3)
#' @param quiet If TRUE, suppress retry messages
#' @param sleep_fn Function used to wait between attempts
#' @return The httr response object
#' @noRd
retry_api_call <- function(call_fn, max_attempts = 3, quiet = FALSE, sleep_fn = Sys.sleep) {
 max_attempts <- normalise_whole_number(max_attempts, "max_attempts", minimum = 1)
 attempt <- 1
 last_error <- NULL

 while (attempt <= max_attempts) {
   tryCatch({
     response <- call_fn()

     # Check for transient HTTP errors (5xx, timeout, connection errors)
     status <- httr::status_code(response)
     if (status >= 500 && status < 600 && attempt < max_attempts) {
       # Server error - retry
       wait_time <- 2 ^ (attempt - 1)  # Exponential backoff: 1, 2, 4 seconds
       if (!quiet) {
         message(sprintf("Server error (HTTP %d), retrying in %ds (attempt %d/%d)...",
                         status, wait_time, attempt + 1, max_attempts))
       }
       sleep_fn(wait_time)
       attempt <- attempt + 1
       next
     }

     # Success or non-retryable error
     return(response)

   }, error = function(e) {
     last_error <<- e
     # Network errors - retry
     if (attempt < max_attempts) {
       wait_time <- 2 ^ (attempt - 1)
       if (!quiet) {
         message(sprintf("Network error: %s. Retrying in %ds (attempt %d/%d)...",
                         conditionMessage(e), wait_time, attempt + 1, max_attempts))
       }
       sleep_fn(wait_time)
       attempt <<- attempt + 1
     } else {
       stop(e)
     }
   })
 }

 # If we've exhausted retries, throw the last error
 if (!is.null(last_error)) {
   stop(last_error)
 }
}

normalise_whole_number <- function(x, name, minimum = 0) {
  if (!is.numeric(x) || length(x) != 1 || is.na(x) || !is.finite(x) ||
      x < minimum || x != floor(x)) {
    stop(sprintf("`%s` must be a whole number greater than or equal to %s.", name, minimum),
         call. = FALSE)
  }
  as.integer(x)
}

perform_api_call <- function(call_fn, retry = 0, quiet = FALSE) {
  retry <- normalise_whole_number(retry, "retry")
  if (retry > 0) {
    retry_api_call(call_fn, max_attempts = retry + 1, quiet = quiet)
  } else {
    call_fn()
  }
}

cache_is_stale <- function(last_updated, max_age_days = 1) {
  if (is.null(last_updated) || length(last_updated) != 1 || is.na(last_updated)) {
    return(TRUE)
  }
  difftime(Sys.time(), last_updated, units = "days") > max_age_days
}

#' Format bytes to human-readable size
#' @noRd
format_bytes <- function(bytes) {
 if (bytes < 1024) return(paste0(bytes, " B"))
 if (bytes < 1024^2) return(sprintf("%.1f KB", bytes / 1024))
 if (bytes < 1024^3) return(sprintf("%.1f MB", bytes / 1024^2))
 return(sprintf("%.1f GB", bytes / 1024^3))
}

as.num = function(x, na.strings = cancensus_na_strings) {
  stopifnot(is.character(x))
  na = x %in% na.strings
  x[na] = 0
  x = as.numeric(x)
  x[na] = NA_real_
  x
}

as.int = function(x, na.strings = cancensus_na_strings) {
  stopifnot(is.character(x))
  na = x %in% na.strings
  x[na] = 0
  x = as.integer(x)
  x[na] = NA_integer_
  x
}

check_recalled_data_and_warn <- function(meta_file,params){
  cached_data<-generate_metadata(meta_file,params)
  recalled_data <- list_recalled_cached_data(cached_data,warn_only_once=TRUE)
  if (!is.null(recalled_data) && nrow(recalled_data)>0) {
    warning("Currently loaded data has been recalled. Use\nlist_recalled_cached_data()\nto inspect recalled locally cached data and\nremove_recalled_cached_data()\nto remove recalled data.")
  }
  d<-NULL
}

check_for_recalled_data_and_warn <- function(){
  recalled_data <- list_recalled_cached_data(warn_only_once=TRUE)
  if (!is.null(recalled_data) && nrow(recalled_data)>0) {
    warning(paste0("Some locally cached data has been recalled. Use\nlist_recalled_cached_data()\nto inspect recalled locally cached data and\nremove_recalled_cached_data()\nto remove recalled data."))
  }
  d<-NULL
}

first_run_checks <- function(){
  # Check caches for recalled data
  path=file.path(tempdir(),"cancensus_first_run_checks.info")
  if (!file.exists(path)) {
    # first time cancensus is run!
    check_for_recalled_data_and_warn()
    readr::write_lines("recall_check\n",path)
  }
  d<-NULL
}

#' A dataset with code table summaries for census data
#' @name CODES_TABLE
#' @docType data
#' @author derived from StatCan definitions
#' @references \url{https://www12.statcan.gc.ca/census-recensement/2021/geo/ref/domain-domaine/index2021-eng.cfm?lang=e&id=CSDtype}, \url{https://www12.statcan.gc.ca/census-recensement/2021/geo/ref/domain-domaine/index2021-eng.cfm?lang=e&id=CDtype}
#' @keywords data
NULL

#' A dataset City of Vancouver skytrain station locations
#' @name COV_SKYTRAIN_STATIONS
#' @docType data
#' @author City of Vancouver Open Data
#' @references \url{https://opendata.vancouver.ca/explore/dataset/rapid-transit-stations/information/}
#' @keywords data
NULL
