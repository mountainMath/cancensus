# Internal functions that do useful things frequently required in other functions

# In-memory session cache for metadata (vector and region lists). Sits in
# front of the tempdir() file cache so repeated calls within a session don't
# pay the disk read + deserialization cost on every access.
.cancensus_session_cache <- new.env(parent = emptyenv())

session_cache_get <- function(key){
  if (exists(key, envir = .cancensus_session_cache, inherits = FALSE)) {
    get(key, envir = .cancensus_session_cache, inherits = FALSE)
  } else {
    NULL
  }
}

session_cache_set <- function(key, value){
  assign(key, value, envir = .cancensus_session_cache)
  value
}

cancensus_base_url <- function(){
  url <- getOption("cancensus.base_url")
  if (is.null(url)) url <- "https://censusmapper.ca"
  url
}

#' User agent sent with CensusMapper API requests
#'
#' @description
#' Identifies the package and its version to the CensusMapper API, in the form
#' `cancensus/<version> httr/<version> R/<version>`. Packages or tools building on
#' **cancensus** can identify themselves by setting the `cancensus.user_agent_prefix`
#' option, e.g. `options(cancensus.user_agent_prefix = "mypackage/1.2.3")`, which gets
#' prepended to the user agent string.
#'
#' @return A character string
#' @keywords internal
cancensus_user_agent <- function(){
  ua <- paste0("cancensus/", utils::packageVersion("cancensus"),
               " httr/", utils::packageVersion("httr"),
               " R/", getRversion())
  prefix <- getOption("cancensus.user_agent_prefix")
  if (is.character(prefix) && length(prefix) == 1 && !is.na(prefix)) {
    # Header values must be a single line without control characters
    prefix <- trimws(gsub("[[:cntrl:]]+", " ", prefix))
    if (nchar(prefix) > 0) ua <- paste(prefix, ua)
  }
  ua
}

# Request configuration shared by all CensusMapper API requests
cm_request_config <- function(quiet = TRUE){
  config <- httr::user_agent(cancensus_user_agent())
  if (!quiet) config <- c(config, httr::progress())
  config
}

# All requests to the CensusMapper API should go through these wrappers so that
# they carry the package user agent. Shows a progress bar unless `quiet`.
cm_GET <- function(url, ..., quiet = TRUE){
  httr::GET(url, cm_request_config(quiet), ...)
}

cm_POST <- function(url, body = NULL, ..., quiet = TRUE){
  httr::POST(url, cm_request_config(quiet), body = body, ...)
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
    }
  }
  if (!is.character(cache_dir)) {
    cc_abort("cache_path_invalid")
  }
  if (!file.exists(cache_dir)) {
    dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
    if (!dir.exists(cache_dir)) {
      cc_abort("cache_dir_create_failed", path = cache_dir)
    }
  }
  cache_key <- paste0(...)
  if (!identical(cache_key, character(0)))
    cache_dir <- file.path(cache_dir, paste0(...))
  cache_dir
}

# Whether downloaded data outlives the R session. `base_path` is an explicit cache location
# overriding the cancensus cache path, for functions with a `cache_path` argument.
cache_is_persistent <- function(base_path = NULL){
  !is.null(base_path) ||
    nchar(Sys.getenv("CM_CACHE_PATH")) > 0 ||
    !is.null(getOption("cancensus.cache_path"))
}

# Warn that downloaded data won't be kept across sessions. Called after data has been downloaded
# and saved, so failed requests don't use up the warning and handlers that turn it into an error
# don't lose the download. Warns once per session, per call would flood users querying in a loop.
warn_if_cache_not_persistent <- function(base_path = NULL, quiet = FALSE, language = NULL){
  if (quiet || cache_is_persistent(base_path) ||
      isTRUE(session_cache_get("cache_warning_shown"))) {
    return(invisible(NULL))
  }
  session_cache_set("cache_warning_shown", TRUE)
  cc_warn("cache_not_persistent", .language = language)
  invisible(NULL)
}


# Escape regex metacharacters so user input can be matched literally
regex_escape <- function(x) {
  gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x)
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

# Validate a language parameter, valid choices are "en" and "fr"
validate_language <- function(language){
  valid_languages <- c("en","fr")
  language <- if (is.character(language) && length(language)==1) tolower(language) else NA_character_
  if (!(language %in% valid_languages)) {
    cc_abort("invalid_language", choices = valid_languages)
  }
  language
}

# Language of a list of vectors: the explicitly requested language if there is one, otherwise
# the language the vector list was retrieved in, otherwise the session default.
vector_list_language <- function(vector_list, language = NULL){
  if (is.null(language)) language <- attr(vector_list, "language")
  if (is.null(language)) language <- getOption("cancensus.language", "en")
  validate_language(language)
}

is_blank <- function(x){
  is.na(x) | !nzchar(trimws(x))
}

# Warn once per session and dataset, per call would flood users querying in a loop
warn_language_fallback <- function(dataset, n_missing, n_total, language = NULL){
  cache_key <- paste0("language_fallback_warning_", dataset)
  if (isTRUE(session_cache_get(cache_key))) return(invisible(NULL))
  session_cache_set(cache_key, TRUE)
  if (n_missing == n_total) {
    cc_warn("french_labels_unavailable_dataset", dataset = dataset, .language = language)
  } else {
    cc_warn("french_labels_unavailable_vectors", n = format_number(n_missing, language),
            total = format_number(n_total, language), dataset = dataset, .language = language)
  }
  invisible(NULL)
}

clean_vector_list <- function(vector_list,dataset=NULL,language=getOption("cancensus.language", "en")){
  if (!inherits(vector_list,"data.frame")) {
    if (inherits(vector_list,"character")) {
      if (is.null(dataset))  dataset <- dataset_from_vector_list(vector_list)
      vector_list = list_census_vectors(dataset, language = language) %>%
        dplyr::filter(vector %in% vector_list)
    } else
      cc_abort("vector_list_unparseable", vector_list = vector_list, .language = language)
  }
  vector_list
}

dataset_from_vector_list <- function(vector_list){
  dataset <- attr(vector_list,'dataset')
  if (is.null(dataset)) {
    vectors = if (inherits(vector_list,"character")) vector_list else vector_list$vector
    dataset <- unique(sub("^[^_]+_([^_]+).*$", "\\1", as.character(vectors)))
    if (length(dataset)!=1) cc_abort("dataset_undetermined")
  }
  dataset
}

cancensus_na_strings <- c("x", "X", "F", "...", "..", "-","N","*","**")

#' Retry an API call with exponential backoff
#'
#' @param call_fn A function that performs the API call and returns an httr response
#' @param max_retries Maximum number of retry attempts (default: 3)
#' @param quiet If TRUE, suppress retry messages
#' @param language Language of the retry messages, see \code{message_language}
#' @return The httr response object
#' @keywords internal
retry_api_call <- function(call_fn, max_retries = 3, quiet = FALSE, language = NULL) {
 attempt <- 1
 last_error <- NULL

 while (attempt <= max_retries) {
   tryCatch({
     response <- call_fn()

     # Check for transient HTTP errors (5xx, rate limiting, request timeout)
     status <- httr::status_code(response)
     if (((status >= 500 && status < 600) || status %in% c(408, 429)) && attempt < max_retries) {
       # Transient error - retry, honoring a Retry-After header if present
       wait_time <- 2 ^ (attempt - 1)  # Exponential backoff: 1, 2, 4 seconds
       retry_after <- suppressWarnings(as.numeric(httr::headers(response)$`retry-after`))
       if (length(retry_after) == 1 && !is.na(retry_after) && retry_after > 0) {
         wait_time <- max(wait_time, min(ceiling(retry_after), 60))
       }
       if (!quiet) {
         cc_inform("retry_http", status = status, wait = wait_time, attempt = attempt + 1,
                   max = max_retries, .language = language)
       }
       Sys.sleep(wait_time)
       attempt <- attempt + 1
       next
     }

     # Success or non-retryable error
     return(response)

   }, error = function(e) {
     last_error <<- e
     # Network errors - retry
     if (attempt < max_retries) {
       wait_time <- 2 ^ (attempt - 1)
       if (!quiet) {
         cc_inform("retry_network", error = conditionMessage(e), wait = wait_time,
                   attempt = attempt + 1, max = max_retries, .language = language)
       }
       Sys.sleep(wait_time)
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

#' Format bytes to human-readable size
#' @param bytes Number of bytes
#' @param language Language for units and decimal mark, see \code{message_language}
#' @keywords internal
format_bytes <- function(bytes, language = NULL) {
 language <- message_language(language)
 units <- if (language == "fr") c("o", "Ko", "Mo", "Go") else c("B", "KB", "MB", "GB")
 if (bytes < 1024) return(paste0(bytes, " ", units[1]))
 exponent <- if (bytes < 1024^2) 1 else if (bytes < 1024^3) 2 else 3
 size <- sprintf("%.1f", bytes / 1024^exponent)
 if (language == "fr") size <- sub(".", ",", size, fixed = TRUE)
 paste0(size, " ", units[exponent + 1])
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

check_recalled_data_and_warn <- function(meta_file,params,language=NULL){
  cached_data<-generate_metadata(meta_file,params)
  recalled_data <- list_recalled_cached_data(cached_data,warn_only_once=TRUE)
  if (!is.null(recalled_data) && nrow(recalled_data)>0) {
    cc_warn("recalled_loaded_data", .language = language)
  }
  d<-NULL
}

check_for_recalled_data_and_warn <- function(){
  recalled_data <- list_recalled_cached_data(warn_only_once=TRUE)
  if (!is.null(recalled_data) && nrow(recalled_data)>0) {
    cc_warn("recalled_cached_data")
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


