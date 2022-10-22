# Internal functions that do useful things frequently required in other functions

cancensus_base_url <- function(){
  url <- getOption("cancensus.base_url")
  if (is.null(url)) url <- "https://censusmapper.ca"
  url
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
    } else cache_dir <- tempdir()
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
    warning("Currently loaded data has been recalled. Use\nlist_recalled_cached_data()\nto inspect recalled locally cached data and\nnremove_recalled_cached_data()\nto remove recalled data.")
  }
  d<-NULL
}

check_for_recalled_data_and_warn <- function(){
  recalled_data <- list_recalled_cached_data(warn_only_once=TRUE)
  if (!is.null(recalled_data) && nrow(recalled_data)>0) {
    warning(paste0("Some locally cached data has been recalled. Use\nlist_recalled_cached_data()\nto inspect recalled locally cached data and\nnremove_recalled_cached_data()\nto remove recalled data."))
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
#' @references \url{https://www12.statcan.gc.ca/census-recensement/2016/ref/dict/geo012-eng.cfm}
#' @keywords data
NULL

#' A dataset City of Vancouver skytrain station locations
#' @name COV_SKYTRAIN_STATIONS
#' @docType data
#' @author City of Vancouver Open Data
#' @references \url{https://opendata.vancouver.ca/explore/dataset/rapid-transit-stations/information/}
#' @keywords data
NULL


