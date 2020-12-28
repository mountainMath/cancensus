# Internal functions that do useful things frequently required in other functions

cancensus_base_url <- function(){
  url <- getOption("cancensus.base_url")
  if (is.null(url)) url <- "https://censusmapper.ca"
  url
}

robust_api_key <- function(api_key){
  api_key <- if (is.null(api_key) && nchar(Sys.getenv("CM_API_KEY")) > 1) { Sys.getenv("CM_API_KEY") } else { api_key }
  api_key <- if (is.null(api_key) && !is.null(getOption("cancensus.api_key"))) { getOption("cancensus.api_key") } else { api_key }
  api_key
}

# Append arguments to the path of the local cache directory.
cache_path <- function(...) {
  cache_dir <- Sys.getenv("CM_CACHE_PATH")
  if (nchar(cache_dir)==0) cache_dir <- getOption("cancensus.cache_path")
  if (is.null(cache_dir) | nchar(cache_dir)==0) cache_dir <- tempdir()
  if (!is.character(cache_dir)) {
    stop("Corrupt 'CM_CACHE_PATH' environment variable or 'cancensus.cache_path' option. Must be a path.",
         .call = FALSE)
  }
  if (!file.exists(cache_dir)) {
    dir.create(cache_dir, showWarnings = FALSE)
  }
  file.path(cache_dir, paste0(...))
}

clean_vector_list <- function(vector_list,dataset=NULL){
  if (!("data.frame") %in% class(vector_list)) {
    if (class(vector_list)=="character") {
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
    vectors = ifelse(class(vector_list)=="character",vector_list,vector_list$vector)
    dataset <- vectors %>%
      as.character() %>%
      lapply(function(d)unlist(strsplit(d,"_"))[2]) %>%
      unlist() %>%
      unique()
    if (length(dataset)!=1) stop("Unable to determine dataset")
  }
  dataset
}

cancensus_na_strings <- c("x", "F", "...", "..", "-","N","*","**")

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


install_api_key <- function(key, overwrite = FALSE, install = FALSE){
  if (install) {
    home <- Sys.getenv("HOME")
    renv <- file.path(home, ".Renviron")
    if(file.exists(renv)){
      # Backup original .Renviron before doing anything else here.
    }
    if(!file.exists(renv)){
      file.create(renv)
    }
    else{
      file.copy(renv, file.path(home, ".Renviron_backup"))
      if(isTRUE(overwrite)){
        message("Your original .Renviron will be backed up and stored in your R HOME directory if needed.")
        oldenv=readLines(renv)
        newenv <- oldenv[-grep("CM_API_KEY", oldenv)]
        writeLines(newenv, renv, sep = "\n")
      }
      else{
        tv <- readLines(renv)
        if(any(grepl("CM_API_KEY",tv))){
          stop("A CM_API_KEY already exists. You can overwrite it with the argument overwrite=TRUE", call.=FALSE)
        }
      }
    }

    keyconcat <- paste0("CM_API_KEY='", key, "'")
    # Append API key to .Renviron file
    write(keyconcat, renv, sep = "\n", append = TRUE)
    message('Your API key has been stored in your .Renviron and can be accessed by Sys.getenv("CM_API_KEY"). \nTo use now, restart R or run `readRenviron("~/.Renviron")`')
    return(key)
  } else {
    message("To install your API key for use in future sessions, run this function with `install = TRUE`.")
    Sys.setenv(CM_API_KEY = key)
  }
}

cm_no_cache_path_message <- paste(
  "Census data is currently stored temporarily.\n\n",
  "In order to speed up performance, reduce API quota usage, and reduce",
  "unnecessary network calls, please set up a persistent cache directory by",
  "setting the environment variable CM_CACHE_PATH= '<path to cancensus cache directory>' or ",
  "setting options(cancensus.cache_path = '<path to cancensus cache directory>')\n\n",
  "You may add this environment varianble to your .Renviron",
  "or add this option, together with your API key, to your .Rprofile.\n\n"
)


#' A dataset with code table summaries for census data
#' @name CODES_TABLE
#' @docType data
#' @author derived from StatCan definitions
#' @references \url{https://www12.statcan.gc.ca/census-recensement/2016/ref/dict/geo012-eng.cfm}
#' @keywords data
NULL


