# Functions for managing user settings, API keys, cache locations

#' Set Censusmapper API key
#'
#' @description Cancensus requires a free Censusmapper API key to retrieve data. This function helps set the key for either the duration of the session (default) or permanently for use across sessions.
#'
#' @param key a Censusmapper API key. For more information on keys see the \href{https://mountainmath.github.io/cancensus/index.html#api-key}{API key section}
#' @param overwrite Option to overwrite any existing Censusmapper keys already stored locally.
#' @param install Option to install permanently for use across sessions.
#'
#' @export
#'
#' @examples
#'\dontrun{
#' set_cancensus_api_key("YOUR_CM_API_KEY")
#'
#' # This will set the key permanently until overwritten again
#' set_cancensus_api_key("YOUR_CM_API_KEY", install = TRUE)
#' }
set_cancensus_api_key <- function(key, overwrite = FALSE, install = FALSE){
  if (install) {
    home <- Sys.getenv("HOME")
    renv <- file.path(home, ".Renviron")
    if(!file.exists(renv)){
      file.create(renv)
    } else{
      # Backup original .Renviron before doing anything else here.
      file.copy(renv, file.path(home, ".Renviron_backup"))
      if(isTRUE(overwrite)){
        message("Adding key to your .Renviron file. Your original .Renviron will be backed up and stored in your R HOME directory if needed.")
        oldenv=readLines(renv)
        newenv <- oldenv[-grep("CM_API_KEY", oldenv)]
        writeLines(newenv, renv, sep = "\n")
      }
      else{
        tv <- readLines(renv)
        if(any(grepl("CM_API_KEY",tv))){
          stop("An existing Censusmapper API key is already saved. You can overwrite it with the argument overwrite=TRUE.", call.=FALSE)
        }
      }
    }

    keyconcat <- paste0("CM_API_KEY='", key, "'")
    # Append API key to .Renviron file
    write(keyconcat, renv, sep = "\n", append = TRUE)
    message('Your API key has been stored in your .Renviron and can be accessed by Sys.getenv("CM_API_KEY"). \nTo use now, restart R or run `readRenviron("~/.Renviron")`')
  } else {
    message("API key set for duration of session. To install your API key for use across sessions, run this function with `install = TRUE`.")
    Sys.setenv(CM_API_KEY = key)
  }

}

#' Set persistent cancensus cache location
#'
#' @description Cancensus provides session caching for retrieved data to increase speeds and reduce API usage. This function will create a persistent cache across sessions.
#'
#' @param cache_path a local directory to use for saving cached data
#' @param overwrite Option to overwrite any existing cache path already stored locally.
#' @param install Option to install permanently for use across sessions.
#'
#' @export
#'
#' @examples
#'\dontrun{
#' set_cancensus_cache_path("~/cancensus_cache")
#'
#' # This will set the cache path permanently until ovewritten again
#' set_cancensus_cache_path("~/cancensus_cache", install = TRUE)
#' }
set_cancensus_cache_path <- function(cache_path, overwrite = FALSE, install = FALSE){
  if (install) {
    home <- Sys.getenv("HOME")
    renv <- file.path(home, ".Renviron")
    if(!file.exists(renv)){
      file.create(renv)
    } else{
      # Backup original .Renviron before doing anything else here.
      file.copy(renv, file.path(home, ".Renviron_backup"))
      if(isTRUE(overwrite)){
        message("Your original .Renviron will be backed up and stored in your R HOME directory if needed.")
        oldenv=readLines(renv)
        newenv <- oldenv[-grep("CM_CACHE_PATH", oldenv)]
        writeLines(newenv, renv, sep = "\n")
      } else{
        tv <- readLines(renv)
        if(any(grepl("CM_CACHE_PATH",tv))){
          stop("A saved cache already exists. You can overwrite it with the argument overwrite=TRUE.", call.=FALSE)
        }
      }
    }

    keyconcat <- paste0("CM_CACHE_PATH='", cache_path, "'")
    # Append cache path .Renviron file
    write(keyconcat, renv, sep = "\n", append = TRUE)
    message('Your cache path has been stored in your .Renviron and can be accessed by Sys.getenv("CM_CACHE_PATH"). \nTo use now, restart R or run readRenviron("~/.Renviron").')
  } else {
    message("Cache set for duration of session. To permanently add your cache path for use across sessions, run this function with install = TRUE.")
    Sys.setenv('CM_CACHE_PATH' = cache_path)
  }
  cache_path
}

#' View saved Censusmapper API key
#'
#' @description View saved API key'
#'
#' @export
#'
#' @examples
#' show_cancensus_api_key()
show_cancensus_api_key <- function() {
  key <- Sys.getenv('CM_API_KEY')
  if (key==""){
    message("No api key path set")
    key <- NULL
  }
  key
}

#' View saved cache directory path
#'
#' @description View saved cache path'
#'
#' @export
#'
#' @examples
#' show_cancensus_cache_path()
show_cancensus_cache_path <- function() {
  path <- Sys.getenv('CM_CACHE_PATH')
  if (path==""){
    message("No cache path set")
    path <- NULL
  }
  path
}

cm_no_cache_path_message <- paste(
  "Census data is currently stored temporarily.\n\n",
  "In order to speed up performance, reduce API quota usage, and reduce",
  "unnecessary network calls, please set up a persistent cache directory via",
  "`set_cancensus_cache_path(<local cache path>, install = TRUE)`.\n\n",
  "to set the environment variable CM_CACHE_PATH= '<path to cancensus cache directory>' or ",
  "setting options(cancensus.cache_path = '<path to cancensus cache directory>')\n\n",
  "This will add your cache directory as environment varianble to your .Renviron to be",
  "used across sessions and projects.\n\n"
)
