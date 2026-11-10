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
        cc_inform("api_key_adding")
        oldenv=readLines(renv)
        key_lines <- grep("CM_API_KEY", oldenv)
        newenv <- if (length(key_lines) > 0) oldenv[-key_lines] else oldenv
        writeLines(newenv, renv, sep = "\n")
      }
      else{
        tv <- readLines(renv)
        if(any(grepl("CM_API_KEY",tv))){
          cc_abort("api_key_exists")
        }
      }
    }

    keyconcat <- paste0("CM_API_KEY='", key, "'")
    # Append API key to .Renviron file
    write(keyconcat, renv, sep = "\n", append = TRUE)
    Sys.setenv(CM_API_KEY = key)
    cc_inform("api_key_installed")
  } else {
    cc_inform("api_key_session")
    Sys.setenv(CM_API_KEY = key)
  }

}

#' Set cancensus cache location
#'
#' @description Cancensus caches retrieved data to increase speeds and reduce API usage. By default the
#' cache only lasts for the R session. This function sets a cache location that persists across
#' sessions, either for all projects or for a specific project.
#'
#' @details
#' With `install = TRUE` the cache path is added to your `.Renviron` file and is used across sessions
#' and projects.
#'
#' With `install = FALSE` the cache path applies to the current R session, taking precedence over one
#' stored in `.Renviron`. Calling this at the top of a script gives a project its own cache, for example to
#' back up the data together with the project for reproducibility. All cache functions, like
#' [list_cancensus_cache()] and [remove_recalled_cached_data()], then work on this project cache.
#'
#' - Use an absolute path, for example via `here::here()`. Relative paths get resolved
#'   against the working directory at the time of use, which can differ e.g. when knitting a document.
#' - Set the path before the first call to [get_census()]. The check of the whole cache for recalled data
#'   only runs once per session, data read from a cache set later only gets checked as it is read.
#' - Don't set the cache path in a project-level `.Renviron` file unless it also has your API key. R
#'   reads a project-level `.Renviron` file instead of the one in your home directory, not in addition to it.
#'
#' The cache path is stored in the `CM_CACHE_PATH` environment variable. The older
#' `cancensus.cache_path` option is still supported, but is only used if the environment variable is not set.
#'
#' @param cache_path a local directory to use for saving cached data
#' @param overwrite Option to overwrite any existing cache path already stored in `.Renviron`, only
#' relevant with `install = TRUE`.
#' @param install Option to install permanently for use across sessions and projects.
#'
#' @md
#' @export
#'
#' @examples
#'\dontrun{
#' # This will set the cache path for the current session, for example for a project-specific cache
#' set_cancensus_cache_path("~/my_project/cancensus_cache")
#'
#' # This will set the cache path permanently until overwritten again
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
        cc_inform("renviron_backup")
        oldenv=readLines(renv)
        path_lines <- grep("CM_CACHE_PATH", oldenv)
        newenv <- if (length(path_lines) > 0) oldenv[-path_lines] else oldenv
        writeLines(newenv, renv, sep = "\n")
      } else{
        tv <- readLines(renv)
        if(any(grepl("CM_CACHE_PATH",tv))){
          cc_abort("cache_path_exists")
        }
      }
    }

    keyconcat <- paste0("CM_CACHE_PATH='", cache_path, "'")
    # Append cache path .Renviron file
    write(keyconcat, renv, sep = "\n", append = TRUE)
    cc_inform("cache_path_installed")
    Sys.setenv('CM_CACHE_PATH' = cache_path)
  } else {
    cc_inform("cache_path_session", path = cache_path)
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
#' \dontrun{
#' show_cancensus_api_key()
#' }
show_cancensus_api_key <- function() {
  key <- Sys.getenv('CM_API_KEY')
  if (key==""){
    cc_inform("no_api_key_set")
    key <- NULL
  }
  key
}

#' View saved cache directory path
#'
#' @description View the cache path in use, from the `CM_CACHE_PATH` environment variable or
#' otherwise the `cancensus.cache_path` option.
#'
#' @return The cache path, or `NULL` with a message if none is set
#'
#' @md
#' @export
#'
#' @examples
#' \dontrun{
#' show_cancensus_cache_path()
#' }
show_cancensus_cache_path <- function() {
  path <- Sys.getenv('CM_CACHE_PATH')
  if (path=="") path <- getOption("cancensus.cache_path")
  if (is.null(path)) cc_inform("no_cache_path_set")
  path
}
