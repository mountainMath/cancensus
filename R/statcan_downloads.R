# Parts of the Statistics Canada website (www12.statcan.gc.ca) sit behind a Cloudflare browser
# verification that can't be completed from R. These helpers detect the block and fall back on
# files that users download in their browser and deposit in the `statcan_downloads` folder of
# the cache directory.

# Folder for manually downloaded files. `base_path` overrides the cancensus cache path, for
# functions with a `cache_path` argument.
statcan_downloads_path <- function(base_path = NULL){
  if (is.null(base_path)) {
    cache_path("statcan_downloads")
  } else {
    file.path(base_path, "statcan_downloads")
  }
}

statcan_cache_is_persistent <- function(base_path = NULL){
  !is.null(base_path) ||
    nchar(Sys.getenv("CM_CACHE_PATH")) > 0 ||
    !is.null(getOption("cancensus.cache_path"))
}

# Checks the file signature instead of listing the contents, so that the check also works for
# large archives and catches HTML pages saved under a .zip name.
statcan_is_zip <- function(path){
  if (!file.exists(path) || dir.exists(path)) return(FALSE)
  signature <- readBin(path, what = "raw", n = 4)
  identical(signature, as.raw(c(0x50, 0x4b, 0x03, 0x04))) ||
    identical(signature, as.raw(c(0x50, 0x4b, 0x05, 0x06)))
}

statcan_is_cloudflare_block <- function(response){
  headers <- httr::headers(response)
  !is.null(headers[["cf-mitigated"]]) ||
    (httr::status_code(response) %in% c(403, 429, 503) &&
       identical(tolower(headers[["server"]]), "cloudflare"))
}

# Warnings get truncated at the `warning.length` option, which by default is short enough to
# cut off the instructions when paths are long.
statcan_warning <- function(message){
  old <- options(warning.length = 8170L)
  on.exit(options(old))
  warning(message, call. = FALSE, immediate. = TRUE, noBreaks. = TRUE)
}

statcan_os <- function(){
  Sys.info()[["sysname"]]
}

# Paths in messages get pasted into file managers, so write them the way the OS does
statcan_display_path <- function(path){
  path <- path.expand(path)
  if (statcan_os() == "Windows") path <- gsub("/", "\\", path, fixed = TRUE)
  path
}

statcan_fallback_message <- function(has_cache){
  if (has_cache) "Returning the existing cached data instead." else "Returning NULL."
}

statcan_blocked_message <- function(url, manual_file, persistent, has_cache = FALSE){
  unzipped_dir <- sub("\\.zip$", "", manual_file)
  paste0(
    "Statistics Canada blocked this download with a Cloudflare browser verification, ",
    "which can't be completed from R. ", statcan_fallback_message(has_cache), "\n\n",
    "The data can still be downloaded manually:\n",
    "1. Paste this link into your web browser and complete the verification if prompted:\n",
    "   ", url, "\n",
    if (statcan_os() == "Darwin") {
      paste0("   Safari unzips downloads automatically, so use another browser such as Chrome or ",
             "Firefox, or turn off \"Open 'safe' files after downloading\" in Safari's settings.\n")
    },
    "2. Move the downloaded file, without unzipping or renaming it, to:\n",
    "   ", statcan_display_path(manual_file), "\n",
    "3. Run this command again", if (has_cache) " with `refresh = TRUE`",
    ". cancensus will load the data from that file, ",
    "add it to the cache, and remove the downloaded file.",
    if (dir.exists(unzipped_dir)) {
      paste0("\n\nThe folder\n   ", statcan_display_path(unzipped_dir), "\nlooks like an unzipped ",
             "copy of this file, which cancensus can't read. It needs the original zip file.")
    },
    if (!persistent) {
      paste0("\n\nNo persistent cache path is set, so this location gets deleted when the R ",
             "session ends. Set one with `set_cancensus_cache_path()` to keep the data across ",
             "sessions, the location above will change accordingly.")
    }
  )
}

# Get a zip file from Statistics Canada, preferring a manually downloaded copy.
#
# Returns the path to the zip file, which the caller should remove after use, or NULL with a
# warning if the file can't be obtained because of the Cloudflare block or because the manually
# downloaded file is invalid. Other download problems are errors. Callers set `has_cache` when
# refreshing existing cached data, which they then fall back on if this returns NULL.
statcan_download_zip <- function(url, base_path = NULL, timeout = 10000, quiet = FALSE, has_cache = FALSE){
  downloads_path <- statcan_downloads_path(base_path)
  manual_file <- file.path(downloads_path, basename(sub("\\?.*$", "", url)))

  if (file.exists(manual_file)) {
    if (!statcan_is_zip(manual_file)) {
      statcan_warning(paste0(
        "The manually downloaded file is not a valid zip file, it may be incomplete or have ",
        "been saved in place of the actual data. ", statcan_fallback_message(has_cache), "\n\n",
        "Please replace\n   ", statcan_display_path(manual_file),
        "\nwith a new download from\n   ", url))
      return(NULL)
    }
    if (!quiet) message("Reading manually downloaded file ", statcan_display_path(manual_file))
    return(manual_file)
  }

  tmp <- tempfile(fileext = ".zip")
  response <- httr::GET(url, cm_request_config(quiet),
                        httr::write_disk(tmp, overwrite = TRUE),
                        httr::timeout(timeout))

  if (statcan_is_cloudflare_block(response)) {
    unlink(tmp)
    if (!quiet) cat("\n") # the progress bar for the challenge page leaves the line open
    if (!dir.exists(downloads_path)) dir.create(downloads_path, recursive = TRUE, showWarnings = FALSE)
    statcan_warning(statcan_blocked_message(url, manual_file, statcan_cache_is_persistent(base_path), has_cache))
    return(NULL)
  }
  if (httr::http_error(response)) {
    unlink(tmp)
    stop("Could not download ", url, " from Statistics Canada, the server responded with HTTP status ",
         httr::status_code(response), ".", call. = FALSE)
  }
  if (!statcan_is_zip(tmp)) {
    unlink(tmp)
    stop("The download of ", url, " from Statistics Canada did not return a zip file.", call. = FALSE)
  }
  tmp
}
