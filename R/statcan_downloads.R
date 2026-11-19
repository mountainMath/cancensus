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
# cut off the instructions when paths are long. They get shown right away and not when the calling
# code is done, as that code may well fail without the file.
statcan_warning <- function(id, message){
  old <- options(warning.length = 8170L, warn = max(1, getOption("warn")))
  on.exit(options(old))
  rlang::warn(message, class = c(paste0("cancensus_", id), "cancensus_warning"))
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

statcan_fallback_message <- function(has_cache, language = NULL){
  cc_msg(if (has_cache) "statcan_fallback_cache" else "statcan_fallback_null", .language = language)
}

statcan_blocked_message <- function(url, manual_file, persistent, has_cache = FALSE, language = NULL){
  msg <- function(id, ...) cc_msg(id, ..., .language = language)
  unzipped_dir <- sub("\\.zip$", "", manual_file)
  paste0(
    msg("statcan_blocked", fallback = statcan_fallback_message(has_cache, language)), "\n\n",
    msg("statcan_manual_download"), "\n",
    "1. ", msg("statcan_manual_step_link"), "\n",
    "   ", url, "\n",
    if (statcan_os() == "Darwin") paste0("   ", msg("statcan_manual_safari"), "\n"),
    "2. ", msg("statcan_manual_step_move"), "\n",
    "   ", statcan_display_path(manual_file), "\n",
    "3. ", msg(if (has_cache) "statcan_manual_step_rerun_refresh" else "statcan_manual_step_rerun"),
    if (dir.exists(unzipped_dir)) {
      paste0("\n\n", msg("statcan_unzipped_folder", path = statcan_display_path(unzipped_dir)))
    },
    if (!persistent) paste0("\n\n", msg("statcan_cache_not_persistent"))
  )
}

# Get a zip file from Statistics Canada, preferring a manually downloaded copy.
#
# Returns the path to the zip file, which the caller should remove after use, or NULL with a
# warning if the file can't be obtained because of the Cloudflare block or because the manually
# downloaded file is invalid. Other download problems are errors. Callers set `has_cache` when
# refreshing existing cached data, which they then fall back on if this returns NULL.
statcan_download_zip <- function(url, base_path = NULL, timeout = 10000, quiet = FALSE, has_cache = FALSE,
                                 language = NULL){
  downloads_path <- statcan_downloads_path(base_path)
  manual_file <- file.path(downloads_path, basename(sub("\\?.*$", "", url)))

  if (file.exists(manual_file)) {
    if (!statcan_is_zip(manual_file)) {
      statcan_warning("statcan_invalid_manual_file",
                      cc_msg("statcan_invalid_manual_file",
                             fallback = statcan_fallback_message(has_cache, language),
                             path = statcan_display_path(manual_file), url = url,
                             .language = language))
      return(NULL)
    }
    if (!quiet) cc_inform("reading_manual_file", path = statcan_display_path(manual_file), .language = language)
    return(manual_file)
  }

  tmp <- tempfile(fileext = ".zip")
  response <- cm_GET(url, httr::write_disk(tmp, overwrite = TRUE), httr::timeout(timeout), quiet = quiet)

  if (statcan_is_cloudflare_block(response)) {
    unlink(tmp)
    if (!quiet) cat("\n") # the progress bar for the challenge page leaves the line open
    if (!dir.exists(downloads_path)) dir.create(downloads_path, recursive = TRUE, showWarnings = FALSE)
    statcan_warning("statcan_blocked", statcan_blocked_message(url, manual_file, cache_is_persistent(base_path), has_cache,
                                            language = language))
    return(NULL)
  }
  if (httr::http_error(response)) {
    unlink(tmp)
    cc_abort("statcan_http_error", url = url, status = httr::status_code(response), .language = language)
  }
  if (!statcan_is_zip(tmp)) {
    unlink(tmp)
    cc_abort("statcan_not_zip", url = url, .language = language)
  }
  warn_if_cache_not_persistent(base_path, quiet = quiet, language = language)
  tmp
}
