# Parts of the Statistics Canada website (www12.statcan.gc.ca) sit behind a Cloudflare browser
# verification that plain downloads from R can't get past. These helpers detect the block and send a
# real browser through it with the safeconduct package when that is installed and finds Chrome or
# Chromium. Otherwise, or when that fails, they fall back on files that users download in their
# browser and deposit in the `statcan_downloads` folder of the cache directory.

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

# Wrappers, so that tests can mock them
safeconduct_installed <- function(){
  requireNamespace("safeconduct", quietly = TRUE)
}

safeconduct_browser_available <- function(){
  safeconduct::browser_available()
}

# Whether a blocked download can be retried in a browser with safeconduct: "available",
# "disabled" by the `cancensus.use_browser` option, "not_installed", or "no_browser" with the
# reason safeconduct gives in the `reason` attribute.
statcan_browser_status <- function(){
  if (!isTRUE(getOption("cancensus.use_browser", TRUE))) return("disabled")
  if (!safeconduct_installed()) return("not_installed")
  available <- safeconduct_browser_available()
  if (isTRUE(available)) return("available")
  structure("no_browser", reason = attr(available, "reason"))
}

# Download `url` to `destfile` through a browser that passes the check. Only `fetch()` and
# `browser_available()` get used, which behave the same since safeconduct 0.1.0. Returns NULL on
# success, and otherwise the reason it failed.
statcan_browser_fetch <- function(url, destfile, timeout = 3600, quiet = FALSE, language = NULL){
  problem <- tryCatch({
    safeconduct::fetch(url, destfile, quiet = quiet, timeout = timeout)
    NULL
  }, safeconduct_error = function(e) conditionMessage(e))
  if (is.null(problem) && !statcan_is_zip(destfile)) {
    problem <- cc_msg("statcan_not_zip", url = url, .language = language)
  }
  if (!is.null(problem)) unlink(destfile)
  problem
}

# The paragraph of the blocked warning about getting past the check with a browser, NULL if
# there is nothing to say
statcan_browser_note <- function(status, problem = NULL, language = NULL){
  switch(as.character(status),
         not_installed = cc_msg("statcan_browser_install", .language = language),
         no_browser = cc_msg("statcan_browser_missing",
                             reason = rlang::`%||%`(attr(status, "reason"), ""), .language = language),
         available = if (!is.null(problem)) {
           cc_msg("statcan_browser_failed", problem = problem, .language = language)
         },
         NULL)
}

statcan_fallback_message <- function(has_cache, language = NULL){
  cc_msg(if (has_cache) "statcan_fallback_cache" else "statcan_fallback_null", .language = language)
}

statcan_blocked_message <- function(url, manual_file, persistent, has_cache = FALSE, language = NULL,
                                    browser_note = NULL){
  msg <- function(id, ...) cc_msg(id, ..., .language = language)
  unzipped_dir <- sub("\\.zip$", "", manual_file)
  paste0(
    msg("statcan_blocked", fallback = statcan_fallback_message(has_cache, language)), "\n\n",
    if (!is.null(browser_note)) paste0(browser_note, "\n\n"),
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
# A download blocked by Cloudflare is tried again through a browser with safeconduct where
# possible, see `statcan_browser_status()`.
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
    status <- statcan_browser_status()
    problem <- NULL
    if (identical(as.character(status), "available")) {
      problem <- statcan_browser_fetch(url, tmp, timeout = timeout, quiet = quiet, language = language)
      if (is.null(problem)) {
        warn_if_cache_not_persistent(base_path, quiet = quiet, language = language)
        return(tmp)
      }
    }
    if (!dir.exists(downloads_path)) dir.create(downloads_path, recursive = TRUE, showWarnings = FALSE)
    statcan_warning("statcan_blocked", statcan_blocked_message(url, manual_file, cache_is_persistent(base_path), has_cache,
                                            language = language,
                                            browser_note = statcan_browser_note(status, problem, language)))
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
