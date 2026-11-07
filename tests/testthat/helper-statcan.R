# Evaluate code with the cancensus cache pointing at a fresh temporary directory
with_statcan_cache <- function(code) {
  cache <- tempfile("cancensus_test_cache")
  dir.create(cache)
  old_env <- Sys.getenv("CM_CACHE_PATH")
  Sys.setenv(CM_CACHE_PATH = cache)
  on.exit({
    Sys.setenv(CM_CACHE_PATH = old_env)
    unlink(cache, recursive = TRUE)
  })
  force(code)
}

fake_response <- function(status, headers = list()) {
  structure(list(status_code = as.integer(status), headers = httr::insensitive(headers)),
            class = "response")
}
