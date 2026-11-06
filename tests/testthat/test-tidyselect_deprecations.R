# tidyselect deprecates .data in selections as a soft deprecation, which only warns when called
# directly from the global environment or package tests. Forcing the warnings makes them visible.

expect_no_tidyselect_warning <- function(code) {
  w <- capture_warnings(code)
  expect_false(any(grepl(".data in tidyselect", w, fixed = TRUE)))
}

mock_regions <- function(env = parent.frame()) {
  local_mocked_bindings(
    handle_cm_status_code = function(...) invisible(NULL),
    cm_GET = function(...) {
      body <- paste0("geo_uid,name,type,population,flag,CMA_UID,CD_UID,PR_UID\n",
                     "5915022,Vancouver,CSD,1,CY,59933,5915,59\n",
                     "5915001,Vancouver,CSD,2,DM,59933,5915,59\n")
      structure(list(status_code = 200L, content = charToRaw(body),
                     headers = httr::insensitive(list(`content-type` = "text/plain"))),
                class = "response")
    },
    .env = env
  )
}

test_that("list_census_regions selects columns without tidyselect deprecation warnings", {
  rlang::local_options(lifecycle_verbosity = "warning")
  mock_regions()
  expect_no_tidyselect_warning(result <- list_census_regions("CA16", use_cache = FALSE, quiet = TRUE))
  expect_named(result, c("region", "name", "level", "pop", "municipal_status", "CMA_UID", "CD_UID", "PR_UID"),
               ignore.order = TRUE)
  expect_type(result$pop, "integer")
})

test_that("add_unique_names_to_region_list removes the helper column without deprecation warnings", {
  rlang::local_options(lifecycle_verbosity = "warning")
  mock_regions()
  regions <- suppressWarnings(list_census_regions("CA16", use_cache = FALSE, quiet = TRUE))
  expect_no_tidyselect_warning(result <- add_unique_names_to_region_list(regions))
  expect_false("count" %in% names(result))
  expect_equal(anyDuplicated(result$Name), 0)
})

test_that("list_cancensus_cache handles files without metadata without deprecation warnings", {
  cache <- tempfile("cancensus_test_cache")
  dir.create(cache)
  old_env <- Sys.getenv("CM_CACHE_PATH")
  Sys.setenv(CM_CACHE_PATH = cache)
  on.exit({
    Sys.setenv(CM_CACHE_PATH = old_env)
    unlink(cache, recursive = TRUE)
  }, add = TRUE)
  rlang::local_options(lifecycle_verbosity = "warning")
  writeLines("some data", file.path(cache, "CM_data_test.rda"))

  expect_no_tidyselect_warning(result <- list_cancensus_cache())
  expect_equal(result$path, "CM_data_test.rda")
  expect_false("manual_size" %in% names(result))
  expect_gt(result$size, 0)
})
