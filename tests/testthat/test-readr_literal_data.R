# readr deprecates passing literal data without wrapping it in I(), and only warns once every
# 8 hours by default. Forcing the warnings makes sure they don't depend on test order.

csv_response <- function(body) {
  structure(list(status_code = 200L, content = charToRaw(body),
                 headers = httr::insensitive(list(`data-version` = "d.1", `content-type` = "text/plain"))),
            class = "response")
}

# Only looks for the readr warning, forcing warnings also surfaces unrelated soft deprecations
expect_no_readr_literal_warning <- function(code) {
  w <- capture_warnings(code)
  expect_false(any(grepl("for literal data", w, fixed = TRUE)))
}

test_that("get_census parses downloaded data without readr deprecation warnings", {
  old_env <- Sys.getenv("CM_CACHE_PATH")
  Sys.unsetenv("CM_CACHE_PATH")
  on.exit(Sys.setenv(CM_CACHE_PATH = old_env), add = TRUE)
  rlang::local_options(lifecycle_verbosity = "warning", cancensus.cache_path = tempfile("cancensus_test_cache"))
  local_mocked_bindings(
    first_run_checks = function() invisible(NULL),
    handle_cm_status_code = function(...) invisible(NULL),
    cm_POST = function(...) csv_response("GeoUID,Type,Region Name,v_CA16_408: Total\n5915022,CSD,Vancouver,1\n")
  )

  expect_no_readr_literal_warning(result <- get_census("CA16", regions = list(CSD = "5915022"), level = "CSD",
                                         vectors = "v_CA16_408", api_key = "CensusMapper_test",
                                         quiet = TRUE))
  expect_equal(result$GeoUID, "5915022")
  expect_equal(result[["v_CA16_408: Total"]], 1)
})

test_that("list_census_regions parses downloaded data without readr deprecation warnings", {
  rlang::local_options(lifecycle_verbosity = "warning")
  local_mocked_bindings(
    handle_cm_status_code = function(...) invisible(NULL),
    cm_GET = function(...) {
      csv_response(paste0("geo_uid,name,type,population,flag,CMA_UID,CD_UID,PR_UID\n",
                          "5915022,Vancouver,CSD,1,CY,59933,5915,59\n"))
    }
  )
  expect_no_readr_literal_warning(result <- list_census_regions("CA16", use_cache = FALSE, quiet = TRUE))
  expect_equal(result$region, "5915022")
})
