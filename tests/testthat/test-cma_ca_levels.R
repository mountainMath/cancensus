level_test_response <- function(body) {
  structure(list(status_code = 200L, content = charToRaw(body),
                 headers = httr::insensitive(list(`data-version` = "d.1", `content-type` = "text/plain"))),
            class = "response")
}

# Runs get_census against a mocked endpoint, returning the result and the request body
get_census_with_mocked_api <- function(regions, level, ...,
                                       csv = "GeoUID,Type,Region Name,Population,rpid,rgid\n35562,CMA,Sarnia (K),1,35,01\n") {
  rlang::local_options(cancensus.cache_path = tempfile("cancensus_test_cache"))
  old_env <- Sys.getenv("CM_CACHE_PATH")
  Sys.unsetenv("CM_CACHE_PATH")
  on.exit(Sys.setenv(CM_CACHE_PATH = old_env), add = TRUE)
  dir.create(getOption("cancensus.cache_path"))
  request <- NULL
  local_mocked_bindings(
    first_run_checks = function() invisible(NULL),
    handle_cm_status_code = function(...) invisible(NULL),
    cm_POST = function(url, body, ...) {
      request <<- body
      level_test_response(csv)
    }
  )
  result <- get_census("CA21", regions = regions, level = level, api_key = "CensusMapper_test",
                       labels = "detailed", quiet = TRUE, ...)
  list(result = result, request = request)
}

test_that("get_census accepts CA and CMACA as level and passes them on to the API", {
  for (level in c("CA", "CMACA")) {
    expect_no_warning(response <- get_census_with_mocked_api(list(C = "01"), level))
    expect_equal(response$request$level, level)
    expect_equal(response$result$GeoUID, "35562")
  }
})

test_that("CA and CMACA levels get the same column names as the CMA level", {
  cma_names <- names(get_census_with_mocked_api(list(C = "01"), "CMA")$result)
  expect_true(all(c("PR_UID", "C_UID") %in% cma_names))
  for (level in c("CA", "CMACA")) {
    expect_equal(names(get_census_with_mocked_api(list(C = "01"), level)$result), cma_names)
  }
})

test_that("get_census accepts CA and CMACA as names of regions and passes them on to the API", {
  for (key in c("CA", "CMACA")) {
    regions <- stats::setNames(list("35562"), key)
    response <- get_census_with_mocked_api(regions, "Regions")
    expect_equal(jsonlite::fromJSON(response$request$regions), stats::setNames(list("35562"), key))
  }
})

test_that("census agglomerations from as_census_region_list are accepted by get_census", {
  regions <- as_census_region_list(dplyr::tibble(region = "35562", level = "CA"))
  response <- get_census_with_mocked_api(regions, "Regions")
  expect_equal(response$result$GeoUID, "35562")
})

test_that("get_census still rejects unknown levels", {
  expect_error(get_census("CA21", regions = list(C = "01"), level = "XX", api_key = "CensusMapper_test"),
               "`level` parameter must be one of")
  expect_error(get_census("CA21", regions = list(XX = "01"), api_key = "CensusMapper_test"),
               "Regions must be composed of valid census aggregation levels")
})

test_that("get_census points to the WDS for the ADA level, which CensusMapper does not have", {
  local_mocked_bindings(cm_POST = function(...) stop("Unexpected request"))
  expect_error(get_census("CA21", regions = list(CSD = "5915022"), level = "ADA", api_key = "CensusMapper_test"),
               "get_statcan_wds_data", class = "cancensus_ada_level_unsupported")
  expect_error(get_census("CA21", regions = list(ADA = "59150019"), api_key = "CensusMapper_test"),
               "get_statcan_wds_data", class = "cancensus_ada_level_unsupported")
})

test_that("get_census checks the resolution when asking for geometries, and only then", {
  local_mocked_bindings(first_run_checks = function() invisible(NULL))
  expect_error(get_census("CA21", regions = list(C = "01"), geo_format = "sf", resolution = "bogus",
                          api_key = "CensusMapper_test", labels = "detailed", quiet = TRUE),
               class = "cancensus_invalid_resolution")
  expect_no_error(get_census_with_mocked_api(list(C = "01"), "CMA", resolution = "bogus"))
})

test_that("get_census sends the vectors as a JSON list, also when there are none", {
  expect_equal(as.character(get_census_with_mocked_api(list(C = "01"), "CMA")$request$vectors), "[]")
  csv <- "GeoUID,Type,Region Name,Population,rpid,rgid,v_CA21_1: Population\n35562,CMA,Sarnia (K),1,35,01,1\n"
  request <- get_census_with_mocked_api(list(C = "01"), "CMA", vectors = c(pop = "v_CA21_1"), csv = csv)$request
  expect_equal(as.character(request$vectors), '["v_CA21_1"]')
})
