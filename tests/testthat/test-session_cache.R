json_response <- function(body) {
  structure(list(status_code = 200L, content = charToRaw(body),
                 headers = httr::insensitive(list(`content-type` = "application/json"))),
            class = "response")
}

csv_response <- function(body) {
  structure(list(status_code = 200L, content = charToRaw(body),
                 headers = httr::insensitive(list(`content-type` = "text/csv"))),
            class = "response")
}

forget <- function(keys) {
  rm(list = intersect(keys, ls(.cancensus_session_cache)), envir = .cancensus_session_cache)
}

test_that("once_per_session is only TRUE the first time for a key", {
  key <- basename(tempfile("once"))
  expect_true(once_per_session(key))
  expect_false(once_per_session(key))
  expect_true(once_per_session(paste0(key, "_other")))
})

test_that("the check of the cache for recalled data runs once per session", {
  forget("first_run_checks")
  checks <- 0
  local_mocked_bindings(check_for_recalled_data_and_warn = function() checks <<- checks + 1)
  first_run_checks()
  first_run_checks()
  expect_equal(checks, 1)
})

test_that("the list of datasets is kept for the session", {
  forget("datasets")
  on.exit(forget("datasets"))
  requests <- 0
  local_mocked_bindings(cm_GET = function(...) {
    requests <<- requests + 1
    json_response('[{"dataset":"CA21","description":"2021 Canada Census","geo_dataset":"CA21"}]')
  })
  expect_message(datasets <- list_census_datasets(), class = "cancensus_querying_datasets")
  expect_equal(datasets$dataset, "CA21")
  expect_message(cached <- list_census_datasets(), class = "cancensus_reading_datasets_cache")
  expect_no_message(list_census_datasets(quiet = TRUE))
  expect_equal(cached, datasets)
  expect_equal(requests, 1)

  suppressMessages(list_census_datasets(use_cache = FALSE))
  expect_equal(requests, 2)

  # warns when the list is more than a day old, unless quiet
  attr(datasets, "last_updated") <- Sys.time() - as.difftime(2, units = "days")
  session_cache_set("datasets", datasets)
  expect_warning(suppressMessages(list_census_datasets()), class = "cancensus_datasets_cache_outdated")
  expect_no_warning(list_census_datasets(quiet = TRUE))
  expect_equal(requests, 2)
})

test_that("the list of regions is kept for the session and can be searched", {
  dataset <- "REG1"
  forget(paste0(dataset, "_regions"))
  requests <- 0
  local_mocked_bindings(cm_GET = function(...) {
    requests <<- requests + 1
    csv_response(paste("geo_uid,name,type,population,flag,CMA_UID,CD_UID,PR_UID",
                       "59933,Vancouver,CMA,2642825,B,,,59",
                       "5915022,Vancouver,CSD,662248,CY,59933,5915,59",
                       "59938,Squamish,CMA,24232,K,,,59", sep = "\n"))
  })
  expect_message(regions <- list_census_regions(dataset), class = "cancensus_querying_regions")
  expect_equal(regions$level, c("CMA", "CSD", "CA"))
  expect_equal(regions$pop, c(2642825L, 662248L, 24232L))
  expect_no_message(expect_equal(list_census_regions(dataset), regions))
  expect_equal(requests, 1)

  found <- search_census_regions("vancouver", dataset)
  expect_equal(found$region, c("59933", "5915022"))
  expect_equal(attr(found, "dataset"), dataset)
  expect_equal(search_census_regions("Vancouver", dataset, level = "CSD")$region, "5915022")
  expect_output(expect_warning(search_census_regions("Vancouvr", dataset),
                               class = "cancensus_no_results_similar_regions"),
                "Similar named regions")
  expect_error(search_census_regions("Winnipeg", dataset), class = "cancensus_no_results")
  expect_equal(requests, 1)
})
