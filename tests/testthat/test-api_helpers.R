test_that("base URLs are normalized and composed consistently", {
  old_url <- getOption("cancensus.base_url")
  on.exit(options(cancensus.base_url = old_url), add = TRUE)

  options(cancensus.base_url = "https://example.test/")

  expect_equal(cancensus:::cancensus_base_url(), "https://example.test")
  expect_equal(
    cancensus:::cm_api_url("vector_info", "CA21.csv"),
    "https://example.test/api/v1/vector_info/CA21.csv"
  )
  expect_equal(
    cancensus:::cm_url("/data_sets/", "CA21", "/place_names.csv"),
    "https://example.test/data_sets/CA21/place_names.csv"
  )
})

test_that("retry helpers validate attempts and retry transient failures", {
  response_200 <- structure(list(status_code = 200, headers = list()), class = "response")
  response_500 <- structure(list(status_code = 500, headers = list()), class = "response")

  attempts <- 0
  result <- cancensus:::retry_api_call(
    function() {
      attempts <<- attempts + 1
      if (attempts == 1) response_500 else response_200
    },
    max_attempts = 2,
    quiet = TRUE,
    sleep_fn = function(seconds) NULL
  )

  expect_identical(result, response_200)
  expect_equal(attempts, 2)
  expect_error(
    cancensus:::perform_api_call(function() response_200, retry = -1),
    "`retry`"
  )
  expect_error(
    cancensus:::retry_api_call(function() response_200, max_attempts = 1.5),
    "`max_attempts`"
  )
  expect_error(
    cancensus:::perform_api_call(function() response_200, retry = Inf),
    "`retry`"
  )
})

test_that("quiet cache reads tolerate missing last_updated metadata", {
  dataset_cache <- file.path(tempdir(), "cancensus_datasets.rda")
  regions_cache <- file.path(tempdir(), "CA21_regions.rda")
  on.exit(unlink(c(dataset_cache, regions_cache)), add = TRUE)

  result <- data.frame(
    dataset = "CA21",
    description = "2021 Census",
    geo_dataset = "CA21",
    attribution = "Statistics Canada 2021",
    reference = "2021",
    reference_url = "https://example.test",
    stringsAsFactors = FALSE
  )
  save(result, file = dataset_cache)

  expect_silent(datasets <- list_census_datasets(quiet = TRUE))
  expect_equal(datasets$dataset, "CA21")

  result <- data.frame(
    region = "59933",
    name = "Vancouver",
    level = "CMA",
    pop = 1L,
    municipal_status = "K",
    CMA_UID = NA_character_,
    CD_UID = NA_character_,
    PR_UID = "59",
    stringsAsFactors = FALSE
  )
  save(result, file = regions_cache)

  expect_silent(regions <- list_census_regions("CA21", quiet = TRUE))
  expect_equal(regions$level, "CA")
})

test_that("metadata endpoints respect custom base URLs", {
  old_url <- getOption("cancensus.base_url")
  on.exit(options(cancensus.base_url = old_url), add = TRUE)
  options(cancensus.base_url = "https://example.test/")

  metadata_caches <- file.path(tempdir(), c("cancensus_datasets.rda", "TEST_vectors.rda", "TEST_regions.rda"))
  unlink(metadata_caches)
  on.exit(unlink(metadata_caches), add = TRUE)

  response_200 <- structure(list(status_code = 200, headers = list()), class = "response")
  captured_urls <- character()

  with_mocked_bindings(
    GET = function(url, ...) {
      captured_urls <<- c(captured_urls, url)
      response_200
    },
    content = function(x, type = NULL, encoding = NULL, ...) {
      if (grepl("list_datasets$", captured_urls[length(captured_urls)])) {
        return('[{"dataset":"TEST","description":"Test","geo_dataset":"TEST","attribution":"Test","reference":"Test","reference_url":"https://example.test"}]')
      }
      if (grepl("vector_info/TEST.csv$", captured_urls[length(captured_urls)])) {
        return("vector,type,label,units,parent,add,details\nv_TEST_1,Total,Root,1,,1,Root")
      }
      "geo_uid,name,type,population,flag,CMA_UID,CD_UID,PR_UID\n1,Test,CMA,1,K,,,59"
    },
    .package = "httr",
    {
      expect_silent(list_census_datasets(use_cache = FALSE, quiet = TRUE))
      expect_silent(list_census_vectors("TEST", use_cache = FALSE, quiet = TRUE))
      expect_silent(list_census_regions("TEST", use_cache = FALSE, quiet = TRUE))
    }
  )

  expect_true("https://example.test/api/v1/list_datasets" %in% captured_urls)
  expect_true("https://example.test/api/v1/vector_info/TEST.csv" %in% captured_urls)
  expect_true("https://example.test/data_sets/TEST/place_names.csv" %in% captured_urls)
})
