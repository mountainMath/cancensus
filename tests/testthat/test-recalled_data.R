mock_cached_data <- function() {
  dplyr::tibble(
    path = c("CM_data_a.rda", "CM_data_b.rda", "CM_geo_c.geojson", "CM_geo_d.geojson"),
    dataset = c("CA21", "CA21", "CA21", "CA16"),
    regions = '{"CMA":["59933"]}',
    level = c("CT", "CT", "CT", "CT"),
    vectors = c('["v_CA21_1","v_CA21_2"]', '["v_CA21_10"]', NA, NA),
    version = c("d.1", "d.1", "g.1", "g.1")
  )
}

mock_recall_database <- function() {
  dplyr::tibble(
    api_version = c("d.1", "g.1"),
    dataset = c("CA21", "CA21"),
    level = c(NA_character_, "CT"),
    vector = c("v_CA21_1", NA_character_)
  )
}

recalled_paths <- function(recall_database, cached_data = mock_cached_data()) {
  local_mocked_bindings(
    get_recalled_database = function(...) recall_database
  )
  list_recalled_cached_data(cached_data)$path
}

test_that("recalled data and geometry cache entries are matched", {
  expect_setequal(recalled_paths(mock_recall_database()),
                  c("CM_data_a.rda", "CM_geo_c.geojson"))
})

test_that("recall database with only data recalls does not error", {
  recalls <- mock_recall_database()[1, ]
  expect_equal(recalled_paths(recalls), "CM_data_a.rda")
})

test_that("recall database with only geometry recalls does not error", {
  recalls <- mock_recall_database()[2, ]
  expect_equal(recalled_paths(recalls), "CM_geo_c.geojson")
})

test_that("empty recall database gives no recalled cache entries", {
  recalls <- mock_recall_database()[0, ]
  result <- {
    local_mocked_bindings(get_recalled_database = function(...) recalls)
    list_recalled_cached_data(mock_cached_data())
  }
  expect_equal(nrow(result), 0)
  expect_true(all(c("path", "dataset", "level", "version") %in% names(result)))
})

test_that("recalls leave an empty cache alone", {
  expect_length(recalled_paths(mock_recall_database(), mock_cached_data()[0, ]), 0)
  expect_length(recalled_paths(mock_recall_database()[1, ], mock_cached_data()[0, ]), 0)
})

test_that("failed downloads of the recall database warn with the reason and keep the cached copy", {
  path <- tempfile(fileext = ".csv")
  writeLines(c("dataset,level,vector", "CA21,,v_CA21_1"), path)
  local_mocked_bindings(recall_data_path = function() path)

  local_mocked_bindings(cm_GET = function(...) fake_response(500))
  expect_warning(result <- get_recalled_database(refresh = TRUE), "HTTP status 500",
                 class = "cancensus_recall_database_unavailable")
  expect_null(result)
  expect_equal(readLines(path)[2], "CA21,,v_CA21_1")

  local_mocked_bindings(cm_GET = function(...) stop("Could not resolve host"))
  expect_warning(get_recalled_database(refresh = TRUE), "Could not resolve host",
                 class = "cancensus_recall_database_unavailable")
  expect_equal(readLines(path)[2], "CA21,,v_CA21_1")

  # without a refresh the cached copy gets read
  expect_equal(get_recalled_database()$vector, "v_CA21_1")
})

test_that("a failed download of the recall database is not attempted again in the session", {
  path <- tempfile(fileext = ".csv")
  local_mocked_bindings(recall_data_path = function() path)
  attempts <- 0
  local_mocked_bindings(cm_GET = function(...) {
    attempts <<- attempts + 1
    stop("Could not resolve host")
  })
  expect_warning(expect_null(get_recalled_database()), class = "cancensus_recall_database_unavailable")
  expect_no_warning(expect_null(get_recalled_database()))
  expect_no_warning(expect_null(list_recalled_cached_data(mock_cached_data(), warn_only_once = TRUE)))
  expect_equal(attempts, 1)

  # a refresh tries again, and a successful one clears the failure
  expect_warning(get_recalled_database(refresh = TRUE), class = "cancensus_recall_database_unavailable")
  expect_equal(attempts, 2)
  local_mocked_bindings(cm_GET = function(url, ...) {
    attempts <<- attempts + 1
    writeLines(c("api_version,dataset,level,vector", "d.1,CA21,,v_CA21_1"), list(...)[[1]]$output$path)
    fake_response(200)
  })
  expect_equal(get_recalled_database(refresh = TRUE)$vector, "v_CA21_1")
  unlink(path)
  expect_equal(get_recalled_database()$vector, "v_CA21_1")
  expect_equal(attempts, 4)
})

test_that("the recall database is parsed once per session", {
  path <- tempfile(fileext = ".csv")
  writeLines(c("api_version,dataset,level,vector", "d.1,CA21,,v_CA21_1"), path)
  local_mocked_bindings(recall_data_path = function() path)
  reads <- 0
  local_mocked_bindings(read_csv = function(...) {
    reads <<- reads + 1
    dplyr::tibble(api_version = "d.1", dataset = "CA21", level = NA_character_, vector = "v_CA21_1")
  }, .package = "readr")
  for (i in 1:3) expect_equal(get_recalled_database()$vector, "v_CA21_1")
  expect_equal(reads, 1)
})

test_that("recall versions are compared as numbers", {
  cached <- mock_cached_data()
  cached$version <- c("d.3", "d.10", "g.2", "g.1")
  recalls <- mock_recall_database()
  recalls$vector[1] <- "v_CA21_10"
  # data cached at version 10 is not affected by a recall of version 3, and the other way around it is
  recalls$api_version <- c("d.3", "g.10")
  expect_equal(recalled_paths(recalls, cached), "CM_geo_c.geojson")
  recalls$api_version <- c("d.10", "g.1")
  expect_equal(recalled_paths(recalls, cached), "CM_data_b.rda")
  expect_true(recall_version("d.10") > recall_version("d.3"))
  expect_true(is.na(recall_version(NA_character_)))
})
