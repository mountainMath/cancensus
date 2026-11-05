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
