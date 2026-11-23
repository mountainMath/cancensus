files_url <- function(year, file) {
  paste0("https://www12.statcan.gc.ca/census-recensement/", year,
         "/geo/sip-pis/boundary-limites/files-fichiers/", file)
}

test_that("boundary file URLs follow Statistics Canada's naming", {
  expect_equal(statcan_geography_url("2021", "PR", "cartographic"), files_url("2021", "lpr_000b21a_e.zip"))
  expect_equal(statcan_geography_url("2021", "CSD", "digital"), files_url("2021", "lcsd000a21a_e.zip"))
  expect_equal(statcan_geography_url("2026", "PR", "cartographic"), files_url("2026", "lpr_000b26a_e.zip"))
  expect_equal(statcan_geography_url("2026", "FED", "digital"), files_url("2026", "lfed000a26a_e.zip"))
  expect_equal(statcan_geography_url("2026", "DA", "cartographic", "generalized"),
               files_url("2026", "lda_000c26a_e.zip"))
  expect_equal(statcan_geography_url("2026", "CAR", "cartographic", "generalized"),
               files_url("2026", "lcar000c26a_e.zip"))
  expect_match(statcan_geography_url("2026", "HR", "digital"), "HR_000a23a_e.zip", fixed = TRUE)
})

test_that("arguments get validated before anything is downloaded", {
  local_mocked_bindings(statcan_download_zip = function(...) stop("Unexpected download"))
  expect_error(get_statcan_geographies("2016", "PR"), class = "cancensus_invalid_census_year")
  expect_error(get_statcan_geographies("2026", "PR", precision = "rough"), class = "cancensus_invalid_precision")
  expect_error(get_statcan_geographies("2021", "PR", precision = "generalized"),
               class = "cancensus_generalized_not_available")
  expect_error(get_statcan_geographies("2026", "PR", type = "digital", precision = "generalized"),
               class = "cancensus_generalized_only_cartographic")
  expect_error(get_statcan_geographies("2026", "HR", precision = "generalized"),
               class = "cancensus_generalized_not_available_hr")
  expect_error(get_statcan_geographies("2021", "CAR"), class = "cancensus_invalid_level_choice")
})

test_that("boundary files that aren't released yet give an error with the release date", {
  local_mocked_bindings(statcan_download_zip = function(...) NULL)
  local_mocked_bindings(Sys.Date = function() as.Date("2026-11-23"), .package = "base")
  expect_error(get_statcan_geographies("2026", "CT"), "2027-02-10", class = "cancensus_geography_not_released")
  expect_error(get_statcan_geographies("2026", "CMACA"), class = "cancensus_geography_not_released")
  expect_error(get_statcan_geographies("2026", "POPCNTR"), class = "cancensus_geography_not_released")
  expect_error(get_statcan_geographies("2026", "FSA"), "2027-09-28")

  with_statcan_cache({
    local_mocked_bindings(Sys.Date = function() as.Date("2027-02-10"), .package = "base")
    expect_null(get_statcan_geographies("2026", "CT", quiet = TRUE))
  })
})

test_that("each year and precision gets its own cache folder", {
  urls <- character(0)
  local_mocked_bindings(statcan_download_zip = function(url, ...) {
    urls <<- c(urls, url)
    NULL
  })
  with_statcan_cache({
    expect_null(get_statcan_geographies("2026", "PR", quiet = TRUE))
    expect_null(get_statcan_geographies("2026", "PR", precision = "generalized", quiet = TRUE))
    expect_null(get_statcan_geographies("2021", "PR", quiet = TRUE))
    geo <- cancensus:::cache_path("geographies")
    expect_true(dir.exists(file.path(geo, "2026", "cartographic")))
    expect_true(dir.exists(file.path(geo, "2026", "cartographic_generalized")))
    expect_true(dir.exists(file.path(geo, "cartographic")))
  })
  expect_equal(basename(urls), c("lpr_000b26a_e.zip", "lpr_000c26a_e.zip", "lpr_000b21a_e.zip"))
})
