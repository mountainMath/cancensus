skip_if_not_installed("duckdb", "1.5.6")
skip_if_not_installed("DBI")

# A small parquet file like the hosted ones, with footer metadata
write_attributes_file <- function(path, uid = "1001", population = 5L) {
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  columns <- paste0('{"DBUID":{"label_en":"Block ID","label_fr":"IDU de l\'\'îlot"},',
                    '"DBPOP2021":{"label_en":"Block population"}}')
  DBI::dbExecute(con, paste0(
    "COPY (SELECT '", uid, "' AS DBUID, ", population, "::INTEGER AS DBPOP2021, 'x' AS NOTE) TO ",
    DBI::dbQuoteString(con, path), " (FORMAT parquet, KV_METADATA {",
    "attribution_en: 'Statistics Canada, a test file', licence_en: 'Test licence', ",
    "title_fr: 'Fichier des attributs géographiques', primary_key: '[\"DBUID\"]', ",
    "columns: '", columns, "'})"))
  path
}

# Serve the file at `source` in place of the hosted file, and fail on any other request
mock_hosted_file <- function(source, env = parent.frame()) {
  local_mocked_bindings(
    fetch_parquet_file = function(url, path, quiet = FALSE) {
      file.copy(source, path, overwrite = TRUE)
      fake_response(200)
    },
    .env = env
  )
}

# Report a failure on any download attempt
mock_no_download <- function(env = parent.frame()) {
  local_mocked_bindings(
    fetch_parquet_file = function(url, path, quiet = FALSE) stop("Unexpected download of ", url),
    .env = env
  )
}

test_that("invalid arguments are errors before anything gets downloaded", {
  with_statcan_cache({
    mock_no_download()
    expect_error(get_statcan_geographic_attributes("1976"), "Only census years 1981, 1986")
    expect_error(get_statcan_geographic_attributes("2022"), "Only census years")
    expect_error(get_statcan_geographic_attributes("2021", language = "de"), "Language must be")
  })
})

test_that("files get downloaded from the expected URL for all census years", {
  with_statcan_cache({
    for (year in seq(1981, 2021, 5)) {
      source <- write_attributes_file(tempfile(fileext = ".parquet"))
      url <- NULL
      local_mocked_bindings(fetch_parquet_file = function(u, path, quiet = FALSE) {
        url <<- u
        file.copy(source, path, overwrite = TRUE)
        fake_response(200)
      })
      suppressMessages(get_statcan_geographic_attributes(year))
      expect_equal(url, paste0("https://data.mountainmath.ca/census/geo_attributes/geo_attributes_",
                               year, ".parquet"))
      unlink(source)
    }
  })
})

test_that("data gets returned with labels and metadata and is cached", {
  with_statcan_cache({
    source <- write_attributes_file(tempfile(fileext = ".parquet"))
    on.exit(unlink(source), add = TRUE)
    mock_hosted_file(source)

    expect_message(result <- get_statcan_geographic_attributes("2021"),
                   "source: Statistics Canada, a test file. Licence: Test licence.", fixed = TRUE)
    expect_s3_class(result, "tbl_df")
    expect_equal(as.vector(result$DBUID), "1001")
    expect_equal(as.vector(result$DBPOP2021), 5L)
    expect_type(result$DBPOP2021, "integer")
    expect_equal(attr(result$DBUID, "label"), "Block ID")
    expect_null(attr(result$NOTE, "label"))

    metadata <- attr(result, "metadata")
    expect_equal(metadata$attribution_en, "Statistics Canada, a test file")
    expect_equal(metadata$primary_key, "DBUID")
    expect_equal(metadata$title_fr, "Fichier des attributs géographiques")
    expect_true(file.exists(cancensus:::cache_path("geo_attributes_2021.parquet")))

    # served from the cache from now on, without the message
    mock_no_download()
    expect_no_message(cached <- get_statcan_geographic_attributes("2021"))
    expect_equal(as.vector(cached$DBUID), "1001")
  })
})

test_that("labels follow the language and fall back to English", {
  with_statcan_cache({
    write_attributes_file(cancensus:::cache_path("geo_attributes_2021.parquet"))
    mock_no_download()

    french <- get_statcan_geographic_attributes("2021", language = "fr")
    expect_equal(attr(french$DBUID, "label"), "IDU de l'îlot")
    expect_equal(attr(french$DBPOP2021, "label"), "Block population")

    rlang::local_options(cancensus.language = "fr")
    expect_equal(attr(get_statcan_geographic_attributes("2021")$DBUID, "label"), "IDU de l'îlot")
  })
})

test_that("a refresh replaces the cached file and reports if it has changed", {
  with_statcan_cache({
    cached <- write_attributes_file(cancensus:::cache_path("geo_attributes_2021.parquet"), uid = "old")

    unchanged <- tempfile(fileext = ".parquet")
    file.copy(cached, unchanged)
    on.exit(unlink(unchanged), add = TRUE)
    mock_hosted_file(unchanged)
    expect_message(get_statcan_geographic_attributes("2021", refresh = TRUE), "has not changed")

    source <- write_attributes_file(tempfile(fileext = ".parquet"), uid = "new")
    on.exit(unlink(source), add = TRUE)
    mock_hosted_file(source)
    expect_message(result <- get_statcan_geographic_attributes("2021", refresh = TRUE), "has changed")
    expect_equal(as.vector(result$DBUID), "new")

    mock_no_download()
    expect_equal(as.vector(get_statcan_geographic_attributes("2021")$DBUID), "new")
  })
})

test_that("failed downloads warn and return NULL, and leave no files behind", {
  with_statcan_cache({
    failures <- list(
      list(function(url, path, quiet = FALSE) fake_response(404), "HTTP status 404"),
      list(function(url, path, quiet = FALSE) stop("Could not resolve host"), "Could not resolve host"),
      list(function(url, path, quiet = FALSE) {
        writeLines("<html>Just a moment...</html>", path)
        fake_response(200)
      }, "did not return a complete parquet file")
    )
    for (failure in failures) {
      local_mocked_bindings(fetch_parquet_file = failure[[1]])
      w <- capture_warnings(result <- get_statcan_geographic_attributes("2021"))
      expect_null(result)
      expect_length(w, 1)
      expect_match(w, failure[[2]], fixed = TRUE)
      expect_match(w, "Returning NULL", fixed = TRUE)
      expect_length(dir(cancensus:::cache_path(), "parquet"), 0)
    }
  })
})

test_that("a truncated download is not accepted", {
  with_statcan_cache({
    source <- write_attributes_file(tempfile(fileext = ".parquet"))
    on.exit(unlink(source), add = TRUE)
    bytes <- readBin(source, "raw", file.size(source))
    local_mocked_bindings(fetch_parquet_file = function(url, path, quiet = FALSE) {
      writeBin(bytes[seq_len(length(bytes) - 10)], path)
      fake_response(200)
    })
    expect_warning(expect_null(get_statcan_geographic_attributes("2021")), "complete parquet file")
  })
})

test_that("a failed refresh returns the cached data with a warning", {
  with_statcan_cache({
    write_attributes_file(cancensus:::cache_path("geo_attributes_2021.parquet"), uid = "old")
    local_mocked_bindings(fetch_parquet_file = function(url, path, quiet = FALSE) fake_response(503))

    w <- capture_warnings(result <- get_statcan_geographic_attributes("2021", refresh = TRUE))
    expect_equal(as.vector(result$DBUID), "old")
    expect_match(w, "HTTP status 503", fixed = TRUE)
    expect_match(w, "Returning the existing cached data instead", fixed = TRUE)
    expect_equal(as.vector(get_statcan_geographic_attributes("2021")$DBUID), "old")
  })
})

test_that("an invalid cached file gets downloaded again", {
  with_statcan_cache({
    writeLines("not parquet", cancensus:::cache_path("geo_attributes_2021.parquet"))
    source <- write_attributes_file(tempfile(fileext = ".parquet"))
    on.exit(unlink(source), add = TRUE)
    mock_hosted_file(source)
    expect_equal(as.vector(suppressMessages(get_statcan_geographic_attributes("2021"))$DBUID), "1001")
  })
})

test_that("the hosted files can be read", {
  skip_on_cran()
  skip_if_offline("data.mountainmath.ca")
  with_statcan_cache({
    result <- suppressMessages(get_statcan_geographic_attributes("1981"))
    expect_equal(nrow(result), 41197)
    expect_false(anyDuplicated(result$EAUID) > 0)
    expect_equal(sum(result$EAPOP1981), 24343181)
    expect_type(result$EAUID, "character")
    expect_match(attr(result, "metadata")$attribution_en, "Statistics Canada")
    expect_false(is.null(attr(result$EAUID, "label")))
  })
})
