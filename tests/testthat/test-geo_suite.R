skip_if_not_installed("duckdb", "1.5.6")
skip_if_not_installed("DBI")

# A manifest like the hosted ones, listing a DA and a CSD table
write_manifest_file <- function(path, tables = c("DA", "CSD")) {
  files <- lapply(tables, function(table) list(
    path = paste0(table, ".parquet"), size = 100, etag = "abc", modified = "2026-11-09T00:00:00Z",
    metadata = list(title_en = paste("Table", table), title_fr = if (table == "DA") paste("Tableau", table),
                    primary_key = list(paste0(table, "UID")))
  ))
  files <- c(files, list(list(path = "README.txt", size = 1)))
  jsonlite::write_json(list(directory = TRUE, metadata = list(licence_en = "Test licence"), files = files),
                       path, auto_unbox = TRUE)
  path
}

# Serve files by the last part of the requested URL, recording the requests in `env$urls`
mock_hosted_files <- function(sources, env = parent.frame()) {
  env$urls <- character(0)
  local_mocked_bindings(
    fetch_hosted_file = function(url, path, quiet = FALSE) {
      env$urls <- c(env$urls, url)
      source <- sources[[basename(url)]]
      if (is.null(source)) stop("Unexpected download of ", url)
      file.copy(source, path, overwrite = TRUE)
      fake_response(200)
    },
    .env = env
  )
}

test_that("invalid arguments are errors before anything gets downloaded", {
  with_statcan_cache({
    mock_no_download()
    expect_error(get_statcan_geo_suite("DA", "1991"), "1996, 2001, 2006, 2011, 2016, 2021.*get_statcan_geographic_attributes")
    expect_error(get_statcan_geo_suite("DA", "1996"), "There is no DA table in the 1996 GeoSuite. The 1996 GeoSuite is based on enumeration areas")
    expect_error(get_statcan_geo_suite("POPCTR", "2006"), "There is no POPCTR table in the 2006 GeoSuite. Available tables are ADA|Available tables are CAN")
    expect_error(get_statcan_geo_suite("EA", "2021"), "get_statcan_geo_suite\\(census_year = \"2021\"\\)")
    expect_error(get_statcan_geo_suite(c("DA", "CT")), "single table name")
    expect_error(get_statcan_geo_suite(NA_character_), "single table name")
    expect_error(get_statcan_geo_suite("DA", language = "de"), "Language must be")
  })
})

test_that("tables get downloaded from the expected URL and cached by year", {
  with_statcan_cache({
    source <- write_attributes_file(tempfile(fileext = ".parquet"))
    on.exit(unlink(source), add = TRUE)
    mock_hosted_files(list(DA.parquet = source, CMA_CA.parquet = source, PN.parquet = source,
                           DB.parquet = source, EA.parquet = source))

    suppressMessages(get_statcan_geo_suite("DA", 2016))
    suppressMessages(get_statcan_geo_suite("da", "2021"))
    suppressMessages(get_statcan_geo_suite("CMA", "2021"))
    suppressMessages(get_statcan_geo_suite("CA", "2016"))
    suppressMessages(get_statcan_geo_suite("LOC", "2011"))
    suppressMessages(get_statcan_geo_suite("CB", "2001"))
    suppressMessages(get_statcan_geo_suite("EA", "1996"))
    base <- "https://data.mountainmath.ca/census/geo_suite/"
    expect_equal(urls, paste0(base, c("2016/DA", "2021/DA", "2021/CMA_CA", "2016/CMA_CA", "2011/PN",
                                      "2001/DB", "1996/EA"), ".parquet"))
    expect_true(file.exists(cancensus:::cache_path("geo_suite/2021/CMA_CA.parquet")))
    expect_true(file.exists(cancensus:::cache_path("geo_suite/1996/EA.parquet")))
  })
})

test_that("tables get returned with labels and metadata and are cached", {
  with_statcan_cache({
    source <- write_attributes_file(tempfile(fileext = ".parquet"))
    on.exit(unlink(source), add = TRUE)
    mock_hosted_files(list(DA.parquet = source))

    expect_message(result <- get_statcan_geo_suite("DA", "2021"),
                   "2021 GeoSuite table DA hosted by MountainMath, source: Statistics Canada, a test file. Licence: Test licence.",
                   fixed = TRUE)
    expect_s3_class(result, "tbl_df")
    expect_equal(as.vector(result$DBUID), "1001")
    expect_equal(attr(result$DBUID, "label"), "Block ID")
    expect_equal(attr(result, "metadata")$primary_key, "DBUID")

    mock_no_download()
    expect_no_message(cached <- get_statcan_geo_suite("DA", "2021"))
    expect_equal(as.vector(cached$DBUID), "1001")
    expect_equal(attr(get_statcan_geo_suite("DA", "2021", language = "fr")$DBUID, "label"), "IDU de l'îlot")
  })
})

test_that("a refresh replaces the cached table and a failed one returns the cached data", {
  with_statcan_cache({
    cached <- cancensus:::cache_path("geo_suite/2021/DA.parquet")
    dir.create(dirname(cached), recursive = TRUE)
    write_attributes_file(cached, uid = "old")

    source <- write_attributes_file(tempfile(fileext = ".parquet"), uid = "new")
    on.exit(unlink(source), add = TRUE)
    mock_hosted_files(list(DA.parquet = source))
    expect_message(result <- get_statcan_geo_suite("DA", "2021", refresh = TRUE), "has changed")
    expect_equal(as.vector(result$DBUID), "new")

    local_mocked_bindings(fetch_hosted_file = function(url, path, quiet = FALSE) fake_response(503))
    w <- capture_warnings(result <- get_statcan_geo_suite("DA", "2021", refresh = TRUE))
    expect_equal(as.vector(result$DBUID), "new")
    expect_match(w, "HTTP status 503", fixed = TRUE)
    expect_match(w, "Returning the existing cached data instead", fixed = TRUE)
  })
})

test_that("failed downloads warn and return NULL", {
  with_statcan_cache({
    local_mocked_bindings(fetch_hosted_file = function(url, path, quiet = FALSE) fake_response(404))
    w <- capture_warnings(result <- get_statcan_geo_suite("DA", "2021"))
    expect_null(result)
    expect_match(w, "geo_suite/2021/DA.parquet: the server responded with HTTP status 404. Returning NULL",
                 fixed = TRUE)
    expect_false(file.exists(cancensus:::cache_path("geo_suite/2021/DA.parquet")))

    w <- capture_warnings(result <- get_statcan_geo_suite(census_year = "2021"))
    expect_null(result)
    expect_match(w, "_manifest.json", fixed = TRUE)
  })
})

test_that("without a level the tables of the year get listed from the manifest", {
  with_statcan_cache({
    manifest <- write_manifest_file(tempfile(fileext = ".json"))
    on.exit(unlink(manifest), add = TRUE)
    mock_hosted_files(list(`_manifest.json` = manifest))

    tables <- get_statcan_geo_suite(census_year = "2016", quiet = TRUE)
    expect_equal(urls, "https://data.mountainmath.ca/census/geo_suite/2016/_manifest.json")
    expect_s3_class(tables, "tbl_df")
    expect_equal(tables$table, c("DA", "CSD"))
    expect_equal(tables$title, c("Table DA", "Table CSD"))
    expect_equal(tables$primary_key, c("DAUID", "CSDUID"))
    expect_equal(tables$size, c(100, 100))

    # served from the cache, with French titles where there are any
    mock_no_download()
    french <- get_statcan_geo_suite(census_year = "2016", language = "fr", quiet = TRUE)
    expect_equal(french$title, c("Tableau DA", "Table CSD"))
  })
})

test_that("an invalid manifest is not accepted", {
  with_statcan_cache({
    local_mocked_bindings(fetch_hosted_file = function(url, path, quiet = FALSE) {
      writeLines("<html>Just a moment...</html>", path)
      fake_response(200)
    })
    expect_warning(expect_null(get_statcan_geo_suite(census_year = "2021")), "complete list of tables")
  })
})

test_that("old GeoSuite files are pointed out once, separately from old attribute files", {
  with_statcan_cache({
    cached <- cancensus:::cache_path("geo_suite/2021/DA.parquet")
    dir.create(dirname(cached), recursive = TRUE)
    write_attributes_file(cached)
    mock_no_download()
    old_dir <- file.path(cancensus:::cache_path("geo_suite_2021"), "2021_92-150-X_eng")
    dir.create(old_dir, recursive = TRUE)
    writeBin(raw(2000), file.path(old_dir, "DA.csv"))
    downloads <- cancensus:::statcan_downloads_path()
    dir.create(downloads)
    old_zip <- file.path(downloads, "2021_92-150-X_eng.zip")
    writeBin(raw(1000), old_zip)

    m <- capture_messages(get_statcan_geo_suite("DA", "2021"))
    expect_length(m, 1)
    expect_match(m, "GeoSuite files are no longer used by get_statcan_geo_suite() and take up 3 kB", fixed = TRUE)
    expect_no_message(get_statcan_geo_suite("DA", "2021"))

    command <- trimws(sub(".*\n", "", trimws(m)))
    eval(parse(text = command))
    expect_false(dir.exists(cancensus:::cache_path("geo_suite_2021")))
    expect_false(file.exists(old_zip))
    expect_true(file.exists(cached))
  })
})

test_that("the hosted GeoSuite can be read", {
  skip_on_cran()
  skip_if_offline("data.mountainmath.ca")
  with_statcan_cache({
    for (year in names(cancensus:::geo_suite_tables)) {
      tables <- get_statcan_geo_suite(census_year = year, quiet = TRUE)
      expect_setequal(tables$table, cancensus:::geo_suite_tables[[year]])
    }
    expect_equal(tables$primary_key[tables$table == "CSD"], "CSDUID")

    csd <- get_statcan_geo_suite("CSD", "2021", quiet = TRUE)
    expect_equal(nrow(csd), 5161)
    expect_false(anyDuplicated(csd$CSDUID) > 0)
    expect_type(csd$CSDUID, "character")
    expect_type(csd$CSDPOP2021, "integer")
    expect_type(csd$CSDRPLAT, "double")
    expect_match(attr(csd, "metadata")$attribution_en, "Statistics Canada")
    expect_equal(attr(csd, "metadata")$columns$CSDUID$source_field, "CSDuid")
  })
})
