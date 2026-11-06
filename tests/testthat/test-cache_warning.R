# Evaluate code with no cache path set and the warning not yet shown in this "session"
with_no_cache_path <- function(code) {
  old_env <- Sys.getenv("CM_CACHE_PATH")
  old_option <- options(cancensus.cache_path = NULL)
  old_shown <- session_cache_get("cache_warning_shown")
  Sys.unsetenv("CM_CACHE_PATH")
  session_cache_set("cache_warning_shown", FALSE)
  on.exit({
    Sys.setenv(CM_CACHE_PATH = old_env)
    options(old_option)
    session_cache_set("cache_warning_shown", isTRUE(old_shown))
  })
  force(code)
}

reset_warning <- function() session_cache_set("cache_warning_shown", FALSE)

# Mock the CensusMapper API, keeping track of the number of requests
mock_api <- function(env = parent.frame()) {
  calls <- new.env()
  calls$n <- 0L
  # keep deprecation warnings from dependencies out of the warning counts
  rlang::local_options(lifecycle_verbosity = "quiet", .frame = env)
  local_mocked_bindings(
    first_run_checks = function() invisible(NULL),
    check_recalled_data_and_warn = function(...) invisible(NULL),
    handle_cm_status_code = function(...) invisible(NULL),
    cm_POST = function(url, ...) {
      calls$n <- calls$n + 1L
      body <- if (grepl("geo.geojson", url, fixed = TRUE)) {
        paste0('{"type":"FeatureCollection","features":[{"type":"Feature",',
               '"properties":{"id":"5915022","t":"CSD"},',
               '"geometry":{"type":"Point","coordinates":[-123,49]}}]}')
      } else {
        "GeoUID,Type,Region Name,v_CA16_408: Total\n5915022,CSD,Vancouver,1\n"
      }
      structure(list(status_code = 200L, content = charToRaw(body),
                     headers = httr::insensitive(list(`data-version` = "d.1", `content-type` = "text/plain"))),
                class = "response")
    },
    .env = env
  )
  calls
}

# Each query needs its own region to not hit data cached by previous tests in tempdir()
new_region <- function() list(CSD = as.character(sample(1e6:9e6, 1)))

query <- function(regions = new_region(), ...) {
  suppressMessages(get_census("CA16", regions = regions, level = "CSD", vectors = "v_CA16_408",
                              api_key = "CensusMapper_test", ...))
}

test_that("looking up the cache path does not notify", {
  with_no_cache_path({
    expect_silent(cancensus:::cache_path())
    expect_silent(cancensus:::cache_path("some_file"))
    expect_silent(list_cancensus_cache())
    expect_silent(remove_from_cancensus_cache(character(0)))
  })
})

test_that("downloading both data and geometry warns once", {
  with_no_cache_path({
    calls <- mock_api()
    w <- capture_warnings(query(geo_format = "sf"))
    expect_equal(calls$n, 2)
    expect_length(w, 1)
    expect_match(w, "No persistent cache path is set", fixed = TRUE)
    expect_match(w, "set_cancensus_cache_path", fixed = TRUE)
  })
})

test_that("the warning shows once per session across downloads", {
  with_no_cache_path({
    calls <- mock_api()
    expect_warning(query(), "No persistent cache path")
    expect_no_warning(query())
    expect_equal(calls$n, 2)
  })
})

test_that("reading from the session cache does not warn", {
  with_no_cache_path({
    calls <- mock_api()
    region <- new_region()
    expect_warning(query(region), "No persistent cache path")
    reset_warning()
    expect_no_warning(query(region))
    expect_equal(calls$n, 1)
  })
})

test_that("quiet = TRUE suppresses the warning without using it up", {
  with_no_cache_path({
    mock_api()
    expect_no_warning(query(quiet = TRUE))
    expect_warning(query(), "No persistent cache path")
  })
})

test_that("a failed request does not use up the warning", {
  with_no_cache_path({
    mock_api()
    local_mocked_bindings(handle_cm_status_code = function(...) stop("API error"))
    expect_error(expect_no_warning(query()), "API error")
    expect_false(isTRUE(session_cache_get("cache_warning_shown")))
  })
})

test_that("get_intersecting_geometries warns when downloading", {
  with_no_cache_path({
    local_mocked_bindings(
      handle_cm_status_code = function(...) invisible(NULL),
      cm_POST = function(...) {
        structure(list(status_code = 200L, content = charToRaw('{"CSD":["5915022"]}'),
                       headers = httr::insensitive(list(`data-version` = "d.1", `content-type` = "text/plain"))),
                  class = "response")
      }
    )
    geometry <- sf::st_sfc(sf::st_point(c(-123 + stats::runif(1), 49)), crs = 4326)
    expect_warning(suppressMessages(get_intersecting_geometries("CA16", "CSD", geometry,
                                                                api_key = "CensusMapper_test")),
                   "No persistent cache path")
  })
})

test_that("StatCan downloads warn, unless the data goes to an explicit location", {
  with_no_cache_path({
    local_mocked_bindings(GET = function(...) {
      structure(list(status_code = 200L, headers = httr::insensitive(list())), class = "response")
    }, .package = "httr")
    local_mocked_bindings(statcan_is_zip = function(...) TRUE)
    url <- "https://example.com/some_file.zip"
    expect_no_warning(statcan_download_zip(url, base_path = tempfile(), quiet = TRUE))
    expect_warning(statcan_download_zip(url, quiet = FALSE), "No persistent cache path")
  })
})

test_that("no warning with a persistent cache path", {
  cache <- tempfile("cancensus_test_cache")
  on.exit(unlink(cache, recursive = TRUE))
  with_no_cache_path({
    mock_api()
    Sys.setenv(CM_CACHE_PATH = cache)
    expect_no_warning(query())
    Sys.unsetenv("CM_CACHE_PATH")
    options(cancensus.cache_path = cache)
    expect_no_warning(query())
  })
})

test_that("a session cache path overrides .Renviron and is used by all cache functions", {
  first <- tempfile("cancensus_test_cache")
  second <- tempfile("cancensus_test_cache")
  on.exit(unlink(c(first, second), recursive = TRUE))
  with_no_cache_path({
    mock_api()
    Sys.setenv(CM_CACHE_PATH = first)
    expect_message(set_cancensus_cache_path(second), second, fixed = TRUE)
    expect_equal(show_cancensus_cache_path(), second)
    query()
    expect_equal(nrow(list_cancensus_cache()), 1)
    expect_length(dir(second, "^CM_data_.*\\.rda$"), 1)
    expect_false(dir.exists(first))
  })
})

test_that("show_cancensus_cache_path reports the cache path in use", {
  with_no_cache_path({
    expect_message(expect_null(show_cancensus_cache_path()), "No cache path set")
    options(cancensus.cache_path = "from_option")
    expect_equal(show_cancensus_cache_path(), "from_option")
    Sys.setenv(CM_CACHE_PATH = "from_env")
    expect_equal(show_cancensus_cache_path(), "from_env")
  })
})

test_that("the corrupt cache path error is clean", {
  with_no_cache_path({
    options(cancensus.cache_path = 123)
    expect_error(cancensus:::cache_path(), "Must be a path\\.$")
  })
})
