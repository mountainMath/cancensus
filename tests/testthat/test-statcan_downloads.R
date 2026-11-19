# These tests use get_statcan_geography_relationships() for the StatCan download fallback, without its
# deprecation warning, which gets tested separately below
session_cache_set("geography_relationships_deprecated", TRUE)

blocked_response <- function() {
  fake_response(403, list(`cf-mitigated` = "challenge", server = "cloudflare"))
}

# Mock the StatCan server, any request fails the test unless a response is given
mock_statcan <- function(response = NULL, env = parent.frame()) {
  local_mocked_bindings(
    GET = function(url, config = list(), ...) {
      if (is.null(response)) stop("Unexpected download of ", url)
      response
    },
    .package = "httr",
    .env = env
  )
}

# Create a zip at `zip_path` from the named list of file paths and their contents
make_zip <- function(zip_path, files) {
  skip_if(Sys.which("zip") == "", "zip utility not available")
  src <- tempfile("zip_src")
  dir.create(src)
  on.exit(unlink(src, recursive = TRUE))
  for (name in names(files)) {
    dir.create(dirname(file.path(src, name)), recursive = TRUE, showWarnings = FALSE)
    writeLines(files[[name]], file.path(src, name))
  }
  dir.create(dirname(zip_path), recursive = TRUE, showWarnings = FALSE)
  old_wd <- setwd(src)
  on.exit(setwd(old_wd), add = TRUE, after = FALSE)
  utils::zip(zip_path, names(files), flags = "-q")
}

deposit <- function(file_name) file.path(cancensus:::cache_path("statcan_downloads"), file_name)

relationships_zip <- "2021_98260004.zip"
relationships_url <- paste0("https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/",
                            "dguid-idugd/files-fichiers/", relationships_zip)


test_that("Cloudflare blocks are detected and other failures are not", {
  expect_true(statcan_is_cloudflare_block(blocked_response()))
  expect_true(statcan_is_cloudflare_block(fake_response(403, list(server = "cloudflare"))))
  expect_true(statcan_is_cloudflare_block(fake_response(403, list(Server = "Cloudflare"))))
  expect_false(statcan_is_cloudflare_block(fake_response(403, list(server = "nginx"))))
  expect_false(statcan_is_cloudflare_block(fake_response(403)))
  expect_false(statcan_is_cloudflare_block(fake_response(404, list(server = "cloudflare"))))
  expect_false(statcan_is_cloudflare_block(fake_response(200, list(server = "cloudflare"))))
})

test_that("blocked downloads warn with the link and target location and return NULL", {
  with_statcan_cache({
    mock_statcan(blocked_response())
    expected_file <- deposit(relationships_zip)

    w <- capture_warnings(result <- get_statcan_geography_relationships("2021"))
    expect_null(result)
    expect_length(w, 1)
    expect_match(w, "blocked", fixed = TRUE)
    expect_match(w, relationships_url, fixed = TRUE)
    expect_match(w, statcan_display_path(expected_file), fixed = TRUE)

    # the folder is ready for the file, and the failure left no cached file behind
    expect_true(dir.exists(dirname(expected_file)))
    expect_false(file.exists(cancensus:::cache_path("geography_relationship_2021.zip")))
  })
})

test_that("all StatCan download functions fail gracefully when blocked", {
  with_statcan_cache({
    mock_statcan(blocked_response())
    expect_warning(expect_null(get_statcan_geography_relationships("2021")),
                   "2021_98260004.zip", fixed = TRUE, class = "cancensus_statcan_blocked")
    expect_warning(expect_null(get_statcan_geographies("2021", "PR", quiet = TRUE)),
                   "lpr_000b21a_e.zip", fixed = TRUE, class = "cancensus_statcan_blocked")
  })
})

test_that("the warning points out when the location does not persist across sessions", {
  with_statcan_cache({
    mock_statcan(blocked_response())
    w <- capture_warnings(get_statcan_geography_relationships("2021"))
    expect_match(w, "blocked")
    expect_no_match(w, "persistent")
  })
  old_env <- Sys.getenv("CM_CACHE_PATH")
  old_option <- options(cancensus.cache_path = NULL)
  Sys.unsetenv("CM_CACHE_PATH")
  on.exit({
    Sys.setenv(CM_CACHE_PATH = old_env)
    options(old_option)
  })
  mock_statcan(blocked_response())
  expect_warning(suppressMessages(get_statcan_geography_relationships("2021")),
                 "No persistent cache path is set")
})

test_that("get_statcan_geographies uses the cache_path argument for the target location", {
  with_statcan_cache({
    mock_statcan(blocked_response())
    custom <- tempfile("cancensus_custom")
    on.exit(unlink(custom, recursive = TRUE))
    expect_warning(get_statcan_geographies("2021", "PR", cache_path = custom, quiet = TRUE),
                   statcan_display_path(file.path(custom, "statcan_downloads", "lpr_000b21a_e.zip")),
                   fixed = TRUE)
  })
})

test_that("manually downloaded geography relationship files get picked up", {
  with_statcan_cache({
    mock_statcan()
    make_zip(deposit("2021_98260004.zip"), list("rel.csv" = c("DGUID", "abc")))

    expect_equal(get_statcan_geography_relationships("2021")$DGUID, "abc")
    expect_false(file.exists(deposit("2021_98260004.zip")))
    expect_true(file.exists(cancensus:::cache_path("geography_relationship_2021.zip")))
    expect_equal(get_statcan_geography_relationships("2021")$DGUID, "abc")
  })
})

test_that("manually downloaded geographic boundary files get picked up", {
  skip_if(Sys.which("zip") == "", "zip utility not available")
  with_statcan_cache({
    mock_statcan()
    shp_dir <- tempfile("shp")
    dir.create(shp_dir)
    on.exit(unlink(shp_dir, recursive = TRUE))
    geo <- sf::st_sf(PRUID = "59", geometry = sf::st_sfc(sf::st_point(c(1, 2)), crs = 4326))
    sf::st_write(geo, file.path(shp_dir, "lpr.shp"), quiet = TRUE)

    target <- deposit("lpr_000b21a_e.zip")
    dir.create(dirname(target), recursive = TRUE)
    utils::zip(target, dir(shp_dir, full.names = TRUE), flags = "-q -j")

    result <- get_statcan_geographies("2021", "PR", quiet = TRUE)
    expect_s3_class(result, "sf")
    expect_equal(result$PRUID, "59")
    expect_false(file.exists(target))
  })
})

test_that("a blocked refresh returns the cached data with a warning, and a deposited file refreshes it", {
  with_statcan_cache({
    make_zip(deposit(relationships_zip), list("rel.csv" = c("DGUID", "old")))
    mock_statcan()
    get_statcan_geography_relationships("2021")

    mock_statcan(blocked_response())
    w <- capture_warnings(result <- get_statcan_geography_relationships("2021", refresh = TRUE))
    expect_equal(result$DGUID, "old")
    expect_length(w, 1)
    expect_match(w, "blocked", fixed = TRUE)
    expect_match(w, "Returning the existing cached data instead", fixed = TRUE)
    expect_match(w, "with `refresh = TRUE`", fixed = TRUE)
    expect_no_match(w, "Returning NULL", fixed = TRUE)
    expect_equal(get_statcan_geography_relationships("2021")$DGUID, "old")

    make_zip(deposit(relationships_zip), list("rel.csv" = c("DGUID", "new")))
    expect_equal(get_statcan_geography_relationships("2021", refresh = TRUE)$DGUID, "new")
    expect_false(file.exists(deposit(relationships_zip)))
  })
})

test_that("a blocked refresh without cached data returns NULL", {
  with_statcan_cache({
    mock_statcan(blocked_response())
    w <- capture_warnings(result <- get_statcan_geography_relationships("2021", refresh = TRUE))
    expect_null(result)
    expect_match(w, "Returning NULL", fixed = TRUE)
    expect_no_match(w, "refresh = TRUE", fixed = TRUE)
  })
})

test_that("blocked refreshes return the cached data for the other StatCan functions", {
  with_statcan_cache({
    mock_statcan()
    make_zip(deposit("2021_98260004.zip"), list("rel.csv" = c("DGUID", "abc")))
    get_statcan_geography_relationships("2021")

    shp_dir <- tempfile("shp")
    dir.create(shp_dir)
    on.exit(unlink(shp_dir, recursive = TRUE))
    geo <- sf::st_sf(PRUID = "59", geometry = sf::st_sfc(sf::st_point(c(1, 2)), crs = 4326))
    sf::st_write(geo, file.path(shp_dir, "lpr.shp"), quiet = TRUE)
    utils::zip(deposit("lpr_000b21a_e.zip"), dir(shp_dir, full.names = TRUE), flags = "-q -j")
    get_statcan_geographies("2021", "PR", quiet = TRUE)

    mock_statcan(blocked_response())
    expect_warning(rel <- get_statcan_geography_relationships("2021", refresh = TRUE),
                   "Returning the existing cached data instead")
    expect_equal(rel$DGUID, "abc")
    expect_warning(geos <- get_statcan_geographies("2021", "PR", refresh = TRUE, quiet = TRUE),
                   "Returning the existing cached data instead")
    expect_equal(geos$PRUID, "59")
  })
})

test_that("an invalid deposited file during a refresh falls back on the cached data", {
  with_statcan_cache({
    mock_statcan()
    make_zip(deposit(relationships_zip), list("rel.csv" = c("DGUID", "old")))
    get_statcan_geography_relationships("2021")

    writeLines("<html>Just a moment...</html>", deposit(relationships_zip))
    expect_warning(result <- get_statcan_geography_relationships("2021", refresh = TRUE),
                   "not a valid zip file.*Returning the existing cached data instead")
    expect_equal(result$DGUID, "old")
    expect_true(file.exists(deposit(relationships_zip)))
  })
})

test_that("invalid manually downloaded files produce a warning and are left in place", {
  with_statcan_cache({
    mock_statcan()
    target <- deposit(relationships_zip)
    dir.create(dirname(target), recursive = TRUE)
    writeLines("<html>Just a moment...</html>", target)

    expect_warning(expect_null(get_statcan_geography_relationships("2021")),
                   "not a valid zip file")
    expect_true(file.exists(target))
  })
})

test_that("other download failures remain errors", {
  with_statcan_cache({
    mock_statcan(fake_response(404, list(server = "nginx")))
    expect_error(get_statcan_geography_relationships("2021"), "HTTP status 404")

    # the mock doesn't write a file, same as a response that isn't a zip file
    mock_statcan(fake_response(200))
    expect_error(get_statcan_geography_relationships("2021"), "did not return a zip file")
  })
})

test_that("URL query parameters are not part of the expected file name", {
  with_statcan_cache({
    mock_statcan(blocked_response())
    expect_warning(
      statcan_download_zip("https://example.com/path/file_a.zip?st=abc"),
      paste0(statcan_display_path(deposit("file_a.zip")), "\n"),
      fixed = TRUE
    )
  })
})

test_that("the Safari note only shows on macOS", {
  message_on <- function(os) {
    local_mocked_bindings(statcan_os = function() os)
    statcan_blocked_message("https://example.com/a.zip", "/cache/statcan_downloads/a.zip", TRUE)
  }
  expect_match(message_on("Darwin"), "Safari unzips downloads automatically", fixed = TRUE)
  expect_no_match(message_on("Windows"), "Safari", fixed = TRUE)
  expect_no_match(message_on("Linux"), "Safari", fixed = TRUE)
})

test_that("paths in messages use native separators on Windows and expand the home directory", {
  local_mocked_bindings(statcan_os = function() "Windows")
  expect_equal(statcan_display_path("C:/Users/me/cache\\statcan_downloads/a.zip"),
               "C:\\Users\\me\\cache\\statcan_downloads\\a.zip")
  w <- statcan_blocked_message("https://example.com/a.zip", "C:/cache/statcan_downloads/a.zip", TRUE)
  expect_match(w, "C:\\cache\\statcan_downloads\\a.zip", fixed = TRUE)
  # URLs keep their forward slashes
  expect_match(w, "https://example.com/a.zip", fixed = TRUE)

  local_mocked_bindings(statcan_os = function() "Linux")
  expect_equal(statcan_display_path("/cache/a.zip"), "/cache/a.zip")
  expect_equal(statcan_display_path("~/a.zip"), path.expand("~/a.zip"))
})

test_that("an unzipped copy of the file is pointed out when the download is blocked", {
  with_statcan_cache({
    mock_statcan(blocked_response())
    w <- capture_warnings(get_statcan_geography_relationships("2021"))
    expect_no_match(w, "unzipped copy", fixed = TRUE)

    unzipped <- sub("\\.zip$", "", deposit(relationships_zip))
    dir.create(unzipped, recursive = TRUE)
    writeLines("DGUID", file.path(unzipped, "rel.csv"))

    w <- capture_warnings(result <- get_statcan_geography_relationships("2021"))
    expect_null(result)
    expect_match(w, "unzipped copy", fixed = TRUE)
    expect_match(w, statcan_display_path(unzipped), fixed = TRUE)
  })
})

test_that("get_statcan_geography_relationships() warns once per session that it is deprecated", {
  with_statcan_cache({
    file_path <- cancensus:::cache_path("geography_relationship_2021.zip")
    make_zip(file_path, list("2021_98260004.csv" = c("DGUID", "abc")))
    mock_statcan()
    session_cache_set("geography_relationships_deprecated", FALSE)
    on.exit(session_cache_set("geography_relationships_deprecated", TRUE), add = TRUE)

    expect_warning(result <- get_statcan_geography_relationships("2021"),
                   "deprecated and will be removed in the next release.*get_statcan_geographic_attributes")
    expect_equal(result$DGUID, "abc")
    expect_no_warning(get_statcan_geography_relationships("2021"))
  })
})

test_that("boundary files zipped inside a folder get unpacked without leaving a folder behind", {
  skip_if(Sys.which("zip") == "", "zip utility not available")
  with_statcan_cache({
    mock_statcan()
    src <- tempfile("shp")
    dir.create(file.path(src, "lpr_000b21a_e"), recursive = TRUE)
    on.exit(unlink(src, recursive = TRUE))
    geo <- sf::st_sf(PRUID = "59", geometry = sf::st_sfc(sf::st_point(c(1, 2)), crs = 4326))
    sf::st_write(geo, file.path(src, "lpr_000b21a_e", "lpr.shp"), quiet = TRUE)

    target <- deposit("lpr_000b21a_e.zip")
    dir.create(dirname(target), recursive = TRUE)
    old_wd <- setwd(src)
    on.exit(setwd(old_wd), add = TRUE, after = FALSE)
    utils::zip(target, "lpr_000b21a_e", flags = "-q -r")

    expect_equal(get_statcan_geographies("2021", "PR", quiet = TRUE)$PRUID, "59")
    expect_equal(dir(cancensus:::cache_path("geographies/cartographic")), "PR")
  })
})
