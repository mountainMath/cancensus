wds_metadata_xml <- paste0(
  '<?xml version="1.0" encoding="UTF-8"?>',
  '<message:Structure xmlns:message="http://www.sdmx.org/resources/sdmxml/schemas/v2_1/message" ',
  'xmlns:structure="http://www.sdmx.org/resources/sdmxml/schemas/v2_1/structure" ',
  'xmlns:common="http://www.sdmx.org/resources/sdmxml/schemas/v2_1/common">',
  '<message:Structures><structure:Codelists>',
  '<structure:Codelist id="CL_GEO_PR" agencyID="STC_CP">',
  '<common:Name xml:lang="en">Provinces</common:Name><common:Name xml:lang="fr">Provinces</common:Name>',
  '<structure:Code id="2021A000259"><common:Name xml:lang="en">British Columbia</common:Name>',
  '<common:Name xml:lang="fr">Colombie-Britannique</common:Name></structure:Code>',
  '</structure:Codelist></structure:Codelists></message:Structures></message:Structure>')

# Answer requests to the Statistics Canada API with `body`, counting the requests
mock_wds <- function(body, status = 200, env = parent.frame()) {
  requests <- 0
  local_mocked_bindings(
    GET = function(url, ...) {
      requests <<- requests + 1
      if (inherits(body, "condition")) stop(body)
      path <- Filter(function(x) inherits(x, "request"), list(...))
      path <- unlist(lapply(path, function(x) x$output$path))
      writeLines(body, path)
      fake_response(status)
    },
    .package = "httr",
    .env = env
  )
  function() requests
}

test_that("the level of a DGUID is read from its schema, with an error for unknown ones", {
  expect_equal(geo_level_from_DGUID("2021A000259"), "PR")
  expect_equal(geo_level_from_DGUID("2021S0503933"), "CMACA")
  expect_equal(geo_level_from_DGUID("2021S0503933", simple = FALSE), "CMA")
  expect_error(geo_level_from_DGUID("2021A999959"), "2021A999959", class = "cancensus_invalid_dguid")
  expect_error(geo_level_from_DGUID("59"), class = "cancensus_invalid_dguid")
  expect_error(get_statcan_wds_data("not a DGUID"), class = "cancensus_invalid_dguid")
})

test_that("WDS metadata is only cached when it is valid", {
  skip_if_not_installed("xml2")
  version <- basename(tempfile("test_version")) # the cache is per version, each run gets its own
  mock_wds("<html><body>Service unavailable</body></html>", status = 503)
  expect_error(get_statcan_wds_metadata("2021", "PR", version = version), "503",
               class = "cancensus_wds_metadata_unavailable")
  mock_wds("<html><body>Maintenance</body></html>")
  expect_error(get_statcan_wds_metadata("2021", "PR", version = version),
               class = "cancensus_wds_metadata_unavailable")

  requests <- mock_wds(wds_metadata_xml)
  metadata <- get_statcan_wds_metadata("2021", "PR", version = version)
  expect_equal(metadata$en, "British Columbia")
  expect_equal(metadata$fr, "Colombie-Britannique")
  expect_equal(get_statcan_wds_metadata("2021", "PR", version = version), metadata)
  expect_equal(requests(), 1)

  # a failed refresh keeps the cached metadata
  mock_wds("<html></html>", status = 500)
  expect_error(get_statcan_wds_metadata("2021", "PR", version = version, refresh = TRUE),
               class = "cancensus_wds_metadata_unavailable")
  mock_wds(stop("Unexpected download"))
  expect_equal(get_statcan_wds_metadata("2021", "PR", version = version), metadata)
})

test_that("failed WDS data requests don't get cached and certificate problems get explained", {
  version <- basename(tempfile("test_version"))
  request <- function() get_statcan_wds_data("2021A000259", members = "1", version = version)
  mock_wds("Invalid member", status = 404)
  expect_error(request(), class = "cancensus_wds_invalid_request")
  requests <- mock_wds("Invalid member", status = 404)
  expect_error(request(), class = "cancensus_wds_invalid_request")
  expect_equal(requests(), 1)

  mock_wds(simpleError("SSL certificate problem: unable to get local issuer certificate"))
  expect_error(request(), "ssl_verifypeer", class = "cancensus_wds_ssl_error")
  mock_wds(simpleError("Could not resolve host: api.statcan.gc.ca"))
  expect_error(request(), "Could not resolve host")
})
