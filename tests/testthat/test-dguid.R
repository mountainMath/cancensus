# DGUIDs in CensusMapper API responses (#238): a DGUID column in data.csv right after GeoUID, and a dguid
# property in geo.geojson. Data downloaded before, which can come from the cache, doesn't have them.

dguid_response <- function(body) {
  structure(list(status_code = 200L, content = charToRaw(enc2utf8(body)),
                 headers = httr::insensitive(list(`data-version` = "d.1", `content-type` = "text/plain"))),
            class = "response")
}

# Census tracts, one with a DGUID and one without, like the CA1116 regions
dguid_data_csv <- function(with_dguid = TRUE, missing = "") {
  if (with_dguid) {
    paste0("GeoUID,DGUID,Type,Region Name,Area (sq km),Population,Dwellings,Households,v_X_1: Total\n",
           "9330001.01,2021S05079330001.01,CT,0001.01,1.5,100,40,38,10\n",
           "9330001.02,", missing, ",CT,0001.02,2.5,200,80,76,20\n")
  } else {
    paste0("GeoUID,Type,Region Name,Area (sq km),Population,Dwellings,Households,v_X_1: Total\n",
           "9330001.01,CT,0001.01,1.5,100,40,38,10\n",
           "9330001.02,CT,0001.02,2.5,200,80,76,20\n")
  }
}

# The dguid property comes last here, the API doesn't guarantee the order of properties
dguid_geo_json <- function(with_dguid = TRUE) {
  feature <- function(id, dguid, x) {
    paste0('{"type":"Feature","properties":{"id":"', id, '","t":"CT","a":"1.5","pop":"100","dw":"40","hh":"38",',
           '"name":"', id, '","rpid":"59933"', if (with_dguid) paste0(',"dguid":', dguid), '},',
           '"geometry":{"type":"Polygon","coordinates":[[[', x, ',0],[', x + 1, ',0],[', x + 1, ',1],[', x, ',0]]]}}')
  }
  paste0('{"type":"FeatureCollection","features":[',
         feature("9330001.01", '"2021S05079330001.01"', 0), ",", feature("9330001.02", "null", 1), "]}")
}

local_dguid_api <- function(data = dguid_data_csv(), geo = dguid_geo_json(), env = parent.frame()) {
  cache_dir <- tempfile("cancensus_dguid_cache")
  dir.create(cache_dir)
  local_mocked_bindings(
    first_run_checks = function() invisible(NULL),
    cache_path = function(...) file.path(cache_dir, paste0(...)),
    warn_if_cache_not_persistent = function(...) invisible(NULL),
    cm_POST = function(url, ...) dguid_response(if (grepl("geo.geojson", url, fixed = TRUE)) geo else data),
    .env = env
  )
}

dguid_census <- function(...) {
  get_census("CA21", regions = list(CMA = "59933"), level = "CT", api_key = "CensusMapper_test", quiet = TRUE, ...)
}

expected_dguids <- c("2021S05079330001.01", NA)

test_that("DGUID stays character right after GeoUID, and regions without one get NA", {
  local_dguid_api()
  for (labels in c("detailed", "short", "labelled")) {
    result <- dguid_census(vectors = "v_X_1", labels = labels)
    expect_type(result$DGUID, "character")
    expect_equal(as.vector(result$DGUID), expected_dguids)
    expect_equal(match("DGUID", names(result)), match("GeoUID", names(result)) + 1)
  }
  # also for data without vectors
  expect_equal(dguid_census(labels = "detailed")$DGUID, expected_dguids)
})

test_that("DGUIDs keep values that look like NA strings or numbers", {
  local_dguid_api(data = dguid_data_csv(missing = "1991X000159001001"))
  expect_equal(dguid_census(vectors = "v_X_1", labels = "detailed")$DGUID,
               c("2021S05079330001.01", "1991X000159001001"))
})

test_that("geometries get a single DGUID column right after GeoUID", {
  local_dguid_api()
  for (labels in c("detailed", "short", "labelled")) {
    with_data <- dguid_census(vectors = "v_X_1", geo_format = "sf", labels = labels)
    geo_only <- dguid_census(geo_format = "sf", labels = labels)
    for (result in list(with_data, geo_only)) {
      expect_s3_class(result, "sf")
      expect_equal(sum(tolower(names(result)) == "dguid"), 1)
      expect_equal(as.vector(result$DGUID), expected_dguids)
      expect_equal(match("DGUID", names(result)), match("GeoUID", names(result)) + 1)
      expect_equal(attr(result, "sf_column"), names(result)[ncol(result)])
    }
  }

  skip_if_not_installed("sp")
  sp <- dguid_census(vectors = "v_X_1", geo_format = "sp", labels = "detailed")
  expect_equal(sp$DGUID, expected_dguids)
  expect_equal(match("DGUID", names(sp)), match("GeoUID", names(sp)) + 1)
})

test_that("DGUID gets a label in both languages for labels = 'labelled'", {
  local_dguid_api()
  en <- dguid_census(vectors = "v_X_1", labels = "labelled", language = "en")
  expect_equal(attr(en$DGUID, "label"), "Dissemination Geography Unique Identifier")
  fr <- dguid_census(vectors = "v_X_1", labels = "labelled", language = "fr")
  expect_equal(attr(fr$DGUID, "label"), "Identificateur géographique unique de diffusion")
  expect_null(attr(dguid_census(vectors = "v_X_1", labels = "detailed")$DGUID, "label"))
})

test_that("responses from before DGUIDs, e.g. from the cache, work without the column", {
  local_dguid_api(data = dguid_data_csv(with_dguid = FALSE), geo = dguid_geo_json(with_dguid = FALSE))
  for (labels in c("detailed", "labelled")) {
    expect_false("DGUID" %in% names(dguid_census(vectors = "v_X_1", labels = labels)))
    geo <- dguid_census(vectors = "v_X_1", geo_format = "sf", labels = labels)
    expect_false(any(tolower(names(geo)) == "dguid"))
    expect_equal(as.vector(geo$GeoUID), c("9330001.01", "9330001.02"))
  }
})

test_that("data and geometries from before and after DGUIDs combine to one DGUID column", {
  for (with_dguid_in_data in c(TRUE, FALSE)) {
    local({
      local_dguid_api(data = dguid_data_csv(with_dguid = with_dguid_in_data),
                      geo = dguid_geo_json(with_dguid = !with_dguid_in_data))
      result <- dguid_census(vectors = "v_X_1", geo_format = "sf", labels = "detailed")
      expect_equal(sum(tolower(names(result)) == "dguid"), 1)
      expect_equal(as.vector(result$DGUID), expected_dguids)
      expect_equal(match("DGUID", names(result)), match("GeoUID", names(result)) + 1)
    })
  }
})
