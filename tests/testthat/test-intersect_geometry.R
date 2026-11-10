intersect_test_response <- function(body) {
  structure(list(status_code = 200L, content = charToRaw(body),
                 headers = httr::insensitive(list(`data-version` = "d.1", `content-type` = "application/json"))),
            class = "response")
}

test_that("get_intersecting_geometries returns a region list, or the region ids when simplified", {
  with_statcan_cache({
    local_mocked_bindings(cm_POST = function(...) intersect_test_response('{"CT":["9330059.17","9330065.00"]}'))
    point <- sf::st_sfc(sf::st_point(c(-123.12, 49.28)), crs = 4326)

    regions <- get_intersecting_geometries("CA21", "CT", point, api_key = "CensusMapper_test", quiet = TRUE)
    expect_equal(regions$CT, c("9330059.17", "9330065.00"))

    ids <- get_intersecting_geometries("CA21", "CT", point, simplified = TRUE,
                                       api_key = "CensusMapper_test", quiet = TRUE)
    expect_identical(ids, c("9330059.17", "9330065.00"))
  })
})
