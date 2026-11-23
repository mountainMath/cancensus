# Offline tests of the CensusMapper helpers: custom functions, label formatting, scales and coords

test_that("cm_gini() and cm_gini2() match hand-computed Gini coefficients", {
  # incomes 10 (bracket midpoint) and 40 (twice the top cutoff less the previous one), one each
  expect_equal(cm_gini(cbind(1, 1), c(0, 20)), 0.3)
  expect_equal(cm_gini2(cbind(1, 1), c(0, 20)), 0.3)
  # a total income of 100 puts the top bracket at 90
  expect_equal(cm_gini(cbind(1, 1), c(0, 20), 100), 0.4)
  expect_equal(cm_gini2(cbind(1, 1), c(0, 20), 100), 0.4)
  # everyone in one bracket
  expect_equal(cm_gini(cbind(0, 10, 0), c(0, 10, 20)), 0)
  # one value per region, NA where an input is missing
  expect_equal(cm_gini(cbind(c(1, NA, 2), c(1, 1, 2)), c(0, 20)), c(0.3, NA, 0.3))
})

test_that("cm_binned_quantile() interpolates within brackets", {
  counts <- cbind(c(10, 5), c(20, 10), c(5, 20))
  cutoffs <- c(0, 20000, 50000)
  expect_equal(cm_binned_quantile(counts, cutoffs, 0.5), c(31250, 56250))
  expect_equal(cm_binned_quantile(counts, cutoffs, c(0.25, 0.75)), c(17500, 78125))
  expect_true(is.nan(cm_binned_quantile(cbind(0, 0, 0), cutoffs, 0.5)))
})

test_that("inputs with the wrong number of regions are errors, not recycled", {
  counts <- cbind(c(10, 5, 1), c(20, 10, 1), c(5, 20, 1))
  expect_error(cm_binned_quantile(counts, c(0, 20000, 50000), c(0.25, 0.75)),
               class = "cancensus_cm_regions_mismatch")
  expect_error(cm_gini(counts, rbind(c(0, 1, 2), c(0, 1, 2))), class = "cancensus_cm_regions_mismatch")
  expect_length(cm_gini(counts, rbind(c(0, 1, 2))), 3)
})

test_that("cm_wasserstein() gives the distance for each region", {
  skip_if_not_installed("transport")
  expect_equal(cm_wasserstein(cbind(c(1, 2), c(3, 4)), c(2, 2)), c(1, 1))
})

test_that("values get formatted like CensusMapper's legends", {
  expect_equal(cm_format_values(c(125000, 1500000), "currency"), c("$125k", "$1.5M"))
  expect_equal(cm_format_values(125000, "currency", "fr"), "125k\u00a0$")
  expect_equal(cm_format_values(0.153, "percentage"), "15.3%")
  expect_equal(cm_format_values(0.153, "percentage", "fr"), "15,3\u00a0%")
  expect_equal(cm_format_values(12, "percentage_100"), "12%")
  expect_equal(cm_format_values(125, "currency_1000"), "$125k")
  # up to three significant digits
  expect_equal(cm_format_values(c(2.5, 1234.5), "ratio"), c("2.5", "1,230"))
  expect_equal(cm_format_values(-2000, "number"), "-2k")
  # values that would round to the same label get more digits
  expect_equal(anyDuplicated(cm_format_values(c(1000, 1001, 1002), "number")), 0L)
})

test_that("bins get labels for their ranges", {
  expect_equal(cm_bin_labels(c(10, 20)), c("< 10", "10 \u2013 20", "\u2265 20"))
  expect_equal(cm_bin_labels(numeric(0), lang = "en"), "All")
  expect_equal(cm_bin_labels(numeric(0), lang = "fr"), "Tout")
  expect_length(cm_auto_breaks(1:100, 4), 3)
  expect_equal(cm_auto_breaks(rep(5, 10), 4), 5)
})

test_that("palettes come from ColorBrewer, viridis or hcl.colors", {
  expect_equal(cm_palette(function(n) rep("red", n), 2), c("red", "red"))
  expect_equal(cm_palette("Blues 3", 3), grDevices::hcl.colors(3, "Blues 3"))
  expect_equal(cm_palette("Blues 3", 3, -1), rev(grDevices::hcl.colors(3, "Blues 3")))
  expect_error(cm_palette("No such palette", 3), class = "cancensus_cm_unknown_palette")
  skip_if_not_installed("RColorBrewer")
  expect_equal(cm_palette("Blues", 4), RColorBrewer::brewer.pal(4, "Blues"))
  expect_length(cm_palette("Blues", 12), 12)
  expect_length(cm_palette("Blues", 1), 1)
  skip_if_not_installed("viridisLite")
  expect_equal(cm_palette("magma", 3), viridisLite::viridis(3, option = "magma"))
})

squares <- function(values) {
  sf::st_sf(value = values, geometry = sf::st_sfc(lapply(seq_along(values) - 1, function(i) {
    sf::st_polygon(list(rbind(c(i, 0), c(i + 1, 0), c(i + 1, 1), c(i, 1), c(i, 0))))
  }), crs = 3857))
}

test_that("scale_fill_cm_bins() bins values and labels all bins", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("RColorBrewer")
  p <- ggplot2::ggplot(squares(c(1, 15, 16, NA))) +
    ggplot2::geom_sf(ggplot2::aes(fill = value)) +
    scale_fill_cm_bins(breaks = c(10, 20))
  built <- ggplot2::ggplot_build(p)
  colours <- RColorBrewer::brewer.pal(3, "Blues")
  expect_equal(as.character(built$data[[1]]$fill), c(colours[1], colours[2], colours[2], "grey60"))
  # the top bin has no data, but still gets a label
  expect_equal(built$plot$scales$get_scales("fill")$get_labels(),
               c("< 10", "10 \u2013 20", "\u2265 20", "NA"))
  expect_error(scale_fill_cm_bins(breaks = c(10, 20), values = c("red", "blue")),
               class = "cancensus_cm_bins_values_count")
})

test_that("coord_cm_bbox() limits the map to a bounding box", {
  skip_if_not_installed("ggplot2")
  base <- ggplot2::ggplot(squares(1:4)) + ggplot2::geom_sf()
  box <- sf::st_bbox(c(xmin = 1, ymin = 0, xmax = 2, ymax = 1), crs = 3857)
  p <- base + coord_cm_bbox(box)
  expect_equal(p$coordinates$limits$x, c(1, 2))
  expect_warning(p <- base + coord_cm_bbox(c(1, 0, 2, 1)), "EPSG:3857", class = "cancensus_cm_bbox_no_crs_data")
  expect_equal(p$coordinates$limits$y, c(0, 1))
  expect_error(coord_cm_bbox("a"), class = "cancensus_cm_bbox_invalid")
  expect_error(coord_cm_bbox(c(1, NA, 2, 1)), class = "cancensus_cm_bbox_missing_values")
})

test_that("the base map needs an API key and spatial data before fetching tiles", {
  local_mocked_bindings(cm_download_tile = function(...) stop("Unexpected download"))
  # ignore an API key set in the environment or options
  local_mocked_bindings(robust_api_key = function(api_key) api_key)
  expect_error(cm_context(squares(1), api_key = ""), "cm_context()", fixed = TRUE,
               class = "cancensus_cm_tile_api_key")
  expect_error(cm_context(data.frame(a = 1), api_key = "x"), class = "cancensus_cm_context_invalid_data")
})

test_that("errors of the helpers are available in French", {
  rlang::local_options(cancensus.language = "fr")
  err <- expect_error(cm_palette("No such palette", 3), class = "cancensus_cm_unknown_palette")
  expect_equal(conditionMessage(err),
               cc_msg("cm_unknown_palette", palette = "No such palette", .language = "fr"))
})
