test_that("cm_tricolore() gives tricolore's colours", {
  skip_if_not_installed("tricolore")
  set.seed(42)
  P <- matrix(stats::rexp(3 * 2000), ncol = 3)
  # Some extreme compositions, near the corners and sides
  P <- rbind(P, c(1, 0, 0), c(0, 1, 0), c(0, 0, 1), c(1, 1, 0), c(0.001, 1, 1))
  settings <- list(
    list(),
    list(breaks = 5),
    list(breaks = Inf),
    list(center = c(0.2, 0.6, 0.2)),
    list(center = c(0.5, 0.3, 0.2), breaks = 4, hue = 120, chroma = 140,
         lightness = 70, contrast = 0.6, spread = 1.5),
    list(center = c(0.2, 0.6, 0.2), breaks = Inf, spread = 0.7)
  )
  for (s in settings) {
    o <- utils::modifyList(list(center = rep(1/3, 3), breaks = 3, hue = 60, chroma = 200,
                                lightness = 80, contrast = 0.4, spread = 1), s)
    ours <- do.call(cm_tricolore, c(list(P[, 1], P[, 2], P[, 3]), o))
    theirs <- tricolore::Tricolore(data.frame(p1 = P[, 1], p2 = P[, 2], p3 = P[, 3]),
                                   "p1", "p2", "p3", center = o$center, breaks = o$breaks,
                                   hue = o$hue / 360, chroma = o$chroma / 200,
                                   lightness = o$lightness / 100, contrast = o$contrast,
                                   spread = o$spread, legend = FALSE)
    # tricolore breaks exact ties between triangles at random
    expect_gt(mean(ours == theirs), 0.998)
  }
})

test_that("cm_tricolore() gives NA for compositions without parts", {
  colours <- cm_tricolore(c(1, NA, 0, -1, 2), c(1, 1, 0, 1, 2), c(1, 1, 0, 3, 2))
  expect_true(is.na(colours[2]) && is.na(colours[3]) && is.na(colours[4]))
  expect_equal(colours[1], colours[5])
  expect_match(colours[1], "^#[0-9A-F]{6}$")
  expect_error(cm_tricolore(1, 1, 1, breaks = 2.5), class = "cancensus_tricolore_invalid_breaks")
  expect_error(cm_tricolore(1, 1, 1, center = c(0.5, 0.5, 0)), class = "cancensus_tricolore_invalid_center")
  expect_error(cm_tricolore(1, 1, 1, spread = 0), class = "cancensus_tricolore_invalid_spread")
})

test_that("geom_cm_tricolore() fills the map and adds a key", {
  skip_if_not_installed("ggplot2", "3.5.0")
  squares <- lapply(0:3, function(i) {
    sf::st_polygon(list(rbind(c(i, 0), c(i + 1, 0), c(i + 1, 1), c(i, 1), c(i, 0))))
  })
  data <- sf::st_sf(a = c(1, 2, 0, NA), b = c(1, 1, 0, 1), c = c(1, 0, 0, 1),
                    geometry = sf::st_sfc(squares, crs = 3857))
  p <- ggplot2::ggplot(data) +
    geom_cm_tricolore(ggplot2::aes(p1 = a, p2 = b, p3 = c), colour = NA, na.value = "grey50")
  built <- ggplot2::ggplot_build(p)
  fill <- as.character(built$data[[1]]$fill)
  expect_equal(fill[1:2], cm_tricolore(c(1, 2), c(1, 1), c(1, 0)))
  expect_equal(fill[3:4], c("grey50", "grey50"))
  # a device of our own, so that measuring text doesn't leave an Rplots.pdf behind
  grDevices::png(tempfile(fileext = ".png"))
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_s3_class(ggplot2::ggplotGrob(p), "gtable")
  expect_error(print(ggplot2::ggplot(data) + geom_cm_tricolore(ggplot2::aes(p1 = a))),
               "p2, p3", class = "cancensus_tricolore_missing_aesthetics")
  expect_error(geom_cm_tricolore(labels = c("a", "b")), class = "cancensus_tricolore_invalid_labels")
})

test_that("the direct lookup of mesh triangles matches the nearest centroid", {
  nearest <- function(P, k) {
    C <- cm_ternary_mesh_centroids(k)
    C[apply(P, 1, function(p) {
      Q <- t(p - t(C))
      which.max(Q[, 2] * Q[, 3] + Q[, 3] * Q[, 1] + Q[, 1] * Q[, 2])
    }), , drop = FALSE]
  }
  set.seed(1)
  # random compositions, and compositions of small counts, many of them on edges between triangles
  P <- rbind(matrix(stats::rexp(3 * 500), ncol = 3), unname(as.matrix(expand.grid(0:6, 0:6, 0:6)))[-1, ])
  P <- P / rowSums(P)
  for (k in c(1:7, 12, 99)) {
    expect_equal(cm_ternary_centroid_of(P, k), nearest(P, k), label = paste("breaks", k))
  }
})
