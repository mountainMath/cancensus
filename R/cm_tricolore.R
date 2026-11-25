# Ternary (three-part) colour maps in the style of CensusMapper's rgb maps,
# with the colours of the tricolore package (Schöley 2021), computed here
# without tricolore's dependency on ggtern.

#' Colours for three-part compositions, as on CensusMapper
#'
#' Colours for compositions of three parts, e.g. the shares of children,
#' working age population and seniors, with the balance scheme of the
#' tricolore package, as used by CensusMapper's ternary ("rgb") maps. The
#' colours are the same as \code{tricolore::Tricolore()}'s, with the
#' parameters in CensusMapper's units, the same as in a CensusMapper map's
#' \code{tricolore_options}.
#'
#' With discrete colours (finite \code{breaks}) the triangle of compositions is
#' split into \code{breaks^2} triangles, and each composition gets the colour
#' of the centroid of its triangle. The \code{center} changes the colours of
#' these triangles, but not their shape.
#'
#' @param p1,p2,p3 The three parts, as counts or shares; they are divided by
#' their total. Compositions with missing or negative parts, or a total of
#' zero, get \code{NA}.
#' @param center The composition that gets the neutral (grey) colour, e.g. the
#' composition of the whole region. By default the centre of the triangle.
#' @param breaks The number of breaks along each side of the triangle for
#' discrete colours, \code{Inf} for continuous colours.
#' @param hue The hue of the first part, in degrees (0 to 360); the other parts
#' are 120 and 240 degrees further.
#' @param chroma The maximum chroma (CIE-LUV, 0 to 200). Above about 75 some
#' colours are outside the sRGB gamut and are clipped.
#' @param lightness The lightness (0 to 100).
#' @param contrast How much less light and chroma compositions near the centre
#' get (0 to 1).
#' @param spread How spread out the colours are around the centre (> 0).
#'
#' @return A character vector of colours (\code{"#RRGGBB"}), one for each
#' composition.
#' @seealso \code{\link{geom_cm_tricolore}()} for a ggplot map with these
#' colours and a colour key.
#' @references Schöley, J. (2021). The centered ternary balance scheme: A
#' technique to visualize surfaces of unbalanced three-part compositions.
#' Demographic Research, 44(19), 443–458. \doi{10.4054/DemRes.2021.44.19}
#' @export
#'
#' @examples
#' cm_tricolore(c(0.2, 0.5, 1), c(0.6, 0.3, 1), c(0.2, 0.2, 1))
cm_tricolore <- function(p1, p2, p3, center = rep(1/3, 3), breaks = 3,
                         hue = 60, chroma = 200, lightness = 80,
                         contrast = 0.4, spread = 1) {
  cm_check_tricolore_options(center, breaks, hue, chroma, lightness, contrast, spread)
  P <- cbind(as.numeric(p1), as.numeric(p2), as.numeric(p3))
  total <- rowSums(P)
  ok <- stats::complete.cases(P) & is.finite(total) & total > 0 & rowSums(P < 0) == 0
  colours <- rep(NA_character_, nrow(P))
  if (!any(ok)) return(colours)
  P <- P[ok, , drop = FALSE] / total[ok]
  if (breaks < 100) P <- cm_ternary_centroid_of(P, breaks)
  # Perturb by the inverse of the centre, then power scale; as in tricolore,
  # including its rounded constants, so the colours match it exactly
  P <- t(t(P) / center)
  P <- P / rowSums(P)
  P <- P^spread
  P <- P / rowSums(P)
  phi <- (hue * 0.0174 + c(0, 2.09, 4.19)) %% 6.28
  z <- as.vector(P %*% complex(argument = phi, modulus = chroma))
  cfactor <- Mod(z) * contrast / chroma + 1 - contrast
  colours[ok] <- substr(grDevices::hcl(h = (Arg(z) * 57.3) %% 360, c = cfactor * Mod(z),
                                       l = cfactor * lightness, fixup = TRUE), 1, 7)
  colours
}

cm_check_tricolore_options <- function(center, breaks, hue, chroma, lightness, contrast, spread) {
  if (!is.numeric(center) || length(center) != 3 || any(!is.finite(center)) || any(center <= 0)) {
    cc_abort("tricolore_invalid_center")
  }
  if (!is.numeric(breaks) || length(breaks) != 1 || is.na(breaks) || breaks < 1 ||
      (is.finite(breaks) && breaks != round(breaks))) {
    cc_abort("tricolore_invalid_breaks")
  }
  for (x in list(hue, chroma, lightness, contrast, spread)) {
    if (!is.numeric(x) || length(x) != 1 || !is.finite(x)) {
      cc_abort("tricolore_invalid_options")
    }
  }
  if (chroma <= 0) cc_abort("tricolore_invalid_chroma")
  if (spread <= 0) cc_abort("tricolore_invalid_spread")
}

# The centroid of the mesh triangle each composition (a row of P, summing to
# 1) is in, for a triangle split into k triangles along each side. With
# a, b, c the compositions' parts times k rounded down, a triangle pointing up
# has a + b + c = k - 1 and its centroid at (a, b, c) + 1/3, one pointing down
# a + b + c = k - 2 and its centroid at (a, b, c) + 2/3. That is the nearest
# centroid, the same as tricolore's. Compositions on or very near the edges
# between triangles (where rounding errors decide) are matched to the nearest
# centroid as tricolore does, so that ties go the same way.
cm_ternary_centroid_of <- function(P, k) {
  scaled <- P * k
  A <- pmin(floor(scaled), k - 1)
  frac <- scaled - floor(scaled)
  s <- rowSums(A)
  edge <- rowSums(frac < 1e-9 | frac > 1 - 1e-9) > 0 | !(s %in% c(k - 1, k - 2))
  out <- (A + ifelse(s == k - 1, 1 / 3, 2 / 3)) / k
  if (any(edge)) {
    C <- cm_ternary_mesh_centroids(k)
    nearest <- apply(P[edge, , drop = FALSE], 1, function(p) {
      Q <- t(p - t(C))
      # The sum of the cross products of the differences is minus half their
      # squared length, so the nearest centroid has the largest
      which.max(Q[, 2] * Q[, 3] + Q[, 3] * Q[, 1] + Q[, 1] * Q[, 2])
    })
    out[edge, ] <- C[nearest, , drop = FALSE]
  }
  out
}

# The centroids of the k^2 triangles of a triangle split into k triangles along
# each side, in tricolore's order
cm_ternary_mesh_centroids <- function(k) {
  K <- k^2
  id <- seq_len(K)
  g <- floor(sqrt(K - id))
  gsq <- g^2
  cbind(
    (((-K + id + g * (g + 2) + 1) %% 2) - 3 * gsq - 3 * id + 3 * K + 1) / (6 * k),
    -(((-K + gsq + id + 2 * g + 1) %% 2) + 3 * g - 3 * k + 1) / (3 * k),
    (((-K + gsq + id + 2 * g + 1) %% 2) + 3 * gsq + 6 * g + 3 * id - 3 * K + 1) / (6 * k))
}

#' A ggplot map of three-part compositions, as on CensusMapper
#'
#' Maps compositions of three parts, e.g. the shares of children, working age
#' population and seniors, in the colours of CensusMapper's ternary ("rgb")
#' maps (see \code{\link{cm_tricolore}()}), with a triangular colour key: a
#' \code{ggplot2::geom_sf()} layer with the colours as fill, and the key as a
#' legend. The key shows the colours of compositions, the first part at the
#' top, the second bottom left and the third bottom right, and the regions as
#' dots.
#'
#' The fill colours are set directly, not through a fill scale, so the plot
#' can still have a fill scale for other layers. Regions without a composition
#' (missing or negative parts, or a total of zero) are \code{na.value}. The
#' compositions are computed from the layer's data, or the plot's data if the
#' layer has none, when the layer is added to the plot.
#'
#' @param mapping Aesthetics made with \code{ggplot2::aes()}: \code{p1},
#' \code{p2} and \code{p3}, the three parts, as counts or shares, are
#' required. Others, e.g. \code{colour}, are passed on to \code{geom_sf()}.
#' @param data The data for the layer, sf data; by default the plot's data.
#' @param labels Labels for the three parts on the key. By default the
#' expressions for \code{p1}, \code{p2} and \code{p3}. Long labels are wrapped.
#' @param name The title of the key, by default none.
#' @inheritParams cm_tricolore
#' @param na.value The colour for regions without a composition.
#' @param show_data Whether to show the regions as dots on the key.
#' @param key_size The width of the key, in cm.
#' @param show.legend Whether to show the key.
#' @param ... Other arguments passed on to \code{ggplot2::geom_sf()}, e.g.
#' \code{colour = NA}.
#'
#' @return An object to add to a ggplot with \code{+}.
#' @seealso \code{\link{cm_tricolore}()} for just the colours.
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' census_data <- get_census("CA21", regions = list(CMA = "59933"),
#'                           vectors = c(children = "v_CA21_11",
#'                                       working_age = "v_CA21_68",
#'                                       seniors = "v_CA21_251"),
#'                           level = "CT", geo_format = "sf")
#' ggplot(census_data) +
#'   geom_cm_tricolore(aes(p1 = children, p2 = working_age, p3 = seniors),
#'                     labels = c("0 to 14", "15 to 64", "65 and over"),
#'                     colour = NA) +
#'   geom_cm_context() +
#'   coord_sf(datum = NA)
#' }
geom_cm_tricolore <- function(mapping = NULL, data = NULL, ..., labels = NULL, name = NULL,
                              center = rep(1/3, 3), breaks = 3, hue = 60, chroma = 200,
                              lightness = 80, contrast = 0.4, spread = 1,
                              na.value = "grey60", show_data = TRUE, key_size = 3,
                              show.legend = TRUE) {
  rlang::check_installed("ggplot2", version = "3.5.0", reason = "for geom_cm_tricolore().")
  cm_check_tricolore_options(center, breaks, hue, chroma, lightness, contrast, spread)
  if (!is.null(labels) && length(labels) != 3) {
    cc_abort("tricolore_invalid_labels")
  }
  structure(list(mapping = mapping, data = data, args = list(...), labels = labels, name = name,
                 options = list(center = center, breaks = breaks, hue = hue, chroma = chroma,
                                lightness = lightness, contrast = contrast, spread = spread),
                 na.value = na.value, show_data = show_data, key_size = key_size,
                 show.legend = show.legend),
            class = "cm_tricolore_layer")
}

#' @exportS3Method ggplot2::ggplot_add
ggplot_add.cm_tricolore_layer <- function(object, plot, ...) {
  parts <- c("p1", "p2", "p3")
  mapping <- object$mapping
  if (is.null(mapping)) mapping <- ggplot2::aes()
  # The parts can also come from the plot's aesthetics
  for (p in parts) {
    if (is.null(mapping[[p]]) && !is.null(plot$mapping[[p]])) mapping[[p]] <- plot$mapping[[p]]
  }
  missing_parts <- parts[vapply(parts, function(p) is.null(mapping[[p]]), logical(1))]
  if (length(missing_parts) > 0) {
    cc_abort("tricolore_missing_aesthetics", missing = missing_parts)
  }
  data <- if (!is.null(object$data)) object$data else plot$data
  if (!inherits(data, "data.frame")) {
    cc_abort("tricolore_no_data")
  }
  values <- lapply(parts, function(p) rlang::eval_tidy(mapping[[p]], data))
  colours <- do.call(cm_tricolore, c(values, object$options))

  data$.cm_tricolore_colour <- ifelse(is.na(colours), object$na.value, colours)
  layer_mapping <- mapping[setdiff(names(mapping), parts)]
  layer_mapping$fill <- quote(I(.cm_tricolore_colour))
  class(layer_mapping) <- class(ggplot2::aes())
  plot <- plot + do.call(ggplot2::geom_sf,
                         c(list(mapping = layer_mapping, data = data), object$args))

  if (isTRUE(object$show.legend)) {
    labels <- object$labels
    if (is.null(labels)) labels <- vapply(parts, function(p) rlang::as_label(mapping[[p]]), character(1))
    points <- if (isTRUE(object$show_data)) {
      P <- do.call(cbind, values)
      P[!is.na(colours), , drop = FALSE]
    }
    key <- cm_tricolore_key(object$options, labels, points)
    size <- grid::unit(object$key_size, "cm")
    # The key becomes a grob only when the legend is drawn: ggplotGrob() measures text on the current
    # device and would otherwise open one (e.g. Rplots.pdf) just by adding the layer
    plot <- plot + ggplot2::guides(cm_tricolore = ggplot2::guide_custom(
      grid::gTree(key = key, cl = "cm_tricolore_key"), title = object$name, width = size, height = size))
  }
  plot
}

#' @exportS3Method grid::makeContent
makeContent.cm_tricolore_key <- function(x) {
  grid::setChildren(x, grid::gList(ggplot2::ggplotGrob(x$key)))
}

# The colour key: a triangle split into triangles coloured by
# cm_tricolore() at their centroids (for discrete colours exactly the colour
# regions), the first part at the top, the second bottom left, the third
# bottom right, and the compositions in `points` as dots
cm_tricolore_key <- function(options, labels, points = NULL) {
  n <- if (options$breaks < 100) options$breaks else 40
  # Triangles pointing up, with corners (a+1, b, c), (a, b+1, c), (a, b, c+1),
  # and down, with corners (a, b+1, c+1), (a+1, b, c+1), (a+1, b+1, c), in
  # units of 1/n, where a + b + c is n - 1 and n - 2
  ab <- expand.grid(a = seq_len(n) - 1, b = seq_len(n) - 1)
  up <- ab[ab$a + ab$b <= n - 1, ]
  up$c <- n - 1 - up$a - up$b
  down <- ab[ab$a + ab$b <= n - 2, ]
  down$c <- n - 2 - down$a - down$b
  corners <- function(t, d) {
    data.frame(triangle = seq_len(nrow(t)),
               p1 = (t$a + d[1]) / n, p2 = (t$b + d[2]) / n, p3 = (t$c + d[3]) / n)
  }
  mesh <- rbind(corners(up, c(1, 0, 0)), corners(up, c(0, 1, 0)), corners(up, c(0, 0, 1)))
  if (nrow(down) > 0) {
    down_mesh <- rbind(corners(down, c(0, 1, 1)), corners(down, c(1, 0, 1)), corners(down, c(1, 1, 0)))
    down_mesh$triangle <- down_mesh$triangle + nrow(up)
    mesh <- rbind(mesh, down_mesh)
  }
  centroids <- stats::aggregate(cbind(p1, p2, p3) ~ triangle, data = mesh, FUN = mean)
  centroids$colour <- do.call(cm_tricolore, c(list(centroids$p1, centroids$p2, centroids$p3), options))
  mesh$colour <- centroids$colour[match(mesh$triangle, centroids$triangle)]
  mesh$x <- mesh$p3 + mesh$p1 / 2
  mesh$y <- mesh$p1 * sqrt(3) / 2

  wrapped <- vapply(as.character(labels), function(l) paste(strwrap(l, 20), collapse = "\n"),
                    character(1), USE.NAMES = FALSE)
  h <- sqrt(3) / 2
  key <- ggplot2::ggplot(mesh, ggplot2::aes(.data$x, .data$y)) +
    ggplot2::geom_polygon(ggplot2::aes(group = .data$triangle, fill = I(.data$colour),
                                       colour = I(.data$colour)), linewidth = 0.1)
  if (!is.null(points) && nrow(points) > 0) {
    points <- points / rowSums(points)
    key <- key + ggplot2::annotate("point", x = points[, 3] + points[, 1] / 2,
                                   y = points[, 1] * h, colour = "black",
                                   alpha = 0.25, size = 0.4, stroke = 0)
  }
  key +
    ggplot2::annotate("text", x = c(0.5, 0, 1), y = c(h + 0.04, -0.04, -0.04), label = wrapped,
                      vjust = c(0, 1, 1), hjust = c(0.5, 0.2, 0.8), size = 2.5, lineheight = 0.9) +
    ggplot2::coord_equal(clip = "off") +
    ggplot2::theme_void()
}
