# ggplot2 helpers for maps made with census data in the style of CensusMapper:
# the base map and binned colour scales. ggplot2 is only suggested.

#' CensusMapper's base map for a ggplot map
#'
#' Adds CensusMapper's base map on top of a map of census data: landuse
#' (semi-transparent, so the data shows through), water, buildings (at high
#' zoom only), roads and place and road labels, drawn from CensusMapper's vector
#' tiles and styled like the maps on censusmapper.ca. As on CensusMapper, add it
#' after the \code{geom_sf()} with the census data, which it draws over.
#'
#' The map looks like CensusMapper showing the data in a window \code{pixels}
#' wide (or at \code{zoom}), scaled to the size of the plot's panel when it is
#' drawn: that zoom level decides which features, roads and labels are shown,
#' and lines and labels keep their size relative to the map whatever size the
#' plot is drawn at. Labels are thinned out so that they don't overlap.
#'
#' Tiles are downloaded from the tile server and cached for the session. They
#' need a CensusMapper API key, the same as for the census data, though tiles
#' count against a separate tile quota. Set it once with
#' \code{\link{set_cancensus_api_key}()} or pass it with \code{api_key}.
#' Needs the ggplot2 package, and GDAL with its MVT driver for sf to read the
#' tiles. To style the base map differently, or to use it with other mapping
#' packages, get its features with \code{\link{cm_context}()}.
#'
#' @param layers Which parts of the base map to draw, any of \code{"landuse"},
#' \code{"water"}, \code{"buildings"}, \code{"roads"} and \code{"labels"}.
#' @param data sf data (or a bounding box) whose extent the base map covers. By
#' default the plot's data, or the first layer with sf data.
#' @param zoom Zoom level (as on CensusMapper) to draw the base map at. By
#' default the zoom level at which the data spans \code{pixels} pixels.
#' @param pixels The width, in pixels, of the CensusMapper map the plot mimics.
#' Larger values show more detail, with relatively smaller lines and labels.
#' @param scale Factor to make lines and labels larger or smaller.
#' @param lang Language of the labels, \code{"en"} or \code{"fr"}.
#' @param family Font family of the labels. By default "Noto Sans", as on
#' CensusMapper, if the systemfonts package finds it, otherwise "sans".
#' @param tile_server Where to get the tiles from.
#' @param api_key An API key for the CensusMapper API. Defaults to the
#' \code{CM_API_KEY} environment variable, as set with
#' \code{\link{set_cancensus_api_key}()}. The key is looked up when the base
#' map is created and kept with it.
#'
#' @return An object to add to a ggplot with \code{+}.
#' @seealso \code{\link{cm_context}()} for the features of the base map as sf data.
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' census_data <- get_census("CA21", regions = list(CSD = "5915022"),
#'                           vectors = c(population = "v_CA21_1"),
#'                           level = "DA", geo_format = "sf")
#' ggplot(census_data) +
#'   geom_sf(aes(fill = population / `Shape Area`), colour = NA) +
#'   scale_fill_viridis_c(trans = "log") +
#'   geom_cm_context() +
#'   coord_sf(datum = NA)
#' }
geom_cm_context <- function(layers = c("landuse", "water", "buildings", "roads", "labels"),
                            data = NULL, zoom = NULL, pixels = 800, scale = 1,
                            lang = c("en", "fr"), family = NULL,
                            tile_server = "https://censusmapper.ca",
                            api_key = Sys.getenv("CM_API_KEY")) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("geom_cm_context() needs the ggplot2 package")
  }
  layers <- match.arg(layers, several.ok = TRUE)
  structure(list(layers = layers, data = data, zoom = zoom, pixels = pixels, scale = scale,
                 lang = match.arg(lang), family = family, tile_server = tile_server,
                 api_key = cm_tile_api_key(api_key, "geom_cm_context()")),
            class = "cm_context_layer")
}

#' @exportS3Method ggplot2::ggplot_add
ggplot_add.cm_context_layer <- function(object, plot, ...) {
  data <- object$data
  if (is.null(data)) data <- plot$data
  if (!inherits(data, c("sf", "sfc", "bbox"))) {
    data <- Find(function(l) inherits(l$data, "sf"), plot$layers)$data
  }
  if (is.null(data)) stop("geom_cm_context() needs sf data in the plot, or its data argument")
  for (l in cm_context_layers(data, object)) plot <- plot + l
  # The labels' x and y would otherwise title the axes
  if ("labels" %in% object$layers) plot <- plot + ggplot2::labs(x = NULL, y = NULL)
  plot
}

# Label styles in MapLibre's priority order. Sizes, halos and padding are in
# pixels; labels are placed when the plot is drawn (makeContent.cm_labels).
cm_label_styles <- function() {
  st <- function(classes, min, max, size, colour, bold, halo, pad = 2, upper = FALSE, road = FALSE)
    list(classes = classes, min = min, max = max, size = size, colour = colour,
         bold = bold, halo = halo, pad = pad, upper = upper, road = road)
  list(
    st("country", 1, 5, list(c(1, 12), c(4, 18)), "#2a2a2a", TRUE, 2, upper = TRUE),
    st("state", 3, 8, list(c(3, 10), c(6, 14)), "#4a4a4a", FALSE, 2, upper = TRUE),
    st("city", 5, Inf, list(c(5, 12), c(8, 16), c(12, 22)), "#000000", TRUE, 2, upper = TRUE),
    st("town", 8, Inf, list(c(8, 11), c(12, 14)), "#0a0a0a", TRUE, 2),
    st("village", 10, Inf, list(c(0, 11)), "#1a1a1a", FALSE, 1.5),
    st(c("motorway", "trunk", "primary"), 10, Inf, list(c(10, 11), c(13, 14)), "#3a3a3a", FALSE, 1.5, 20,
       road = TRUE),
    st(c("secondary", "tertiary"), 12, Inf, list(c(12, 11), c(14, 13)), "#4a4a4a", FALSE, 1.5, 30,
       road = TRUE),
    st(c("residential", "unclassified", "living_street", "service"), 14, Inf, list(c(0, 12)),
       "#5a5a5a", FALSE, 1.5, 40, road = TRUE))
}

# The ggplot layers of the base map: the features from cm_context_data(),
# styled like CensusMapper
cm_context_layers <- function(data, o) {
  v <- cm_view(data, o$zoom, o$pixels)
  z <- v$z
  bbox <- as.numeric(v$bbox)
  span <- v$span

  styles <- cm_label_styles()
  wanted <- vapply(styles, function(s) z >= s$min && z < s$max, logical(1))
  is_road <- vapply(styles, function(s) s$road, logical(1))
  labels <- "labels" %in% o$layers
  need <- c(if ("landuse" %in% o$layers && z > 9) c("landcover", "landuse"),
            if ("water" %in% o$layers) "water",
            if ("roads" %in% o$layers) c(if (z > 9) "aeroway", "roads"),
            if ("buildings" %in% o$layers && z > 14.5) "buildings",
            if (labels && any(wanted & !is_road)) "places",
            if (labels && any(wanted & is_road)) "road_labels")
  ctx <- cm_context_data(data, need, v, o$lang, o$tile_server, o$api_key,
                         road_label_classes = unlist(lapply(styles[wanted & is_road], `[[`, "classes")))
  has <- function(layer) !is.null(ctx[[layer]]) && nrow(ctx[[layer]]) > 0

  rows <- function(d, ...) {
    if (is.null(d) || nrow(d) == 0) return(NULL)
    v <- list(...)
    for (n in names(v)) d[[n]] <- rep_len(v[[n]], nrow(d))
    d
  }
  ramp <- function(stops, base = 1) cm_interpolate(z, stops, base)
  fill_layer <- function(d) {
    if (is.null(d) || nrow(d) == 0) return(NULL)
    ggplot2::geom_sf(data = d, ggplot2::aes(fill = I(.data$fill), alpha = I(.data$alpha)),
                     colour = NA, inherit.aes = FALSE)
  }
  # Lines with widths (in pixels, by zoom) from styles[[d$style]], drawn to scale
  line_layer <- function(d, styles) {
    if (is.null(d) || nrow(d) == 0) return(NULL)
    ggplot2::layer_sf(
      geom = cm_geom("lines"), stat = "sf", position = "identity", data = d,
      mapping = ggplot2::aes(colour = I(.data$colour), linetype = I(.data$linetype),
                             alpha = I(.data$alpha), style = .data$style),
      inherit.aes = FALSE, show.legend = FALSE,
      params = list(styles = styles, bbox = bbox, span = span, z = z, scale = o$scale))
  }

  out <- list()

  if (has("landcover") || has("landuse")) {
    a <- ramp(list(c(9, 0), c(10, 0.85)))
    natural <- rows(ctx$landcover, fill = "#a8a8a8", alpha = a)
    industrial <- if (has("landuse")) ctx$landuse[ctx$landuse$class %in% "industrial", ]
    industrial <- rows(industrial, fill = "#9a9a9a", alpha = a)
    out <- c(out, list(fill_layer(cm_rbind(natural, industrial))))
  }
  if (has("water")) {
    out <- c(out, list(fill_layer(rows(ctx$water, fill = "#d0d0d0", alpha = 1))))
  }
  if (has("aeroway")) {
    aero <- ctx$aeroway
    a <- ramp(list(c(9, 0), c(10, 0.9)))
    poly <- sf::st_is(aero, c("POLYGON", "MULTIPOLYGON"))
    out <- c(out, list(
      fill_layer(rows(aero[poly & aero$class %in% "runway", ], fill = "#2a2a2a", alpha = a)),
      line_layer(rows(aero[!poly & aero$class %in% "taxiway", ], colour = "#4a4a4a",
                      linetype = "solid", alpha = a, style = 1),
                 list(list(min = 0, width = list(c(0, 2)))))))
  }
  if (has("buildings")) {
    a <- ramp(list(c(14.5, 0), c(15.5, 0.9)))
    out <- c(out, list(
      fill_layer(rows(ctx$buildings, fill = "#d0d0d0", alpha = a)),
      line_layer(rows(ctx$buildings, colour = "#a0a0a0", linetype = "solid", alpha = a, style = 1),
                 list(list(min = 0, width = list(c(0, 0.5)))))))
  }
  if (has("roads")) {
    roads <- ctx$roads
    parts <- list()
    line_styles <- list()
    add <- function(classes, minzoom, colour, width, casing = NULL, linetype = "solid") {
      if (z < minzoom) return()
      d <- roads[roads$class %in% classes, ]
      if (nrow(d) == 0) return()
      for (w in list(casing, width)) {
        if (is.null(w)) next
        line_styles[[length(line_styles) + 1]] <<- list(min = minzoom, width = w)
        parts[[length(parts) + 1]] <<- rows(d, colour = if (identical(w, casing)) "#000000" else colour,
                                            linetype = if (identical(w, casing)) "solid" else linetype,
                                            alpha = 1, style = length(line_styles))
      }
    }
    add("motorway", 0, "#1a1a1a", list(c(4, 1), c(8, 3), c(14, 8)), list(c(4, 1.5), c(8, 4), c(14, 10)))
    add("trunk", 5, "#2a2a2a", list(c(5, 0.5), c(8, 2), c(14, 6)), list(c(5, 1), c(8, 3), c(14, 8)))
    add("primary", 6, "#3a3a3a", list(c(6, 0.5), c(8, 2), c(14, 5)), list(c(6, 0.8), c(8, 2.5), c(14, 6)))
    add("secondary", 8, "#4a4a4a", list(c(8, 1), c(14, 4)), list(c(8, 1.5), c(14, 5)))
    add("tertiary", 9, "#5a5a5a", list(c(9, 0.5), c(14, 3)))
    add(c("residential", "unclassified", "living_street"), 10, "#6a6a6a", list(c(10, 0.5), c(14, 2)))
    add("service", 11, "#808080", list(c(0, 1)))
    add(c("path", "footway", "cycleway", "bridleway"), 11, "#909090", list(c(0, 1)), linetype = "22")
    out <- c(out, list(line_layer(do.call(cm_rbind, parts), line_styles)))
  }
  if (labels) {
    family <- o$family
    if (is.null(family)) {
      family <- "sans"
      if (requireNamespace("systemfonts", quietly = TRUE) &&
          "Noto Sans" %in% systemfonts::system_fonts()$family) family <- "Noto Sans"
    }
    style_of <- function(class, which) {
      m <- rep(NA_integer_, length(class))
      for (i in rev(which(wanted & which))) m[class %in% styles[[i]]$classes] <- i
      m
    }
    cols <- c("label", "style", "angle", "length")

    places <- NULL
    if (has("places")) {
      places <- ctx$places
      places$label <- places$name
      places$style <- style_of(places$class, !is_road)
      places <- places[!is.na(places$style), ]
      upper <- vapply(styles, function(s) s$upper, logical(1))[places$style]
      places$label[upper] <- toupper(places$label[upper])
      places <- places[!duplicated(paste(places$class, places$label)), ]
      places$angle <- rep(0, nrow(places))
      places$length <- rep(NA_real_, nrow(places))
      places <- places[order(places$style), cols]
    }
    roads <- NULL
    if (has("road_labels")) {
      roads <- ctx$road_labels
      roads$label <- roads$name
      roads$style <- style_of(roads$class, is_road)
      roads <- roads[!is.na(roads$style), ]
      roads <- roads[order(roads$style, -roads$length), cols]
    }
    lab <- cm_rbind(places, roads)
    if (!is.null(lab) && nrow(lab) > 0) {
      xy <- sf::st_coordinates(lab)
      lab <- sf::st_drop_geometry(lab)
      lab$x <- xy[, 1]
      lab$y <- xy[, 2]
      lab$priority <- seq_len(nrow(lab))
      out <- c(out, list(ggplot2::layer(
        geom = cm_geom("labels"), stat = "identity", position = "identity", data = lab,
        mapping = ggplot2::aes(x = .data$x, y = .data$y, label = .data$label, style = .data$style,
                               angle = .data$angle, length = .data$length, priority = .data$priority),
        inherit.aes = FALSE, show.legend = FALSE,
        params = list(styles = styles, bbox = bbox, span = span, z = z, family = family, scale = o$scale))))
    }
  }
  Filter(Negate(is.null), out)
}

# Labels and lines are grobs that are laid out when drawn, when the panel's
# size, and with it the size of a CensusMapper pixel, is known. Their geoms are
# made when first needed, as ggplot2 is only suggested.
.cm_geoms <- new.env(parent = emptyenv())

cm_geom <- function(name) {
  if (is.null(.cm_geoms[[name]])) {
    .cm_geoms[[name]] <- switch(name,
      labels = ggplot2::ggproto("GeomCmLabels", ggplot2::Geom,
        required_aes = c("x", "y", "label"),
        default_aes = ggplot2::aes(style = 1, angle = 0, length = NA, priority = 0),
        draw_key = ggplot2::draw_key_blank,
        draw_panel = function(data, panel_params, coord, styles, bbox, span, z, family = "sans", scale = 1) {
          data <- coord$transform(data, panel_params)
          grid::gTree(data = data, styles = styles, bbox_npc = cm_bbox_npc(bbox, panel_params, coord),
                      span = span, z = z, family = family, scale = scale, cl = "cm_labels")
        }),
      lines = ggplot2::ggproto("GeomCmLines", ggplot2::GeomSf,
        optional_aes = "style",
        draw_panel = function(self, data, panel_params, coord, styles, bbox, span, z, scale = 1) {
          data <- coord$transform(data, panel_params)
          grid::gTree(data = data, styles = styles, bbox_npc = cm_bbox_npc(bbox, panel_params, coord),
                      span = span, z = z, scale = scale, cl = "cm_lines")
        }))
  }
  .cm_geoms[[name]]
}

# Width of the data's bbox in the panel (0-1)
cm_bbox_npc <- function(bbox, panel_params, coord) {
  corners <- coord$transform(data.frame(x = bbox[c(1, 3)], y = bbox[c(2, 2)]), panel_params)
  abs(diff(corners$x))
}

# Size of a CensusMapper pixel at zoom z in the panel being drawn, in inches
cm_inch_per_px <- function(x) {
  W <- grid::convertWidth(grid::unit(1, "npc"), "inches", valueOnly = TRUE)
  x$bbox_npc * W / (2^x$z * 512 * x$span / (2 * pi * 6378137)) * x$scale
}

# Lines, with widths set when drawn
#' @exportS3Method grid::makeContent
makeContent.cm_lines <- function(x) {
  d <- x$data
  if (nrow(d) == 0) return(grid::setChildren(x, grid::gList()))
  # grid's lwd is in 1/96 inch
  px <- vapply(x$styles[d$style], function(si) cm_interpolate(x$z, si$width, 1.5), numeric(1))
  gp <- grid::gpar(col = ggplot2::alpha(d$colour, d$alpha), lwd = px * cm_inch_per_px(x) * 96,
                   lty = d$linetype, lineend = "round", linejoin = "round")
  grid::setChildren(x, grid::gList(sf::st_as_grob(d$geometry, gp = gp)))
}

# Labels, sized and placed when drawn
#' @exportS3Method grid::makeContent
makeContent.cm_labels <- function(x) {
  W <- grid::convertWidth(grid::unit(1, "npc"), "inches", valueOnly = TRUE)
  H <- grid::convertHeight(grid::unit(1, "npc"), "inches", valueOnly = TRUE)
  d <- x$data[order(x$data$priority), ]
  world <- 2 * pi * 6378137
  z <- x$z
  s <- x$styles[d$style]
  get <- function(f) vapply(s, function(si) as.numeric(si[[f]]), numeric(1))
  size <- vapply(s, function(si) cm_interpolate(z, si$size), numeric(1))
  inch <- cm_inch_per_px(x)                  # one CensusMapper pixel in inches
  m_per_px <- world / (512 * 2^z)
  nch <- nchar(d$label)
  road <- !is.na(d$length)
  ok <- z >= get("min") & z < get("max") & (!road | d$length / m_per_px >= nch * size * 0.6)
  X <- d$x * W
  Y <- d$y * H
  a <- d$angle * pi / 180
  w <- (nch * 0.6 + 0.5) * size * inch / 2  # half extents of the label box
  h <- 0.7 * size * inch
  ex <- w * abs(cos(a)) + h * abs(sin(a))
  ey <- w * abs(sin(a)) + h * abs(cos(a))
  ok <- ok & X - ex >= 0 & X + ex <= W & Y - ey >= 0 & Y + ey <= H
  # Greedily, like MapLibre: a label only if it doesn't collide with one placed
  # before (with the style's padding), and the same road name only every 300px
  pad <- get("pad") * inch
  placed <- integer(0)
  for (i in which(ok)) {
    if (road[i]) {
      same <- placed[d$label[placed] == d$label[i] & road[placed]]
      if (any(sqrt((X[same] - X[i])^2 + (Y[same] - Y[i])^2) < 300 * inch)) next
    }
    fits <- TRUE
    for (j in placed) {
      if (!cm_boxes_separated(X[c(i, j)], Y[c(i, j)], w[c(i, j)] + pad[i], h[c(i, j)] + pad[i], a[c(i, j)])) {
        fits <- FALSE
        break
      }
    }
    if (fits) placed <- c(placed, i)
  }
  k <- placed
  if (length(k) == 0) return(grid::setChildren(x, grid::gList()))
  bold <- vapply(s[k], function(si) si$bold, logical(1))
  gp <- function(col, n = 1) grid::gpar(col = col, fontsize = rep(size[k] * inch * 72, n),
                                        fontface = rep(ifelse(bold, 2, 1), n), fontfamily = x$family)
  # White halo: the label drawn 16 times around its position
  t <- seq(0, 2 * pi, length.out = 17)[-17]
  r <- get("halo")[k] * inch
  halo <- grid::textGrob(rep(d$label[k], 16), x = grid::unit(rep(X[k], 16) + outer(r, cos(t)), "inches"),
                         y = grid::unit(rep(Y[k], 16) + outer(r, sin(t)), "inches"),
                         rot = rep(d$angle[k], 16), gp = gp("#ffffff", 16))
  text <- grid::textGrob(d$label[k], x = grid::unit(X[k], "inches"), y = grid::unit(Y[k], "inches"),
                         rot = d$angle[k], gp = gp(vapply(s[k], function(si) si$colour, "")))
  grid::setChildren(x, grid::gList(halo, text))
}

# Whether two rotated boxes (centres, half extents, angles) don't overlap, by
# the separating axis test.
cm_boxes_separated <- function(x, y, w, h, a) {
  # Quick accept if their circumcircles don't touch
  if (sqrt(diff(x)^2 + diff(y)^2) > sum(sqrt(w^2 + h^2))) return(TRUE)
  ux <- cbind(cos(a), sin(a))
  uy <- cbind(-sin(a), cos(a))
  for (u in list(ux[1, ], uy[1, ], ux[2, ], uy[2, ])) {
    r <- w * abs(ux %*% u) + h * abs(uy %*% u)
    if (abs(sum(c(diff(x), diff(y)) * u)) > sum(r)) return(TRUE)
  }
  FALSE
}

cm_interpolate <- function(z, stops, base = 1) {
  zs <- vapply(stops, function(s) s[1], numeric(1))
  vs <- vapply(stops, function(s) s[2], numeric(1))
  if (z <= zs[1]) return(vs[1])
  if (z >= zs[length(zs)]) return(vs[length(vs)])
  k <- findInterval(z, zs)
  t <- if (base == 1) (z - zs[k]) / (zs[k + 1] - zs[k])
       else (base^(z - zs[k]) - 1) / (base^(zs[k + 1] - zs[k]) - 1)
  vs[k] + t * (vs[k + 1] - vs[k])
}

#' Binned colour scale for choropleth maps, as on CensusMapper
#'
#' A discrete fill (or colour) scale for numeric values that bins them at
#' \code{breaks} and colours the bins from a palette, with legend labels for
#' the bins formatted for the kind of values shown. Values below the first
#' break go in the first bin and values at or above the last break in the
#' last, as in CensusMapper's maps. It extends \code{ggplot2::scale_fill_manual()}:
#' \code{values} sets the colours directly and \code{labels} the legend labels.
#'
#' @param palette The palette the colours come from: the name of a ColorBrewer
#' palette (e.g. \code{"Blues"} or \code{"YlOrRd"}, via RColorBrewer, as on
#' CensusMapper), of a viridis palette (\code{"viridis"}, \code{"magma"},
#' \code{"inferno"}, \code{"plasma"}, \code{"cividis"}, \code{"rocket"},
#' \code{"mako"} or \code{"turbo"}, via viridisLite), or of a palette of
#' \code{grDevices::hcl.colors()}. Or a function that returns n colours.
#' @param breaks The cutoffs between the bins, in increasing order. By default
#' \code{n} bins with about as many regions in each, at rounded cutoffs.
#' @param n The number of bins when there are no \code{breaks}.
#' @param direction 1 for the palette's colours in their order, -1 for the
#' reverse.
#' @param units What the values are, for formatting the labels:
#' \code{"number"} (1.2k, 3.4M), \code{"percentage"} (shares, 0.12 is 12\%),
#' \code{"percentage_100"} (percentages, 12 is 12\%), \code{"ratio"} (1,234.5),
#' \code{"currency"} ($1.2k) or \code{"currency_1000"} (currency in thousands).
#' @param lang Language for formatting the labels, \code{"en"} or \code{"fr"}.
#' @param values Colours for the bins, from the lowest to the highest,
#' instead of the palette's.
#' @param labels Labels for the bins instead of the ones made from
#' \code{breaks}: a character vector, or a function that gets the breaks and
#' returns the labels for the bins.
#' @param na.value The colour for missing values.
#' @param ... Other arguments passed on to \code{ggplot2::discrete_scale()},
#' e.g. \code{name} or \code{guide}.
#' @param aesthetics The aesthetics the scale works with, e.g.
#' \code{c("fill", "colour")}.
#'
#' @return A ggplot2 scale.
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' census_data <- get_census("CA21", regions = list(CMA = "59933"),
#'                           vectors = c(lim_at = "v_CA21_1085"),
#'                           level = "CT", geo_format = "sf")
#' ggplot(census_data) +
#'   geom_sf(aes(fill = lim_at / 100), colour = NA) +
#'   scale_fill_cm_bins(palette = "YlOrRd", breaks = c(0.05, 0.1, 0.15, 0.2),
#'                      units = "percentage") +
#'   coord_sf(datum = NA)
#' }
scale_fill_cm_bins <- function(palette = "Blues", breaks = NULL, n = 5, direction = 1,
                               units = c("number", "percentage", "percentage_100", "ratio",
                                         "currency", "currency_1000"),
                               lang = c("en", "fr"), values = NULL, labels = NULL,
                               na.value = "grey60", ..., aesthetics = "fill") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("scale_fill_cm_bins() needs the ggplot2 package")
  }
  units <- match.arg(units)
  lang <- match.arg(lang)
  if (!is.null(breaks)) {
    breaks <- sort(unique(as.numeric(breaks)))
    n <- length(breaks) + 1
  }
  if (!is.null(values) && length(values) < n) {
    stop("scale_fill_cm_bins() needs ", n, " values, one for each bin, got ", length(values))
  }
  pal <- function(k) {
    if (!is.null(values)) return(unname(values[seq_len(k)]))
    cm_palette(palette, k, direction)
  }
  ggplot2::discrete_scale(aesthetics = aesthetics, palette = pal, na.value = na.value,
                          drop = FALSE, ..., super = cm_bins_scale(breaks, n, units, lang, labels))
}

#' @rdname scale_fill_cm_bins
#' @export
scale_colour_cm_bins <- function(..., aesthetics = "colour") {
  scale_fill_cm_bins(..., aesthetics = aesthetics)
}

# The scale's class: a discrete scale that bins numeric values when ggplot
# transforms the data, before the scale is trained, so that the legend gets
# all bins, labelled with the ranges of values in them.
cm_bins_scale <- function(breaks, n, units, lang, labels) {
  ggplot2::ggproto(NULL, ggplot2::ScaleDiscrete,
    cm_breaks = breaks, cm_n = n, cm_units = units, cm_lang = lang, cm_labels = labels,
    transform = function(self, x) {
      if (!is.numeric(x)) return(x)
      if (is.null(self$cm_breaks)) self$cm_breaks <- cm_auto_breaks(x, self$cm_n)
      k <- length(self$cm_breaks) + 1
      factor(findInterval(x, self$cm_breaks), levels = seq_len(k) - 1)
    },
    get_labels = function(self, breaks = self$get_breaks()) {
      if (is.null(breaks)) return(NULL)
      b <- self$cm_breaks
      labels <- self$cm_labels
      if (is.function(labels)) labels <- labels(b)
      if (is.null(labels)) labels <- cm_bin_labels(b, self$cm_units, self$cm_lang)
      out <- labels[as.integer(as.character(breaks)) + 1]
      out[is.na(breaks)] <- "NA"
      out
    })
}

# Cutoffs for n bins with about the same number of values in each, rounded to
# two significant digits (or more if needed to keep them apart).
cm_auto_breaks <- function(x, n) {
  x <- x[is.finite(x)]
  if (length(x) == 0 || n < 2) return(numeric(0))
  q <- unique(stats::quantile(x, seq_len(n - 1) / n, names = FALSE))
  for (digits in 2:6) {
    b <- unique(signif(q, digits))
    if (length(b) == length(q)) break
  }
  b
}

# Labels for the bins between breaks: "< a", "a - b", ..., ">= z" (with an en
# dash and a greater-than-or-equal sign).
cm_bin_labels <- function(breaks, units = "number", lang = "en") {
  if (length(breaks) == 0) return("All")
  l <- cm_format_values(breaks, units, lang)
  k <- length(l)
  c(paste0("< ", l[1]),
    if (k > 1) paste0(l[-k], " \u2013 ", l[-1]),
    paste0("\u2265 ", l[k]))
}

# Values formatted for units like CensusMapper's legends. Each value gets as
# many significant digits as it needs, up to three (more if needed to keep the
# values apart), so that 125000 is $125k and 0.153 is 15.3%.
cm_format_values <- function(x, units = "number", lang = "en") {
  v <- x * switch(units, percentage = 100, currency_1000 = 1000, 1)
  short <- units %in% c("number", "currency", "currency_1000")
  fr <- lang == "fr"
  num <- function(r) {
    # digits = 15 shows the rounded values without floating point noise
    vapply(r, function(ri) format(ri, digits = 15, scientific = FALSE, trim = TRUE,
                                  big.mark = if (fr) "\u202f" else ",",
                                  decimal.mark = if (fr) "," else "."), "")
  }
  need <- vapply(v, function(vi) {
    for (d in 1:3) if (isTRUE(abs(signif(vi, d) - vi) <= 1e-9 * abs(vi))) return(d)
    3
  }, numeric(1))
  for (extra in 0:3) {
    digits <- need + extra
    r <- signif(v, digits)
    out <- if (short) {
      p <- pmin(3, pmax(0, floor(log10(pmax(abs(r), 1)) / 3)))
      suffix <- (if (fr) c("", "k", "M", "G") else c("", "k", "M", "B"))[p + 1]
      paste0(num(signif(abs(r) / 1000^p, digits)), suffix)
    } else {
      num(r)
    }
    if (!anyDuplicated(out)) break
  }
  neg <- if (short) ifelse(r < 0, "-", "") else ""
  switch(units,
    percentage = , percentage_100 = paste0(out, if (fr) "\u00a0%" else "%"),
    currency = , currency_1000 = if (fr) paste0(neg, out, "\u00a0$") else paste0(neg, "$", out),
    paste0(neg, out))
}

# n colours from a named palette (or a function), in the palette's order or,
# with direction -1, the reverse.
cm_palette <- function(palette, n, direction = 1) {
  viridis <- c("viridis", "magma", "inferno", "plasma", "cividis", "rocket", "mako", "turbo")
  cols <- if (is.function(palette)) {
    palette(n)
  } else if (palette %in% rownames(RColorBrewer::brewer.pal.info)) {
    max <- RColorBrewer::brewer.pal.info[palette, "maxcolors"]
    if (n < 3) {
      # ColorBrewer palettes start at 3 colours: their middle, or ends
      RColorBrewer::brewer.pal(3, palette)[list(2, c(1, 3))[[max(1, n)]]]
    } else if (n <= max) {
      RColorBrewer::brewer.pal(n, palette)
    } else {
      grDevices::colorRampPalette(RColorBrewer::brewer.pal(max, palette))(n)
    }
  } else if (tolower(palette) %in% viridis) {
    viridisLite::viridis(n, option = tolower(palette))
  } else if (palette %in% grDevices::hcl.pals()) {
    grDevices::hcl.colors(n, palette)
  } else {
    stop("Unknown palette '", palette, "': use a ColorBrewer, viridis or hcl.colors() palette, ",
         "or set the colours with values")
  }
  if (direction < 0) rev(cols) else cols
}
