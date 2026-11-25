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
#' The base map covers the area the plot shows: by default all its sf data, or
#' the limits set with \code{\link{coord_cm_bbox}()} or
#' \code{ggplot2::coord_sf()}. It is drawn in the plot's CRS, whatever CRS the
#' data are in.
#'
#' The tiles are downloaded when the plot is drawn and cached for the session.
#' They need a CensusMapper API key, the same as for the census data, though
#' tiles count against a separate tile quota. The key is taken from the
#' \code{CM_API_KEY} environment variable, set with
#' \code{\link{set_cancensus_api_key}()}.
#' Without an API key the plot is drawn without the base map, with a warning.
#'
#' Needs the ggplot2 package, and GDAL with its MVT driver for sf to read the
#' tiles. To style the base map differently, or to use it with other mapping
#' packages, get its features with \code{\link{cm_context}()}.
#'
#' @param layers Which parts of the base map to draw, any of \code{"landuse"},
#' \code{"water"}, \code{"buildings"}, \code{"roads"} and \code{"labels"}.
#' @param zoom Zoom level (as on CensusMapper) to draw the base map at. By
#' default the zoom level at which the area shown spans \code{pixels} pixels.
#' @param pixels The width, in pixels, of the CensusMapper map the plot mimics.
#' Larger values show more detail, with relatively smaller lines and labels.
#' @param scale Factor to make lines and labels larger or smaller.
#' @param language Language of the labels, \code{"en"} or \code{"fr"}. Defaults to
#' the \code{cancensus.language} option, and to \code{"en"} if that is not set.
#' @param family Font family of the labels. By default "Noto Sans", as on
#' CensusMapper, if the systemfonts package finds it, otherwise "sans".
#' @param tile_server Where to get the tiles from.
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
                            zoom = NULL, pixels = 800, scale = 1,
                            language = getOption("cancensus.language", "en"), family = NULL,
                            tile_server = "https://censusmapper.ca") {
  rlang::check_installed("ggplot2", reason = "for geom_cm_context().")
  layers <- match.arg(layers, several.ok = TRUE)
  structure(list(layers = layers, zoom = zoom, pixels = pixels, scale = scale,
                 language = validate_language(language), family = family, tile_server = tile_server),
            class = "cm_context_layer")
}

#' @exportS3Method ggplot2::ggplot_add
ggplot_add.cm_context_layer <- function(object, plot, ...) {
  # One layer that draws the whole base map when the panel is drawn, for the panel's extent, in the
  # CRS of the plot's coord_sf()
  geom <- ggplot2::ggproto("GeomCmContext", ggplot2::Geom,
    draw_key = ggplot2::draw_key_blank,
    draw_panel = function(data, panel_params, coord, cm) cm_context_grob(cm, panel_params, coord))
  plot <- plot + ggplot2::layer(geom = geom, stat = "identity", position = "identity",
                                data = data.frame(cm_context = 1), mapping = NULL, inherit.aes = FALSE,
                                show.legend = FALSE, params = list(cm = object))
  # Like geom_sf(), a map needs coord_sf()
  if (isTRUE(plot$coordinates$default)) plot <- plot + ggplot2::coord_sf(default = TRUE)
  plot
}

#' Show the area of a bounding box on a ggplot map
#'
#' A \code{ggplot2::coord_sf()} that limits a map to a bounding box, e.g. the
#' area of a CensusMapper map's view, while the data can extend beyond it. The
#' box can be in any CRS: it is turned into the map's CRS. With
#' \code{bbox = NULL} the map shows all its data, as with \code{coord_sf()},
#' so that cropping is switched off by setting the box to \code{NULL}.
#' \code{\link{geom_cm_context}()} covers the area shown.
#'
#' @param bbox The area to show: a bounding box (\code{sf::st_bbox()}), or sf
#' data or an sfc whose bounding box to show, or a numeric vector
#' \code{c(xmin, ymin, xmax, ymax)}. A box without a CRS, and a numeric vector,
#' are taken to be in the CRS of the map's data (of the plot, or of its first
#' sf layer). \code{NULL} to show all data.
#' @param crs The CRS of the map. By default that of the map's data.
#' @param datum The CRS of the graticule; by default none.
#' @param ... Other arguments passed on to \code{ggplot2::coord_sf()}, e.g.
#' \code{expand}.
#'
#' @return An object to add to a ggplot with \code{+}.
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' census_data <- get_census("CA21", regions = list(CMA = "59933"),
#'                           vectors = c(population = "v_CA21_1"),
#'                           level = "CT", geo_format = "sf")
#' view_bbox <- sf::st_bbox(c(xmin = -123.19, ymin = 49.24, xmax = -123.05, ymax = 49.30),
#'                          crs = 4326)
#' ggplot(census_data) +
#'   geom_sf(aes(fill = population / `Shape Area`), colour = NA) +
#'   geom_cm_context() +
#'   coord_cm_bbox(view_bbox)
#' }
coord_cm_bbox <- function(bbox = NULL, crs = NULL, datum = NA, ...) {
  rlang::check_installed("ggplot2", reason = "for coord_cm_bbox().")
  if (!is.null(bbox)) {
    if (inherits(bbox, c("sf", "sfc"))) {
      bbox <- sf::st_bbox(bbox)
    } else if (!inherits(bbox, "bbox")) {
      if (!is.numeric(bbox) || length(bbox) != 4) {
        cc_abort("cm_bbox_invalid")
      }
      if (!is.null(names(bbox))) bbox <- bbox[c("xmin", "ymin", "xmax", "ymax")]
      bbox <- sf::st_bbox(stats::setNames(as.numeric(bbox), c("xmin", "ymin", "xmax", "ymax")))
    }
    if (anyNA(as.numeric(bbox))) cc_abort("cm_bbox_missing_values")
  }
  structure(list(bbox = bbox, crs = crs, datum = datum, args = list(...)), class = "cm_bbox_coord")
}

#' @exportS3Method ggplot2::ggplot_add
ggplot_add.cm_bbox_coord <- function(object, plot, ...) {
  b <- object$bbox
  args <- c(list(crs = object$crs, datum = object$datum), object$args)
  if (!is.null(b)) {
    box_crs <- sf::st_crs(b)
    # A box without a CRS is in the CRS of the map's data
    if (is.na(box_crs)) {
      data <- plot$data
      if (!inherits(data, c("sf", "sfc"))) {
        data <- Find(function(l) inherits(l$data, c("sf", "sfc")), plot$layers)$data
      }
      if (!is.null(data)) box_crs <- sf::st_crs(data)
      if (!is.na(box_crs)) cc_warn("cm_bbox_no_crs_data", crs = cm_crs_name(box_crs))
      else cc_warn("cm_bbox_no_crs_map")
    }
    # Limits in the box's CRS, turned into the map's CRS by coord_sf(); the
    # area shown covers all of the box. Without any CRS the limits are in the
    # map's CRS.
    args <- c(list(xlim = as.numeric(b[c("xmin", "xmax")]), ylim = as.numeric(b[c("ymin", "ymax")]),
                   default_crs = if (!is.na(box_crs)) box_crs, lims_method = "box"),
              args)
  }
  plot + do.call(ggplot2::coord_sf, args)
}

# A CRS's EPSG code, e.g. "EPSG:4326", or else its name
cm_crs_name <- function(crs) {
  if (!is.na(crs$epsg)) paste0("EPSG:", crs$epsg) else crs$Name
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

# The base map for a panel, as grobs. The panel's extent (x_range, y_range)
# is in the panel's CRS, which coord_sf() takes from its crs argument or the
# first sf layer; the features are turned into that CRS, so the base map
# matches the data whatever CRS the data, the coord or its limits are in.
cm_context_grob <- function(o, panel_params, coord) {
  crs <- panel_params$crs
  if (!inherits(coord, "CoordSf") || is.null(crs) || is.na(crs)) {
    cc_abort("cm_context_needs_coord_sf")
  }
  xr <- panel_params$x_range
  yr <- panel_params$y_range
  panel <- sf::st_bbox(c(xmin = xr[1], ymin = yr[1], xmax = xr[2], ymax = yr[2]), crs = crs)
  # Width of the panel in web mercator, along its middle
  mid <- sf::st_sfc(sf::st_point(c(xr[1], mean(yr))), sf::st_point(c(xr[2], mean(yr))), crs = crs)
  m <- sf::st_coordinates(sf::st_transform(mid, 3857))
  span <- sqrt(sum((m[2, 1:2] - m[1, 1:2])^2))
  z <- o$zoom
  if (is.null(z)) z <- log2(o$pixels * 2 * pi * 6378137 / (512 * span)) # MapLibre's 512px tiles
  v <- cm_view(panel, z)
  v$span <- span
  f <- cm_context_features_cached(v, o)

  # Features with their geometry in npc, as a plain data frame
  npc <- function(d) {
    g <- sf::st_geometry(d)
    d <- sf::st_drop_geometry(d)
    d$geometry <- g
    coord$transform(d, panel_params)
  }
  children <- lapply(f, function(p) {
    d <- npc(p$d)
    switch(p$type,
      fill = {
        key <- paste(d$fill, d$alpha)
        do.call(grid::gList, lapply(split(seq_len(nrow(d)), key), function(i) {
          sf::st_as_grob(d$geometry[i], gp = grid::gpar(col = NA, fill = ggplot2::alpha(d$fill[i[1]], d$alpha[i[1]])))
        }))
      },
      lines = grid::gTree(data = d, styles = p$styles, span = v$span, z = v$z,
                          scale = o$scale, cl = "cm_lines"),
      labels = {
        xy <- sf::st_coordinates(d$geometry)
        d$x <- xy[, 1]
        d$y <- xy[, 2]
        # The direction of road labels in the panel: a point a little way along
        # the road (whose angle is in web mercator), in the panel's CRS
        d$dx <- 1
        d$dy <- 0
        road <- !is.na(d$length)
        if (any(road)) {
          pm <- sf::st_coordinates(sf::st_transform(p$d[road, ], 3857))
          step <- 2 * pi * 6378137 / (512 * 2^v$z)
          a <- d$angle[road] * pi / 180
          ahead <- sf::st_as_sf(data.frame(x = pm[, 1] + step * cos(a), y = pm[, 2] + step * sin(a)),
                                coords = c("x", "y"), crs = 3857)
          ahead <- sf::st_coordinates(npc(sf::st_transform(ahead, crs))$geometry)
          d$dx[road] <- ahead[, 1] - d$x[road]
          d$dy[road] <- ahead[, 2] - d$y[road]
        }
        d$geometry <- NULL
        grid::gTree(data = d, styles = p$styles, span = v$span, z = v$z,
                    family = p$family, scale = o$scale, cl = "cm_labels")
      })
  })
  grid::gTree(children = do.call(grid::gList, Filter(Negate(is.null), children)))
}

# The base map's features are kept for the session, for redrawing plots
.cm_context_cache <- new.env(parent = emptyenv())

cm_context_features_cached <- function(v, o) {
  key <- digest::digest(list(as.numeric(v$bbox), v$crs$wkt, v$z,
                             o[c("layers", "language", "family", "tile_server")]))
  if (is.null(.cm_context_cache[[key]])) {
    # Without an API key the plot is drawn without its base map
    if (!valid_api_key(robust_api_key(""))) {
      cc_warn("cm_context_no_api_key", caller = "geom_cm_context()")
      return(list())
    }
    if (length(ls(.cm_context_cache)) >= 20) rm(list = ls(.cm_context_cache), envir = .cm_context_cache)
    .cm_context_cache[[key]] <- cm_context_features(v, o)
  }
  .cm_context_cache[[key]]
}

# The features of the base map for view v, styled like CensusMapper, in the
# order to draw them: a list of fills, lines and labels.
cm_context_features <- function(v, o) {
  z <- v$z
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
  ctx <- cm_context_data(NULL, need, v, o$language, o$tile_server, robust_api_key(""),
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
    list(type = "fill", d = d)
  }
  # Lines with widths (in pixels, by zoom) from styles[[d$style]], drawn to scale
  line_layer <- function(d, styles) {
    if (is.null(d) || nrow(d) == 0) return(NULL)
    list(type = "lines", d = d, styles = styles)
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
      lab$priority <- seq_len(nrow(lab))
      out <- c(out, list(list(type = "labels", d = lab, styles = styles, family = family)))
    }
  }
  Filter(Negate(is.null), out)
}

# Labels and lines are grobs that are laid out when drawn, when the panel's size, and with it the
# size of a CensusMapper pixel, is known.

# Size of a CensusMapper pixel at zoom z in the panel being drawn, in inches
cm_inch_per_px <- function(x) {
  W <- grid::convertWidth(grid::unit(1, "npc"), "inches", valueOnly = TRUE)
  W / (2^x$z * 512 * x$span / (2 * pi * 6378137)) * x$scale
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
  # The labels' angles on the page, from their direction in the panel (dx, dy
  # in npc), turned to read left to right
  d$angle <- atan2(d$dy * H, d$dx * W) * 180 / pi
  d$angle <- ifelse(d$angle > 90, d$angle - 180, ifelse(d$angle < -90, d$angle + 180, d$angle))
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
#' The legend shows every bin in its colour, including bins without data. If
#' you pass your own \code{guide}, add \code{override.aes = list(.draw = TRUE)}
#' to it, or \code{show.legend = TRUE} to the layer, to keep this with
#' ggplot2 3.5 or later.
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
#' \code{"percentage_100"} (percentages, 12 is 12\%), \code{"ratio"} (1,230, 0.153),
#' \code{"currency"} ($1.2k) or \code{"currency_1000"} (currency in thousands).
#' Labels show up to three significant digits, more only where needed to tell
#' the breaks apart.
#' @param language Language for formatting the labels, \code{"en"} or \code{"fr"}.
#' Defaults to the \code{cancensus.language} option, and to \code{"en"} if that
#' is not set.
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
                               language = getOption("cancensus.language", "en"), values = NULL, labels = NULL,
                               na.value = "grey60", ..., aesthetics = "fill") {
  rlang::check_installed("ggplot2", reason = "for scale_fill_cm_bins().")
  units <- match.arg(units)
  lang <- validate_language(language)
  if (!is.null(breaks)) {
    breaks <- sort(unique(as.numeric(breaks)))
    n <- length(breaks) + 1
  }
  if (!is.null(values) && length(values) < n) {
    cc_abort("cm_bins_values_count", n = n, got = length(values))
  }
  pal <- function(k) {
    if (!is.null(values)) return(unname(values[seq_len(k)]))
    cm_palette(palette, k, direction)
  }
  args <- list(...)
  if (is.null(args$guide)) {
    # Since ggplot2 3.5 the legend leaves keys without data in the layer blank
    # (via the internal .draw column) unless the layer has show.legend = TRUE.
    # Bins should always show the whole colour ramp, so draw every key.
    args$guide <- ggplot2::guide_legend(override.aes = list(.draw = TRUE))
  }
  do.call(ggplot2::discrete_scale,
          c(list(aesthetics = aesthetics, palette = pal, na.value = na.value, drop = FALSE),
            args, list(super = cm_bins_scale(breaks, n, units, lang, labels))))
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
  if (length(breaks) == 0) return(cc_msg("cm_bins_all", .language = lang))
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

# The names of the ColorBrewer palettes, as in rownames(RColorBrewer::brewer.pal.info), so that
# palette names can be recognized without RColorBrewer installed
cm_brewer_palettes <- c("BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn", "Spectral",
                        "Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2", "Set3",
                        "Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges", "OrRd", "PuBu",
                        "PuBuGn", "PuRd", "Purples", "RdPu", "Reds", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd")

# n colours from a named palette (or a function), in the palette's order or,
# with direction -1, the reverse.
cm_palette <- function(palette, n, direction = 1) {
  viridis <- c("viridis", "magma", "inferno", "plasma", "cividis", "rocket", "mako", "turbo")
  cols <- if (is.function(palette)) {
    palette(n)
  } else if (palette %in% cm_brewer_palettes) {
    rlang::check_installed("RColorBrewer", reason = paste0("for the ColorBrewer palette '", palette, "'."))
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
    rlang::check_installed("viridisLite", reason = paste0("for the viridis palette '", palette, "'."))
    viridisLite::viridis(n, option = tolower(palette))
  } else if (palette %in% grDevices::hcl.pals()) {
    grDevices::hcl.colors(n, palette)
  } else {
    cc_abort("cm_unknown_palette", palette = palette)
  }
  if (direction < 0) rev(cols) else cols
}
