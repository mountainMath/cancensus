# Helpers for maps made with census data in the style of CensusMapper

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

#' The features of CensusMapper's base map as sf data
#'
#' Gets the features of CensusMapper's base map for the extent of some sf data
#' or a bounding box from CensusMapper's vector tiles: water, landcover,
#' landuse, airports, buildings, roads, and places and road labels. These are
#' the features \code{\link{geom_cm_context}()} draws, unstyled, for drawing
#' them in other ways or with other mapping packages.
#'
#' The tiles, and with them the detail of the features, are those CensusMapper
#' uses at the zoom level at which the data spans \code{pixels} pixels, or at
#' \code{zoom}. Which classes of features exist at a zoom level is decided by
#' the tiles; \code{\link{geom_cm_context}()} additionally shows some classes
#' (minor roads, buildings, road labels) only at higher zoom levels.
#'
#' Tiles are downloaded from the tile server and cached for the session. They
#' need a CensusMapper API key, the same as for the census data, though tiles
#' count against a separate tile quota. Needs GDAL with its MVT driver for sf
#' to read the tiles.
#'
#' @param data sf data, an sfc or a bounding box whose extent to get the base
#' map for.
#' @param layers Which layers to get, any of \code{"water"},
#' \code{"landcover"}, \code{"landuse"}, \code{"aeroway"}, \code{"buildings"},
#' \code{"roads"}, \code{"places"} and \code{"road_labels"}.
#' @param zoom Zoom level (as on CensusMapper) whose tiles to use. By default
#' the zoom level at which the data spans \code{pixels} pixels.
#' @param pixels The width, in pixels, of the CensusMapper map whose zoom level
#' to use.
#' @param lang Language of the names, \code{"en"} or \code{"fr"}.
#' @param tile_server Where to get the tiles from.
#' @param api_key An API key for the CensusMapper API. Defaults to the
#' \code{CM_API_KEY} environment variable, as set with
#' \code{\link{set_cancensus_api_key}()}.
#'
#' @return A named list with an sf data frame for each layer, in the CRS of
#' \code{data} and cropped to its bounding box. A layer without features is an
#' sf data frame without rows. Features have their \code{class}, and places and
#' road labels their \code{name} in the language \code{lang} (or the local
#' name). Road labels are points along named roads, with the \code{angle} (in
#' degrees) to turn labels by and the \code{length} of the road (in metres). The
#' other columns come from the tiles and may change. The zoom level is in the
#' \code{"zoom"} attribute.
#' @seealso \code{\link{geom_cm_context}()} to add the base map to a ggplot map.
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' census_data <- get_census("CA21", regions = list(CSD = "5915022"),
#'                           vectors = c(population = "v_CA21_1"),
#'                           level = "DA", geo_format = "sf")
#' context <- cm_context(census_data, layers = c("water", "roads", "places"))
#' major <- context$roads[context$roads$class %in% c("motorway", "trunk", "primary"), ]
#' ggplot(census_data) +
#'   geom_sf(aes(fill = population / `Shape Area`), colour = NA) +
#'   geom_sf(data = context$water, fill = "lightblue", colour = NA) +
#'   geom_sf(data = major, colour = "grey30") +
#'   geom_sf_text(data = context$places[context$places$class == "suburb", ],
#'                aes(label = name), size = 3) +
#'   coord_sf(datum = NA)
#' }
cm_context <- function(data,
                       layers = c("water", "landcover", "landuse", "aeroway", "buildings",
                                  "roads", "places", "road_labels"),
                       zoom = NULL, pixels = 800, lang = c("en", "fr"),
                       tile_server = "https://censusmapper.ca",
                       api_key = Sys.getenv("CM_API_KEY")) {
  if (!inherits(data, c("sf", "sfc", "bbox"))) stop("cm_context() needs sf data, an sfc or a bounding box")
  layers <- match.arg(layers, several.ok = TRUE)
  api_key <- cm_tile_api_key(api_key, "cm_context()")
  cm_context_data(data, layers, cm_view(data, zoom, pixels), match.arg(lang), tile_server, api_key)
}

# The API key for the tiles, or an error before any tiles are fetched
cm_tile_api_key <- function(api_key, caller) {
  api_key <- robust_api_key(api_key)
  if (!valid_api_key(api_key)) {
    stop(caller, " needs a CensusMapper API key for the base map tiles. ",
         "Set it with set_cancensus_api_key() or pass it with the api_key argument. ",
         "Get a free API key at https://censusmapper.ca/users/sign_up", call. = FALSE)
  }
  api_key
}

# The data's CRS and bbox, its extent and width in web mercator, the zoom level
# to draw at and the zoom level of the tiles
cm_view <- function(data, zoom = NULL, pixels = 800) {
  world <- 2 * pi * 6378137
  merc <- sf::st_bbox(sf::st_transform(sf::st_as_sfc(sf::st_bbox(data)), 3857))
  span <- merc[["xmax"]] - merc[["xmin"]]
  z <- zoom
  if (is.null(z)) z <- log2(pixels * world / (512 * span)) # MapLibre's 512px tiles
  z <- max(0, min(z, 20))
  list(crs = sf::st_crs(data), bbox = sf::st_bbox(data), merc = merc, span = span,
       z = z, tz = min(13, floor(z)))
}

# The layers of the base map for view v. Road labels only for
# road_label_classes, if given.
cm_context_data <- function(data, layers, v, lang, server, api_key, road_label_classes = NULL) {
  get <- function(layer, source = "canada", zoom = v$tz) {
    d <- cm_tile_layer(v$merc, zoom, layer, source, server, api_key)
    if (is.null(d) || nrow(d) == 0) return(NULL)
    # Crop in a planar CRS; a lon/lat box is the same box in web mercator
    planar <- sf::st_is_longlat(v$crs)
    if (!planar) d <- sf::st_transform(d, v$crs)
    crop <- function(d) suppressWarnings(sf::st_crop(d, if (planar) v$merc else v$bbox))
    d <- tryCatch(crop(d), error = function(e) crop(sf::st_make_valid(d)))
    d <- d[!sf::st_is_empty(d), ]
    if (planar) d <- sf::st_transform(d, v$crs)
    d
  }
  # Names in the language lang, or the local name
  named <- function(d) {
    if (is.null(d)) return(NULL)
    n <- if (lang == "fr" && "name.fr" %in% names(d)) d$name.fr else if ("name.en" %in% names(d)) d$name.en
    if (is.null(n)) n <- d$name
    d$name <- ifelse(is.na(n) | n == "", d$name, n)
    d[!is.na(d$name) & d$name != "", ]
  }
  one <- function(layer) switch(layer,
    water = {
      water <- if (v$tz >= 6) get("water")
      if (v$z < 9) water <- cm_rbind(get("water", "world_low", min(v$tz, 8)), water)
      water
    },
    landcover = get("landcover_natural"),
    landuse = get("landuse"),
    aeroway = get("aeroway"),
    buildings = get("building"),
    roads = {
      roads <- get("transportation")
      if (!is.null(roads)) roads <- roads[!sf::st_is(roads, c("POLYGON", "MULTIPOLYGON", "POINT", "MULTIPOINT")), ]
      roads
    },
    places = named(get("place")),
    road_labels = {
      roads <- named(get("transportation_name"))
      if (!is.null(roads) && !is.null(road_label_classes)) roads <- roads[roads$class %in% road_label_classes, ]
      if (!is.null(roads) && nrow(roads) > 0) cm_road_label_points(roads, v$z)
    })
  empty <- sf::st_sf(geometry = sf::st_sfc(crs = v$crs))
  out <- lapply(stats::setNames(layers, layers), function(layer) {
    d <- one(layer)
    if (is.null(d) || nrow(d) == 0) empty else d
  })
  attr(out, "zoom") <- v$z
  out
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

# Label candidates for named roads: one per merged piece of a road (same class
# and name) that is long enough for its label at zoom z, at the piece's
# midpoint and turned along it.
cm_road_label_points <- function(d, z) {
  crs <- sf::st_crs(d)
  d <- sf::st_transform(d, 3857)
  key <- paste(d$class, d$name, sep = "|")
  pieces <- lapply(split(seq_len(nrow(d)), key), function(i) {
    g <- sf::st_union(sf::st_geometry(d)[i])
    g <- sf::st_cast(sf::st_line_merge(sf::st_cast(g, "MULTILINESTRING")), "LINESTRING")
    data.frame(row = i[1], geometry = g)
  })
  p <- sf::st_sf(do.call(rbind, pieces), crs = 3857)
  m_per_px <- 2 * pi * 6378137 / (512 * 2^z)
  p <- p[as.numeric(sf::st_length(p)) >= nchar(d$name[p$row]) * 10 * 0.6 * m_per_px, ]
  if (nrow(p) == 0) return(NULL)
  coords <- do.call(rbind, lapply(sf::st_geometry(p), function(l) {
    xy <- sf::st_coordinates(l)[, 1:2, drop = FALSE]
    s <- c(0, cumsum(sqrt(rowSums(diff(xy)^2))))
    h <- s[length(s)] / 2
    k <- max(1, findInterval(h, s, all.inside = TRUE))
    t <- (h - s[k]) / (s[k + 1] - s[k])
    pt <- xy[k, ] + t * (xy[k + 1, ] - xy[k, ])
    a <- atan2(xy[k + 1, 2] - xy[k, 2], xy[k + 1, 1] - xy[k, 1]) * 180 / pi
    if (a > 90) a <- a - 180
    if (a < -90) a <- a + 180
    c(pt, a)
  }))
  out <- data.frame(class = d$class[p$row], name = d$name[p$row], angle = coords[, 3],
                    length = as.numeric(sf::st_length(p)))
  out <- sf::st_sf(out, geometry = sf::st_sfc(lapply(seq_len(nrow(coords)), function(i)
    sf::st_point(coords[i, 1:2])), crs = 3857))
  sf::st_transform(out, crs)
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

cm_rbind <- function(...) {
  ds <- Filter(function(d) !is.null(d) && nrow(d) > 0, list(...))
  if (length(ds) == 0) return(NULL)
  cols <- unique(unlist(lapply(ds, function(d) setdiff(names(d), attr(d, "sf_column")))))
  ds <- lapply(ds, function(d) {
    g <- sf::st_geometry(d)
    d <- sf::st_drop_geometry(d)
    for (c in setdiff(cols, names(d))) d[[c]] <- NA
    sf::st_sf(d[cols], geometry = g)
  })
  do.call(rbind, ds)
}

# The features of one tile layer for a bounding box in EPSG:3857. Tiles are
# cached for the session, by source, zoom and tile (not by API key).
cm_tile_layer <- function(merc, z, layer, source, server, api_key) {
  world <- 2 * pi * 6378137
  n <- 2^z
  tx <- function(x) min(n - 1, max(0, floor((x + world / 2) / world * n)))
  ty <- function(y) min(n - 1, max(0, floor((world / 2 - y) / world * n)))
  xs <- tx(merc$xmin):tx(merc$xmax)
  ys <- ty(merc$ymax):ty(merc$ymin)
  if (length(xs) * length(ys) > 100) {
    warning("geom_cm_context(): ", length(xs) * length(ys), " tiles at zoom ", z,
            "; set a lower zoom or fewer pixels")
  }
  dir <- file.path(tempdir(), "cm_tiles", source, z)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  tiles <- expand.grid(x = xs, y = ys)
  ds <- lapply(seq_len(nrow(tiles)), function(i) {
    x <- tiles$x[i]
    y <- tiles$y[i]
    path <- file.path(dir, paste0(x, "_", y, ".pbf"))
    if (!file.exists(path)) {
      cm_download_tile(sprintf("%s/%s/%d/%d/%d", server, source, z, x, y), api_key, path)
    }
    if (file.size(path) == 0) return(NULL)
    opts <- c(paste0("X=", x), paste0("Y=", y), paste0("Z=", z))
    # A tile only has the layers it has features for. GDAL's notes on the
    # tiles go to stderr; keep them out of the console.
    d <- NULL
    utils::capture.output(utils::capture.output(type = "message", {
      if (layer %in% sf::st_layers(path, options = opts)$name) {
        d <- suppressWarnings(sf::st_read(path, layer = layer, options = opts, quiet = TRUE))
      }
    }))
    d
  })
  d <- do.call(cm_rbind, ds)
  if (!is.null(d)) d <- d[!sf::st_is_empty(d), ]
  d
}

# Download a tile to path. Stops if the server refuses the API key or the tile
# quota is used up (401, 403), or on other failures, so that a base map isn't
# silently left empty. An empty tile (no features) is an empty file.
cm_download_tile <- function(url, api_key, path) {
  tmp <- paste0(path, ".part")
  on.exit(unlink(tmp))
  response <- tryCatch(cm_GET(url, httr::write_disk(tmp, overwrite = TRUE),
                              query = list(api_key = api_key)),
                       error = function(e) {
                         stop("geom_cm_context(): could not download base map tile ", url, ": ",
                              conditionMessage(e), call. = FALSE)
                       })
  status <- httr::status_code(response)
  if (status %in% c(401, 403)) {
    reason <- httr::headers(response)[["x-tile-auth-reason"]]
    stop("geom_cm_context(): the CensusMapper tile server refused the base map tiles (HTTP ", status,
         if (!is.null(reason) && nzchar(reason)) paste0(": ", reason), "). ",
         if (status == 401) "Check your API key, set with set_cancensus_api_key()."
         else "Your API key may be inactive, or its tile quota used up.",
         call. = FALSE)
  }
  if (httr::http_error(response)) {
    stop("geom_cm_context(): could not download base map tile ", url, " (HTTP ", status, ")",
         call. = FALSE)
  }
  # An empty response (e.g. 204) may not leave a file behind
  if (!file.exists(tmp)) file.create(tmp)
  file.rename(tmp, path)
  invisible(path)
}

# CensusMapper's functions for function strings that have no short vectorised
# R form. Each takes its vectors as matrices with one row per region (e.g.
# cbind(a, b, c) inside dplyr::mutate()), or as a plain vector that is the same
# for every region, and returns one value per region. As on CensusMapper, any
# missing input gives NA. The reference implementations are in CensusMapper's
# components/customFunctions.tsx.

#' Gini coefficient from income brackets, as on CensusMapper
#'
#' CensusMapper's \code{gini()}: the Gini coefficient from counts per income
#' bracket and the brackets' lower cutoffs, one value per region. Incomes are
#' taken at the bracket midpoints. The top bracket's income is twice its lower
#' cutoff less the previous cutoff, or is estimated from the total income when
#' that is given.
#'
#' @param counts Counts per bracket: a matrix with one row per region and one
#' column per bracket, e.g. \code{cbind(v1, v2, v3)} inside
#' \code{dplyr::mutate()}.
#' @param lower_cutoffs The brackets' lower cutoffs, a vector that is the same
#' for every region, or a matrix like \code{counts}.
#' @param total Total income per region, or 0 (the default) for none.
#'
#' @return A numeric vector with one value per region, NA where any input is
#' missing.
#' @export
#'
#' @examples
#' cm_gini(cbind(c(10, 5), c(20, 10), c(5, 20)), c(0, 20000, 50000))
cm_gini <- function(counts, lower_cutoffs, total = 0) {
  cm_per_region(counts, lower_cutoffs, total, function(x, cut, total) {
    k <- length(cut)
    income <- c((cut[-k] + cut[-1]) / 2, 2 * cut[k] - cut[k - 1])
    if (total != 0 && x[k] > 0) {
      top <- (total - sum(income[-k] * x[-k])) / x[k]
      if (top > cut[k]) income[k] <- if (top > income[k]) top else (top + income[k]) / 2
    }
    share <- x / sum(x)
    s <- c(0, cumsum(income * share))
    1 - sum(share * (s[-(k + 1)] + s[-1])) / s[k + 1]
  })
}

#' Gini coefficient from income brackets, CensusMapper's gini2()
#'
#' Like \code{\link{cm_gini}}, but the top bracket's income total is the larger
#' of its estimate from the bracket width and what the total income leaves
#' after the other brackets.
#'
#' @inheritParams cm_gini
#' @inherit cm_gini return
#' @export
#'
#' @examples
#' cm_gini2(cbind(c(10, 5), c(20, 10), c(5, 20)), c(0, 20000, 50000), c(1e6, 2e6))
cm_gini2 <- function(counts, lower_cutoffs, total = 0) {
  cm_per_region(counts, lower_cutoffs, total, function(x, cut, total) {
    k <- length(cut)
    income <- c(x[-k] * (cut[-k] + cut[-1]) / 2, x[k] * (2 * cut[k] - cut[k - 1]))
    if (total != 0 && x[k] > 0) income[k] <- max(income[k], total - sum(income[-k]))
    s <- cumsum(income)
    1 - sum(x * (s + c(0, s[-k]))) / s[k] / sum(x)
  })
}

#' Quantile from brackets, as on CensusMapper
#'
#' CensusMapper's \code{binned_quantile()} (and \code{quantile()}): the
#' p-quantile, e.g. the median income, from counts per bracket and the brackets'
#' lower cutoffs, interpolating linearly within the bracket. The lowest bracket
#' starts at 0 and the top bracket ends at twice its lower cutoff.
#'
#' @inheritParams cm_gini
#' @param p The quantile, between 0 and 1, the same for every region or one per
#' region.
#'
#' @return A numeric vector with one value per region, NA where any input is
#' missing and NaN where there are no counts.
#' @export
#'
#' @examples
#' cm_binned_quantile(cbind(c(10, 5), c(20, 10), c(5, 20)), c(0, 20000, 50000), 0.5)
cm_binned_quantile <- function(counts, lower_cutoffs, p) {
  cm_per_region(counts, lower_cutoffs, p, function(x, cut, p) {
    target <- sum(x) * p
    if (!(target > 0)) return(NaN)
    k <- length(x)
    cum <- cumsum(x)
    j <- which(cum >= target)[1]
    if (is.na(j)) j <- k
    below <- if (j > 1) cum[j - 1] else 0
    lo <- if (j > 1) cut[j] else 0
    hi <- if (j < k) cut[j + 1] else 2 * cut[k]
    a <- (target - below) / (cum[j] - below)
    hi * a + lo * (1 - a)
  })
}

#' Wasserstein distance per region, as on CensusMapper
#'
#' CensusMapper's \code{wasserstein()}: the p-Wasserstein distance between two
#' samples of the same size for each region, using
#' \code{transport::wasserstein1d()}. Needs the transport package.
#'
#' @param a,b The samples: matrices with one row per region, e.g.
#' \code{cbind(v1, v2, v3)} inside \code{dplyr::mutate()}, or vectors that are
#' the same for every region.
#' @param p The order of the distance, the same for every region or one per
#' region.
#'
#' @return A numeric vector with one value per region, NA where any input is
#' missing.
#' @export
#'
#' @examples
#' \dontrun{
#' cm_wasserstein(cbind(c(1, 2), c(3, 4)), c(2, 2))
#' }
cm_wasserstein <- function(a, b, p = 1) {
  if (!requireNamespace("transport", quietly = TRUE)) {
    stop("cm_wasserstein() needs the transport package: install.packages(\"transport\")")
  }
  cm_per_region(a, b, p, function(a, b, p) transport::wasserstein1d(a, b, p = p))
}

# x as a matrix with n rows: a matrix with n rows as is, or a vector (or
# one-row matrix) repeated for every row
cm_rows <- function(x, n) {
  if (is.matrix(x) && nrow(x) == n) return(x)
  if (is.matrix(x) && nrow(x) == 1) x <- x[1, ]
  matrix(x, nrow = n, ncol = length(x), byrow = TRUE)
}

# Applies f(x, y, value) to each region's rows of the vectors x and y and its
# single value (a column, or one value for all regions); NA if any is missing.
cm_per_region <- function(x, y, value, f) {
  n <- max(1, unlist(lapply(list(x, y), function(v) if (is.matrix(v)) nrow(v))), length(value))
  x <- cm_rows(x, n)
  y <- cm_rows(y, n)
  value <- rep_len(value, n)
  vapply(seq_len(n), function(i) {
    if (anyNA(c(x[i, ], y[i, ], value[i]))) NA_real_ else f(x[i, ], y[i, ], value[i])
  }, numeric(1))
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
