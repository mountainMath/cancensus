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
#' Tiles are downloaded from the tile server and cached for the session.
#' Needs the ggplot2 package, and GDAL with its MVT driver for sf to read the
#' tiles.
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
#'
#' @return An object to add to a ggplot with \code{+}.
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
                            tile_server = "https://censusmapper.ca") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("geom_cm_context() needs the ggplot2 package")
  }
  layers <- match.arg(layers, several.ok = TRUE)
  structure(list(layers = layers, data = data, zoom = zoom, pixels = pixels, scale = scale,
                 lang = match.arg(lang), family = family, tile_server = tile_server),
            class = "cm_context")
}

#' @exportS3Method ggplot2::ggplot_add
ggplot_add.cm_context <- function(object, plot, ...) {
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

cm_context_layers <- function(data, o) {
  crs <- sf::st_crs(data)
  box <- sf::st_as_sfc(sf::st_bbox(data))
  merc <- sf::st_bbox(sf::st_transform(box, 3857))
  world <- 2 * pi * 6378137
  # The data's bbox, and its width in web mercator, to scale lines and labels
  # to the panel when drawn
  bbox <- as.numeric(sf::st_bbox(data))
  span <- merc[["xmax"]] - merc[["xmin"]]
  z <- o$zoom
  if (is.null(z)) z <- log2(o$pixels * world / (512 * span)) # MapLibre's 512px tiles
  z <- max(0, min(z, 20))
  tz <- min(13, floor(z))

  get <- function(layer, source = "canada", zoom = tz) {
    d <- cm_tile_layer(merc, zoom, layer, source, o$tile_server)
    if (is.null(d) || nrow(d) == 0) return(NULL)
    # Crop in a planar CRS; a lon/lat box is the same box in web mercator
    planar <- sf::st_is_longlat(crs)
    if (!planar) d <- sf::st_transform(d, crs)
    crop <- function(d) suppressWarnings(sf::st_crop(d, if (planar) merc else sf::st_bbox(data)))
    d <- tryCatch(crop(d), error = function(e) crop(sf::st_make_valid(d)))
    d <- d[!sf::st_is_empty(d), ]
    if (planar) d <- sf::st_transform(d, crs)
    d
  }
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

  if ("landuse" %in% o$layers && z > 9) {
    a <- ramp(list(c(9, 0), c(10, 0.85)))
    natural <- rows(get("landcover_natural"), fill = "#a8a8a8", alpha = a)
    industrial <- get("landuse")
    industrial <- rows(industrial[industrial$class %in% "industrial", ], fill = "#9a9a9a", alpha = a)
    out <- c(out, list(fill_layer(cm_rbind(natural, industrial))))
  }
  if ("water" %in% o$layers) {
    water <- if (tz >= 6) get("water")
    if (z < 9) water <- cm_rbind(get("water", "world_low", min(tz, 8)), water)
    out <- c(out, list(fill_layer(rows(water, fill = "#d0d0d0", alpha = 1))))
  }
  if ("roads" %in% o$layers && z > 9) {
    aero <- get("aeroway")
    if (!is.null(aero)) {
      a <- ramp(list(c(9, 0), c(10, 0.9)))
      poly <- sf::st_is(aero, c("POLYGON", "MULTIPOLYGON"))
      out <- c(out, list(
        fill_layer(rows(aero[poly & aero$class %in% "runway", ], fill = "#2a2a2a", alpha = a)),
        line_layer(rows(aero[!poly & aero$class %in% "taxiway", ], colour = "#4a4a4a",
                        linetype = "solid", alpha = a, style = 1),
                   list(list(min = 0, width = list(c(0, 2)))))))
    }
  }
  if ("buildings" %in% o$layers && z > 14.5) {
    b <- get("building")
    a <- ramp(list(c(14.5, 0), c(15.5, 0.9)))
    out <- c(out, list(
      fill_layer(rows(b, fill = "#d0d0d0", alpha = a)),
      line_layer(rows(b, colour = "#a0a0a0", linetype = "solid", alpha = a, style = 1),
                 list(list(min = 0, width = list(c(0, 0.5)))))))
  }
  if ("roads" %in% o$layers) {
    roads <- get("transportation")
    if (!is.null(roads)) {
      roads <- roads[!sf::st_is(roads, c("POLYGON", "MULTIPOLYGON", "POINT", "MULTIPOINT")), ]
      parts <- list()
      styles <- list()
      add <- function(classes, minzoom, colour, width, casing = NULL, linetype = "solid") {
        if (z < minzoom) return()
        d <- roads[roads$class %in% classes, ]
        if (nrow(d) == 0) return()
        for (w in list(casing, width)) {
          if (is.null(w)) next
          styles[[length(styles) + 1]] <<- list(min = minzoom, width = w)
          parts[[length(parts) + 1]] <<- rows(d, colour = if (identical(w, casing)) "#000000" else colour,
                                              linetype = if (identical(w, casing)) "solid" else linetype,
                                              alpha = 1, style = length(styles))
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
      out <- c(out, list(line_layer(do.call(cm_rbind, parts), styles)))
    }
  }
  if ("labels" %in% o$layers) {
    family <- o$family
    if (is.null(family)) {
      family <- "sans"
      if (requireNamespace("systemfonts", quietly = TRUE) &&
          "Noto Sans" %in% systemfonts::system_fonts()$family) family <- "Noto Sans"
    }
    name_of <- function(d) {
      n <- if (o$lang == "fr" && "name.fr" %in% names(d)) d$name.fr else if ("name.en" %in% names(d)) d$name.en
      if (is.null(n)) n <- d$name
      ifelse(is.na(n) | n == "", d$name, n)
    }
    # Label styles in MapLibre's priority order. Sizes, halos and padding are in
    # pixels; labels are placed when the plot is drawn (makeContent.cm_labels).
    st <- function(classes, min, max, size, colour, bold, halo, pad = 2, upper = FALSE)
      list(classes = classes, min = min, max = max, size = size, colour = colour,
           bold = bold, halo = halo, pad = pad, upper = upper)
    styles <- list(
      st("country", 1, 5, list(c(1, 12), c(4, 18)), "#2a2a2a", TRUE, 2, upper = TRUE),
      st("state", 3, 8, list(c(3, 10), c(6, 14)), "#4a4a4a", FALSE, 2, upper = TRUE),
      st("city", 5, Inf, list(c(5, 12), c(8, 16), c(12, 22)), "#000000", TRUE, 2, upper = TRUE),
      st("town", 8, Inf, list(c(8, 11), c(12, 14)), "#0a0a0a", TRUE, 2),
      st("village", 10, Inf, list(c(0, 11)), "#1a1a1a", FALSE, 1.5),
      st(c("motorway", "trunk", "primary"), 10, Inf, list(c(10, 11), c(13, 14)), "#3a3a3a", FALSE, 1.5, 20),
      st(c("secondary", "tertiary"), 12, Inf, list(c(12, 11), c(14, 13)), "#4a4a4a", FALSE, 1.5, 30),
      st(c("residential", "unclassified", "living_street", "service"), 14, Inf, list(c(0, 12)),
         "#5a5a5a", FALSE, 1.5, 40))
    wanted <- vapply(styles, function(s) z >= s$min && z < s$max, logical(1))
    is_road <- seq_along(styles) >= 6
    style_of <- function(class, which) {
      m <- rep(NA_integer_, length(class))
      for (i in rev(which(wanted & which))) m[class %in% styles[[i]]$classes] <- i
      m
    }

    places <- if (any(wanted & !is_road)) get("place")
    if (!is.null(places)) {
      places$label <- name_of(places)
      places$style <- style_of(places$class, !is_road)
      places <- places[!is.na(places$style) & !is.na(places$label) & places$label != "", ]
      upper <- vapply(styles, function(s) s$upper, logical(1))[places$style]
      places$label[upper] <- toupper(places$label[upper])
      places <- places[!duplicated(paste(places$class, places$label)), ]
      places$angle <- rep(0, nrow(places))
      places$length <- rep(NA_real_, nrow(places))
      places <- places[order(places$style), c("label", "style", "angle", "length")]
    }
    roads <- NULL
    if (any(wanted & is_road)) {
      names <- get("transportation_name")
      if (!is.null(names)) {
        names$label <- name_of(names)
        names$style <- style_of(names$class, is_road)
        names <- names[!is.na(names$style) & !is.na(names$label) & names$label != "", ]
        if (nrow(names) > 0) roads <- cm_road_label_points(names, z)
      }
      if (!is.null(roads)) roads <- roads[order(roads$style, -roads$length), ]
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

# Label candidates for named roads: one per merged piece that is long enough
# for its label at zoom z, at the piece's midpoint and turned along it.
cm_road_label_points <- function(d, z) {
  crs <- sf::st_crs(d)
  d <- sf::st_transform(d, 3857)
  key <- paste(d$style, d$label, sep = "|")
  pieces <- lapply(split(seq_len(nrow(d)), key), function(i) {
    g <- sf::st_union(sf::st_geometry(d)[i])
    g <- sf::st_cast(sf::st_line_merge(sf::st_cast(g, "MULTILINESTRING")), "LINESTRING")
    data.frame(row = i[1], geometry = g)
  })
  p <- sf::st_sf(do.call(rbind, pieces), crs = 3857)
  m_per_px <- 2 * pi * 6378137 / (512 * 2^z)
  p <- p[as.numeric(sf::st_length(p)) >= nchar(d$label[p$row]) * 10 * 0.6 * m_per_px, ]
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
  out <- data.frame(label = d$label[p$row], style = d$style[p$row], angle = coords[, 3],
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
# cached for the session.
cm_tile_layer <- function(merc, z, layer, source, server) {
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
      url <- sprintf("%s/%s/%d/%d/%d", server, source, z, x, y)
      ok <- tryCatch(utils::download.file(url, path, mode = "wb", quiet = TRUE) == 0,
                     error = function(e) FALSE, warning = function(w) FALSE)
      if (!ok) {
        unlink(path)
        return(NULL)
      }
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
