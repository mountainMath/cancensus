# Helpers for working with census data in the style of CensusMapper: the base
# map's features, and CensusMapper's custom functions. The ggplot2 helpers are
# in cm_ggplot_helpers.R.

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
