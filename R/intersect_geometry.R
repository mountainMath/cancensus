#' Get identifiers for census regions interesecting a geometry
#'
#' @description
#' This function returns a list of regions that intersect a give geometry. It is useful for example when
#' one is interested in census data for a particular geographic region that does not coincide with
#' well-known census geometries. The returned value can be used as the \code{regions} parameter in \code{get_census}
#' to get corresponding census geographies and variables that cover the give geometry.
#'
#' This function requires advanced CensusMapper API access.
#'
#' @param dataset A CensusMapper dataset identifier.
#' @param level The census aggregation level to retrieve. One of \code{"Regions"}, \code{"PR"}, \code{"CMA"}, \code{"CD"}, \code{"CSD"}, \code{"CT"}, \code{"DA"}, \code{"EA"} (for 1996 census), or \code{"DB"} (for 2001-2016 censuses)..
#' @param geometry An \code{sf} or \code{sfc} object
#' @param use_cache If set to TRUE (the default) data will be read from the local cache if available.
#' @param quiet When TRUE, suppress messages and warnings.
#' @param api_key An API key for the CensusMapper API. Defaults to \code{options()} and then the \code{CM_API_KEY} environment variable.
#'
#' @source Census data and boundary geographies are reproduced and distributed on
#' an "as is" basis with the permission of Statistics Canada (Statistics Canada
#' 1996; 2001; 2006; 2011; 2016).
#'
#' @export
#'
#' @examples
#' # Query the API for the census tract contain the coordinates [-123.25149, 49.27026]
#' \dontrun{
#' point_geo <- sf::st_sfc(sf::st_point(c(-123.25149, 49.27026)),crs=4326)
#' regions <- get_intersecting_geometries(dataset = 'CA16', level = 'CT', geometry = point_geo)
#' census_data <- get_census(dataset='CA16', regions=regions,
#'                           vectors=c("v_CA16_408","v_CA16_409","v_CA16_410"),
#'                           level='CT')
#'
#'}
get_intersecting_geometries <- function(dataset, level, geometry,
                                        use_cache = TRUE, quiet = FALSE,
                                        api_key=getOption("cancensus.api_key")) {
  api_key <- robust_api_key(api_key)
  have_api_key <- !is.null(api_key)
  result <- NULL

    if ("sf" %in% class(geometry)) {
    geometry=sf::st_geometry(geometry)
  }
  if (!("sfc" %in% class(geometry))) {
    stop("The `geometry` parameter needs to be of class sf or sfc")
  }
  if (length(geometry)>1) {
    geometry <- sf::st_union(geometry)
  }

  geo <- geojsonsf::sfc_geojson(geometry)

  param_string <- paste0("dataset=", dataset,
                         "&level=", level,
                         "&geometry=", geo)
  data_file <- cache_path("CM_data_intersect_",
                          digest::digest(param_string, algo = "md5"), ".rda")

  if (!use_cache || !file.exists(data_file)) {
    if (!have_api_key) {
      stop(paste("No API key set. Use options(cancensus.api_key = 'XXX') or",
                 "Sys.setenv(CM_API_KEY = 'XXX') to set one."))
    }
    url <- paste0(cancensus_base_url(),"/api/v1/intersecting_geographies")
    body <- list(dataset=dataset,
                 level=level,
                 geometry=geo,
                 api_key=api_key)
    response <- if (!quiet) {
      message("Querying CensusMapper API...")
      httr::POST(url, body = body,
                 config = httr::accept_json(),
                 httr::progress())
    } else {
      httr::POST(url, body = body,
                 config = httr::accept_json())
    }
    handle_cm_status_code(response, NULL)

    result <- httr::content(response, type = "text", encoding = "UTF-8") %>%
      jsonlite::fromJSON()
    attr(result, "last_updated") <- Sys.time()
    save(result, file = data_file)
  } else {
    if (!quiet) message("Reading intersection data from local cache.")
    # Load `result` object from cache.
    load(file = data_file)
  }
  result
}
