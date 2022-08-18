#' Get identifiers for census regions intersecting a geometry
#'
#' @description
#' This function returns a list of regions that intersect a given geometry input. This list of regions can be used
#' directly to query census when one is interested in census data for a particular geographic region that does
#' not coincide with defined census geometries. The returned value can be used as the \code{regions}
#' parameter in \link{get_census} to get corresponding census geographies and variables that cover the give geometry.
#' Input spatial objects can be any \code{sf} or \code{sfc} class objects such as \code{POINT}, \code{MULTIPOINT} or \code{POLYGON}.
#'
#' This function comes with CensusMapper API limits
#'
#' @param dataset A CensusMapper dataset identifier.
#' @param level The census aggregation level to retrieve. One of \code{"Regions"}, \code{"PR"}, \code{"CMA"}, \code{"CD"}, \code{"CSD"}, \code{"CT"}, \code{"DA"}, \code{"EA"} (for 1996 census), or \code{"DB"} (for 2001-2016 censuses)..
#' @param geometry A valid \code{sf} or \code{sfc} class object. As long as the geometry is valid, any projection is accepted. Objects will be reprojected as server-side intersections use lat/lon projection.
#' @param simplified If \code{TRUE} will return a region list compatible with \link{get_census}, otherwise will return a character vector of matching region ids.
#' @param use_cache If set to \code{TRUE} (the default) data will be read from the local cache if available.
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
#'
#' \dontrun{
#' # Example using a POINT-class object from a pair of lat/lon coordinates
#' point_geo <- sf::st_sfc(sf::st_point(c(-123.25149, 49.27026)), crs=4326)
#' regions <- get_intersecting_geometries(dataset = 'CA16', level = 'CT', geometry = point_geo)
#' census_data <- get_census(dataset='CA16', regions=regions,
#'                           vectors=c("v_CA16_408","v_CA16_409","v_CA16_410"),
#'                           level='CT')
#'
#' # Example using a POLYGON-class object from a bounding box with four coordinates
#' poly_geo <- sf::st_as_sfc(sf::st_bbox(c(ymin = 49.25, ymax = 49.30,
#'                           xmin = -123.25, xmax = -123.30)), crs = 4326)
#' regions <- get_intersecting_geometries(dataset = 'CA16', level = 'CT', geometry = poly_geo)
#' census_data <- get_census(dataset='CA16', regions=regions,
#'                          vectors=c("v_CA16_408","v_CA16_409","v_CA16_410"), level='CT')
#'
#'}
get_intersecting_geometries <- function(dataset, level, geometry, simplified = FALSE,
                                        use_cache = TRUE, quiet = FALSE,
                                        api_key=Sys.getenv("CM_API_KEY")) {
  api_key <- robust_api_key(api_key)
  have_api_key <- valid_api_key(api_key)
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

  crs=sf::st_crs(geometry)$epsg

  if (is.na(crs) || crs!=4326) {
    geometry <- geometry %>% sf::st_transform(4326)
  }

  geo <- geojsonsf::sfc_geojson(geometry)

  param_string <- paste0("dataset=", dataset,
                         "&level=", level,
                         "&geometry=", geo)
  data_base_name <- paste0("CM_data_intersect_",digest::digest(param_string, algo = "md5"))
  data_file <- cache_path(data_base_name, ".rda")
  meta_file <- paste0(data_file, ".meta")

  if (!use_cache || !file.exists(data_file)) {
    if (!have_api_key) {
      stop(paste("No API key set. Use set_api_key('<your API ket>`) to set one, or set_api_key('<your API ket>`, install = TRUE) to save is permanently in our .Renviron."))
    }
    url <- paste0(cancensus_base_url(),"/api/v1/intersecting_geographies")
    body <- list(dataset=dataset,
                 level=level,
                 area=sf::st_area(geometry) %>% as.numeric(),
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
    data_version <- response$headers$`data-version`

    result <- httr::content(response, type = "text", encoding = "UTF-8") %>%
      jsonlite::fromJSON()
    attr(result, "last_updated") <- Sys.time()
    save(result, file = data_file)
    file_info <- file.info(data_file)
    metadata <- dplyr::tibble(dataset=dataset,level=level,created_at=Sys.time(),
                               version=data_version,size=file_info$size)
    saveRDS(metadata, file = meta_file)
  } else {
    if (!quiet) message("Reading intersection data from local cache.")
    # Load `result` object from cache.
    load(file = data_file)
  }

  touch_metadata(meta_file)

  if(simplified) {result <- c(as.character(result))}
  result
}
