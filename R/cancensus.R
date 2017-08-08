#' CensusMapper API access
#'
#' This function allows convenient access to the CensusMapper API
#'
#' Got to https://censusmapper.ca/api to select variables and geographic regions
#' you want to download data for
#'
#' An API key is required to use this function. Either set the API key on censusmapper
#' api_key='<your API key>'
#' or as environment variable
#' sys.setenv(CM_API_KEY='<your API key>')
#'
#' @param dataset A CensusMapper dataset identifier.
#' @param level The census aggregation level to retrieve. One of \code{"PR"}, \code{"CMA"}, \code{"CD"}, \code{"CSD"}, \code{"CT"} or \code{"DA"}.
#' @param regions A named list of census regions to retrieve. Names must be valid census aggregation levels.
#' @param vectors An R vector containing the CensusMapper variable names of the census variables to download. If no vectors are specified only geographic data will get downloaded.
#' @param geo_format One of \code{"sf"} to return an \code{\link[sf]{sf}} object (the default; requires the \code{sf} package), \code{"sp"} to return a \code{\link[sp]{SpatialPolygonsDataFrame}} object (requires the \code{rgdal} package), or \code{NA} to append no geographical information to the returned data at all.
#' @param labels Set to "detailed" by default, but truncated Census variable names can be selected by setting labels = "short". Use cancensensus.labels() to return variable label information.
#' @param use_cache If set to TRUE (the default) data will be read from the local cache if available.
#' @keywords canada census data api
#' @export
#' @examples
#' # Load data without associated geographical spatial data
#' census_data <- cancensus.load(dataset='CA16', regions=list(CMA="59933"), vectors=c("v_CA16_408","v_CA16_409","v_CA16_410"), level='CSD', geo_format = NA)
#' # Load data with associated geographical spatial data using the sf standard 
#' census_data <- cancensus.load(dataset='CA16', regions=list(CMA="59933"), vectors=c("v_CA16_408","v_CA16_409","v_CA16_410"), level='CSD', geo_format = "sf")
#' # Load data with geography and truncated variable names
#' census_data <- cancensus.load(dataset='CA16', regions=list(CMA="59933"), vectors=c("v_CA16_408","v_CA16_409","v_CA16_410"), level='CSD', geo_format = "sf", labels="short")
#' # 
#' # Get details for truncated variables
#' cancensus.labels(census_data)
cancensus.load <- function (dataset, level, regions, vectors=c(), geo_format = "sf", labels = "detailed", use_cache=TRUE, api_key=getOption("cancensus.api_key")) {
  api_key <- if (is.null(api_key) && nchar(Sys.getenv("CM_API_KEY")) > 1) { Sys.getenv("CM_API_KEY") } else { api_key }
  have_api_key <- !is.null(api_key)
  result <- NULL

  # Turn the region list into a valid JSON dictionary.
  if (is.character(regions)) {
    warning(paste("passing `regions` as a character vector is depreciated, and",
                  "will be removed in future versions"))
  } else if (is.null(names(regions)) || !all(names(regions) %in% VALID_LEVELS)) {
    stop("regions must be composed of valid census aggregation levels.")
  } else {
    regions <- jsonlite::toJSON(regions)
  }

  # Check if the aggregation level is valid.
  if (!level %in% VALID_LEVELS) {
    stop("the `level` parameter must be one of 'PR', 'CMA', 'CD', 'CSD', 'CT', or 'DA'")
  }

  # Check that we can read the requested geo format.
  if (is.na(geo_format)) { # This is ok.
  } else if (geo_format == "sf" && !requireNamespace("sf", quietly = TRUE)) {
    stop("the `sf` package is required to return 'sf' objects.")
  } else if (geo_format == "sp" && !requireNamespace("rgdal", quietly = TRUE)) {
    stop("the `rgdal` package is required to return 'sp' objects")
  } else if (!geo_format %in% c("sf", "sp")) {
    stop("the `geo_format` parameter must be one of 'sf', 'sp', or NA")
  }

  base_url="https://CensusMapper.ca/api/v1/"
  dir.create('data_cache', showWarnings = FALSE) # make sure cache directory exists
  # load data variables
  if (length(vectors)>0) {
    param_string <- paste0("regions=", regions,
                           # Convert vectors to a JSON list.
                           "&vectors=", jsonlite::toJSON(vectors),
                           "&level=", level, "&dataset=", dataset)
    data_file <- paste0("data_cache/CM_data_",
                        digest::digest(param_string, algo = "md5"), ".csv")
    if (!use_cache || !file.exists(data_file)) {
      if (!have_api_key) {
        stop(paste("No API key set. Use options(cancensus.api_key = 'XXX') or",
                   "Sys.setenv(CM_API_KEY = 'XXX') to set one."))
      }
      url <- paste0(base_url, "data.csv?", param_string, "&api_key=", api_key)
      response <- httr::GET(url, httr::write_disk(data_file, overwrite = TRUE),
                            httr::progress())
      cancensus.handle_status_code(response,data_file)
    } else {
      message("Reading vectors data from local cache.")
    }
    # read the data file and transform to proper data types
    if (requireNamespace("readr", quietly = TRUE)) {
      # Use readr::read_csv if it's available.
      result <- readr::read_csv(data_file, na = c("x","F"), col_types = list(.default = "d", GeoUID = "c", Type = 'c', "Region Name" = 'c'))
      result$GeoUID <- as.character(result$GeoUID)
      result$Type <- as.factor(result$Type)
      result$`Region Name` <- as.factor(result$`Region Name`)
    } else {
      result <- read.csv(data_file,  na = c("x","F"), colClasses=c("GeoUID"="character","Type"="factor","Region Name"="factor"),stringsAsFactors=F, check.names = FALSE)
    }
  } else if (is.na(geo_format)) {
    stop('Neither vectors nor geo data specified, nothing to do.')
  }

  if (!is.na(geo_format)) {
    param_string <- paste0("regions=", regions, "&level=", level, "&dataset=",
                           dataset)
    geo_file <- paste0("data_cache/CM_geo_",
                       digest::digest(param_string, algo = "md5"), ".geojson")
    if (!use_cache || !file.exists(geo_file)) {
      if (!have_api_key) {
        stop(paste("No API key set. Use options(cancensus.api_key = 'XXX') or",
                   "Sys.setenv(CM_API_KEY = 'XXX') to set one."))
      }
      url <- paste0(base_url, "geo.geojson?", param_string, "&api_key=",
                    api_key)
      response <- httr::GET(url, httr::write_disk(geo_file, overwrite = TRUE),
                            httr::progress())
      cancensus.handle_status_code(response,geo_file)
    } else {
      message("Reading geo data from local cache.")
    }
    # read the geo file and transform to proper data types
    result <- if (geo_format == "sf") {
      geos <- sf::read_sf(geo_file)
      geos$id <- as.character(geos$id)
      if (!is.null(result)) {
        dplyr::inner_join(geos, result, by = c("id" = "GeoUID"))
      } else {
        geos
      }
    } else { # geo_format == "sp"
      geos <- rgdal::readOGR(geo_file, "OGRGeoJSON")
      geos@data$id <- as.character(geos@data$id)
      if (!is.null(result)) {
        sp::merge(geos, result, by.x = "id", by.y = "GeoUID")
      } else {
        geos
      }
    }
  }

  if (length(vectors)>0) {
   census_labels <- names(result)[grep("^v_", names(result))]
   census_labels <- strsplit(census_labels, ": ")
   census_labels <- dplyr::as_data_frame(do.call(rbind, census_labels))
   names(census_labels) <- c("Vector", "Detail")
   attributes(result)$census_labels <- census_labels
   if(labels == "short") {
     if (geo_format=="sp") {names(result@data) <- gsub(":.*","",names(result@data))}
     else {names(result) <- gsub(":.*","",names(result))}
   }
  }
  return(result)
}

#' Convenience function to load only census data and no geographies.
#'
#' @param ... Further arguments passed on to \code{\link[cancensus]{cancensus.load}}.
#' @inheritParams cancensus.load
#'
#' @keywords canada census data api
#' @export
#' @examples
#' census_data <- cancensus.load_data(dataset='CA16', regions='{"CMA":["59933"]}', vectors=c("v_CA16_408","v_CA16_409","v_CA16_410"), level='CSD')
cancensus.load_data <- function (dataset, level, regions, vectors, ...) {
  return(cancensus.load(dataset, level, regions, vectors, geo_format = NA, ...))
}

#' Convenience function to load only census geography without data.
#'
#' @param ... Further arguments passed on to \code{\link[cancensus]{cancensus.load}}.
#' @inheritParams cancensus.load
#'
#' @keywords canada census data api
#' @export
#' @examples
#' census_data <- cancensus.load_geo(dataset='CA16', regions='{"CMA":["59933"]}', level='CSD', geo_format = "sf")
cancensus.load_geo <- function (dataset, level, regions, geo_format = "sf", ...) {
  return(cancensus.load(dataset, level, regions, vectors=c(), geo_format=geo_format, ...))
}

# This is the set of valid census aggregation levels, also used in the named
# elements of the `regions` parameter.
VALID_LEVELS <- c("PR", "CMA", "CD", "CSD", "CT", "DA")

#' Convenience function to set the api key for the current session.
#' @param api_key Your CensusMapper API key.
#' @export
#' @examples
#'cancensus.set_api_key('CensusMapper_2e24662e6dde22b46d5a316e81bebddf')
cancensus.set_api_key <- function(api_key){
  warning(paste0("cancensus.set_api_key() is depreciated, and will be removed ",
                 "in future versions. Use options(cancensus.api_key = \"",
                 api_key, "\") or Sys.setenv(CM_API_KEY = \"", api_key,
                 "\") instead."))
  options(cancensus.api_key = api_key)
}

#' Query the CensusMapper API for available datasets.
#'
#' @return
#'
#' A data frame with a column \code{dataset} containing the code for the
#' dataset, and a column \code{description} describing it.
#'
#' @export
list_datasets <- function() {
  response <- httr::GET("https://censusmapper.ca/api/v1/list_datasets",
                        httr::accept_json())
  if (httr::status_code(response) == 200) {
    result <- jsonlite::fromJSON(httr::content(response, type = "text",
                                               encoding = "UTF-8"))
    class(result) <- c("tbl_df", "tbl", "data.frame")
    result
  } else {
    stop("API query for available data sets failed with error: ",
         httr::content(response, as = "text"),
         "(", httr::status_code(response),  ")")
  }
}

#' Return Census variable names and labels as a tidy data frame
#'
#' @return
#'
#' A data frame with a column \code{variable} containing the truncated
#' variable name, and a column \code{label} describing it.
#'
#' @export
cancensus.labels <-  function(dat) {
  if("census_labels" %in% names(attributes(dat))) {
    attributes(dat)$census_labels
  } else {
    warning("Data does not have variables to labels. No Census variables selected as vectors. See ?cancensus.load() for more information. ")}
  }


#' Internal function to handle unfavourable http responses
cancensus.handle_status_code <- function(response,path){
  if (httr::status_code(response)!=200) {
    message=content(response,as="text")
    file.remove(path)
    if (httr::status_code(response)==401) {
      # Problem with API key
      stop(paste("Download of Census Data failed.",
                 "Please ensure that your API key is valid and has a large enough quota left.",
                 message, sep=' '))
    } else if (httr::status_code(response)==500) {
      stop(paste("Download of Census Data failed.",
                 "The request triggered a server error, the CensusMapper maintainers have been notified and will fix this as soon as possible.",
                 message, sep=' '))
    } else {
      stop(paste("Download of Census Data failed.",
                 message, sep=' '))
    }
  }
}

.onLoad <- function(libname, pkgname) {
  if (!"cancensus.api_key" %in% names(options())) {
    # Try to get the API key from the CM_API_KEY environment variable, if it has not already been specified.
    options(cancensus.api_key = if (nchar(Sys.getenv("CM_API_KEY")) > 1) { Sys.getenv("CM_API_KEY") } else { NULL })
  }
}
