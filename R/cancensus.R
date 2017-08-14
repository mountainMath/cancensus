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
#' @param level The census aggregation level to retrieve. One of \code{"Regions"}, \code{"PR"}, \code{"CMA"}, \code{"CD"}, \code{"CSD"}, \code{"CT"} or \code{"DA"}.
#' @param regions A named list of census regions to retrieve. Names must be valid census aggregation levels.
#' @param vectors An R vector containing the CensusMapper variable names of the census variables to download. If no vectors are specified only geographic data will get downloaded.
#' @param geo_format One of \code{"sf"} to return an \code{\link[sf]{sf}} object (the default; requires the \code{sf} package), \code{"sp"} to return a \code{\link[sp]{SpatialPolygonsDataFrame}} object (requires the \code{rgdal} package), or \code{NA} to append no geographical information to the returned data at all.
#' @param labels Set to "detailed" by default, but truncated Census variable names can be selected by setting labels = "short". Use cancensensus.labels() to return variable label information.
#' @param use_cache If set to TRUE (the default) data will be read from the local cache if available.
#' @param api_key An API key for the CensusMapper API. Defaults to \code{options()} and then the \code{CM_API_KEY} environment variable.
#' @keywords canada census data api
#' @export
#' @examples
#' # Load data without associated geographical spatial data
#' census_data <- cancensus.load(dataset='CA16', regions=list(CMA="59933"),
#'                               vectors=c("v_CA16_408","v_CA16_409","v_CA16_410"),
#'                               level='CSD', geo_format = NA)
#'
#' # Load data with associated geographical spatial data using the sf standard
#' census_data <- cancensus.load(dataset='CA16', regions=list(CMA="59933"),
#'                               vectors=c("v_CA16_408","v_CA16_409","v_CA16_410"),
#'                               level='CSD', geo_format = "sf")
#'
#' # Load data with geography and truncated variable names
#' census_data <- cancensus.load(dataset='CA16', regions=list(CMA="59933"),
#'                               vectors=c("v_CA16_408","v_CA16_409","v_CA16_410"),
#'                               level='CSD', geo_format = "sf", labels="short")
#'
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
    stop("the `level` parameter must be one of 'Regions', 'PR', 'CMA', 'CD', 'CSD', 'CT', or 'DA'")
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
      result <- utils::read.csv(data_file,  na = c("x","F"), colClasses=c("GeoUID"="character","Type"="factor","Region Name"="factor"),stringsAsFactors=F, check.names = FALSE)
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

    #transform and rename
    transform_geo <- function(g){
      as_character=c("id","rpid","rgid","ruid","rguid","q")
      as_numeric=c("a","nrr")
      as_factor=c("t")
      as_integer=c("pop","dw","hh","pop2")
      as_character=append(append(as_character,as_numeric),as_integer)

      g <- g %>%
        mutate_at(intersect(names(g),as_character),funs(as.character)) %>%
        mutate_at(intersect(names(g),as_numeric),funs(as.numeric))  %>%
        mutate_at(intersect(names(g),as_integer),funs(as.integer))  %>%
        mutate_at(intersect(names(g),as_factor),funs(as.factor))

      #change names
      #standar table
      name_change <- tibble(
        old=c("id","a" ,"t" ,"dw","hh","pop","pop2","nrr","q"),
        new=c("GeoUID","Shape Area" ,"Type" ,"Dwellings","Households","Population","Adjusted Population (previous Census)","NHS Non-Return Rate","Quality Flags")
        )
      #geo uid name changes
      if (level=='DB') {
        name_change <- name_change %>% rbind(
          c('rpid','DA_UID'),
          c('rgid','CSD_UID'),
          c('ruid','CT_UID'),
          c('rguid','CMA_UID'))
      }
      if (level=='DA') {
        name_change <- name_change %>% rbind(
          c('rpid','CSD_UID'),
          c('rgid','CD_UID'),
          c('ruid','CT_UID'),
          c('rguid','CMA_UID'))
      }
      if (level=='CT') {
        name_change <- name_change %>% rbind(
          c('rpid','CMA_UID'),
          c('rgid','PR_UID'),
          c('ruid','CSD_UID'),
          c('rguid','CD_UID'))
      }
      if (level=='CSD') {
        name_change <- name_change %>% rbind(
          c('rpid','CSD_UID'),
          c('rgid','PR_UID'),
          c('ruid','CMA_UID'))
      }
      if (level=='CD') {
        name_change <- name_change %>% rbind(c('rpid','PR_UID'),c('rgid','C_UID'))
      }
      if (level=='CMA') {
        name_change <- name_change %>% rbind(c('rpid','PR_UID'),c('rgid','C_UID'))
      }
      if (level=='PR') {
        name_change <- name_change %>% rbind(c('rpid','C_UID'))
      }
      old_names <- names(g)
      for (x in intersect(old_names,name_change$old)) {
        old_names[old_names==x]<-name_change$new[name_change$old==x]
      }
      names(g)<-old_names
      #g <- g %>% rename('GeoUID'='id','Population'='pop','Dwellings'='dw','Households'='hh',"Type"='t')

      return(g)
    }
    result <- if (geo_format == "sf") {
      geos <- sf::read_sf(geo_file) %>% transform_geo
      if (!is.null(result)) {
        geos %>%
          #select(-Population,-Dwellings,-Households,-Type)  %>%
          dplyr::inner_join(result %>% select(-Population,-Dwellings,-Households,-Type), by = "GeoUID")
      } else {
        geos
      }
    } else { # geo_format == "sp"
      geos <- rgdal::readOGR(geo_file, "OGRGeoJSON")
      geos@data <- geos@data %>% transform_geo
      if (!is.null(result)) {
        geos@data <- geos@data %>% select(-Population,-Dwellings,-Households,-Type)
        sp::merge(geos, result, by = "GeoUID")
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
#' census_data <- cancensus.load_data(dataset='CA16', regions=list(CMA:"59933"),
#'                                    vectors=c("v_CA16_408","v_CA16_409","v_CA16_410"),
#'                                    level='CSD')
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
#' census_data <- cancensus.load_geo(dataset='CA16', regions=list(CMA="59933"),
#'                                   level='CSD', geo_format = "sf")
cancensus.load_geo <- function (dataset, level, regions, geo_format = "sf", ...) {
  return(cancensus.load(dataset, level, regions, vectors=c(), geo_format=geo_format, ...))
}

# This is the set of valid census aggregation levels, also used in the named
# elements of the `regions` parameter.
VALID_LEVELS <- c("Regions","C","PR", "CMA", "CD", "CSD", "CT", "DA", "DB")

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
cancensus.list_datasets <- function() {
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

#' Query the CensusMapper API for available vectors for a given dataset.
#'
#' @param dataset The dataset to query for available vectors, e.g.
#'   \code{"CA16"}.
#'
#' @export
#'
#' @importFrom dplyr %>%
cancensus.list_vectors <- function(dataset) {
  # TODO: Validate dataset?
  result <- NULL
  response <- httr::GET(paste0("https://censusmapper.ca/api/v1/vector_info/",
                               dataset, ".csv"))
  if (httr::status_code(response) == 200 &&
        !requireNamespace("readr", quietly = TRUE)) {
    result <- utils::read.csv(httr::content(response, type = "text",
                                            encoding = "UTF-8"),
                              stringsAsFactors = FALSE)
    class(result) <- c("tbl_df", "tbl", "data.frame")
  } else if (httr::status_code(response) == 200) {
    # Use `readr`, if available.
    result <- readr::read_csv(httr::content(response, type = "text",
                                            encoding = "UTF-8"))
  } else {
    stop("API query for available vectors failed with error: ",
         httr::content(response, as = "text"),
         "(", httr::status_code(response),  ")")
  }
  result %>%
    dplyr::mutate(type = factor(type),
                  units = factor(units)) %>%
    dplyr::select(vector, type, label, units, parent_vector = parent) %>%
    dplyr::mutate(
      units = recode(.$units, 
                         `1` = "Number",
                         `2` = "Percentage ratio (0.0-1.0)",
                         `3` = "Currency",
                         `4` = "Ratio",
                         `5` = "Percentage (0-100)")) %>%
    dplyr::mutate(
      add = case_when(.$add == "1" ~ "Additive",
                      .$add == "0" ~ "Not additive",
                      grepl("^2.",.$add) ~  paste0("Average of ",gsub("^2.","",.$add)),
                      grepl("^3.",.$add) ~  paste0("Median  of ",gsub("^2.","",.$add))
                      )
      )
  # Feel free to recode to more meaningful names above
}

#' Return Census variable names and labels as a tidy data frame
#'
#' @param x A data frame, \code{sp} or \code{sf} object returned from
#'   \code{cancensus.load} or similar.
#'
#' @return
#'
#' A data frame with a column \code{variable} containing the truncated
#' variable name, and a column \code{label} describing it.
#'
#' @export
cancensus.labels <-  function(x) {
  if("census_labels" %in% names(attributes(x))) {
    attributes(x)$census_labels
  } else {
    warning("Data does not have variables to labels. No Census variables selected as vectors. See ?cancensus.load() for more information. ")}
  }


# Internal function to handle unfavourable http responses
cancensus.handle_status_code <- function(response,path){
  if (httr::status_code(response)!=200) {
    message=httr::content(response,as="text")
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

