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
#' @param level A geographic aggregation level for downloading data, e.g. CSD, CT, DA.
#' @param regions A json hash describing the geographic regions.
#' @param vectors An R vector containing the CensusMapper variable names of the census variables to download. If no vectors are specified only geographic data will get downloaded.
#' @param geo If set to TRUE, the function will also return the geographic data.
#' @param format Choose whether you want to use the sf or sp spatial format. Using sf will return a dataframe with a field for sf geometry, while using sp will return a SpatialPolygonsDataFrame object. Assumes sf as default but can be overwritten by selecting format = "sp".
#' @param labels Set to "detail" by default, but truncated Census variable names can be selected by setting labels = "short". Use cancensensus.labels() to return variable label information.
#' @param use_cache If set to TRUE (the default) data will be read from the local cache if available.
#' @keywords canada census data api
#' @export
#' @examples
#' census_data <- cancensus.load(dataset='CA16', regions='{"CMA":["59933"]}', vectors=c("v_CA16_408","v_CA16_409","v_CA16_410"), level='CSD', geo=TRUE)
#' census_data <- cancensus.load(dataset='CA16', regions='{"CMA":["59933"]}', vectors=c("v_CA16_408","v_CA16_409","v_CA16_410"), level='CSD', geo=TRUE, labels="short")
#' # Get details for truncated variables
#' cancensus.labels(census_data)
cancensus.load <- function (dataset, level, regions, vectors=c(), geo=TRUE, format = "sf", labels = "detailed", use_cache=TRUE, api_key=getOption("cancensus.api_key")) {
  api_key <- if (is.null(api_key) && nchar(Sys.getenv("CM_API_KEY")) > 1) { Sys.getenv("CM_API_KEY") } else { api_key }
  have_api_key <- !is.null(api_key)

  base_url="https://CensusMapper.ca/api/v1/"
  dir.create('data_cache', showWarnings = FALSE) # make sure cache directory exists
  # load data variables
  if (length(vectors)>0) {
    vectors_string=jsonlite::toJSON(vectors)
    data_param_string=paste(
      paste('regions',regions,sep='='),
      paste('vectors',vectors_string,sep='='),
      paste('level',level,sep='='),
      paste('dataset',dataset,sep='=')
      ,sep='&')
    data_hash=digest::digest(data_param_string,algo='md5')
    data_file=paste('data_cache/CM_data_',data_hash,'.csv',sep='')
    data_base_url=paste0(base_url,'data.csv')
    if (!use_cache || !file.exists(data_file)) {
      if (!have_api_key) stop('No API key set. Either set the key via\ncancensus.set_api_key(\'<your censusmappper API key>\')\n or as an environment variable \nSys.setenv(CM_API_KEY=\'<your API key>\')')
      final_data_param_string=paste(data_param_string,paste('api_key',api_key,sep='='),sep='&')
      response <- httr::GET(paste(data_base_url,final_data_param_string,sep='?'),httr::write_disk(data_file,overwrite = TRUE),httr::progress())
      cancensus.handle_status_code(response,data_file)
    }
    # read the data file and transform to proper data types
    if (requireNamespace("readr", quietly = TRUE)) {
      # Use readr::read_csv if it's available.
      dat <- readr::read_csv(data_file, na = c("x","F"), col_types = list(.default = "d", GeoUID = "c", Type = 'c', "Region Name" = 'c'))
      dat$GeoUID <- as.character(dat$GeoUID)
      dat$Type <- as.factor(dat$Type)
      dat$`Region Name` <- as.factor(dat$`Region Name`)
    } else {
      dat <- read.csv(data_file,  na = c("x","F"), colClasses=c("GeoUID"="character","Type"="factor","Region Name"="factor"),stringsAsFactors=F, check.names = FALSE)
    }
  } else if (!geo) {
    stop('Neither vectors nor geo data specified, nothing to do.')
  }

  if (geo) {
    geo_param_string=paste(
      paste('regions',regions,sep='='),
      paste('level',level,sep='='),
      paste('dataset',dataset,sep='=')
      ,sep='&')
    geo_hash=digest::digest(geo_param_string,algo='md5')
    geo_file=paste('data_cache/CM_geo_',geo_hash,'.geojson',sep='')
    if (!use_cache || !file.exists(geo_file)) {
      if (!have_api_key) stop('No API key set. Either set the key via\ncancensus.set_api_key(\'<your censusmappper API key>\')\n or as an environment variable \nSys.setenv(CM_API_KEY=\'<your API key>\')')
      final_geo_param_string=paste(geo_param_string,paste('api_key',api_key,sep='='),sep='&')
      geo_base_url=paste0(base_url,'geo.geojson')
      response <- httr::GET(paste(geo_base_url,final_geo_param_string,sep='?'),httr::write_disk(geo_file,overwrite = TRUE));
      cancensus.handle_status_code(response,geo_file)
    }
    # read the geo file and transform to proper data types
    if(format == "sf") {
      geos=sf::read_sf(geo_file)
      geos$id <- as.character(geos$id)
    } else {
      if ("rgdal" %in% installed.packages()) {
        geos=rgdal::readOGR(geo_file, "OGRGeoJSON")
        geos@data$id <- as.character(geos@data$id)
      } else {
        install.packages("rgdal")
        geos=rgdal::readOGR(geo_file, "OGRGeoJSON")
        geos@data$id <- as.character(geos@data$id)
      }
    }

    if (exists("dat")) {
      if(format == "sf") {
      result <- dplyr::inner_join(geos, dat, by = c("id" = "GeoUID"))
      } else {
        result <- sp::merge(geos, dat, by.x = "id", by.y = "GeoUID")
      }
    } else {
      result=geos;

    }
  } else {
    result=dat
  }
  if (length(vectors)>0) {
   census_labels <- names(result)[grep("^v_", names(result))]
   census_labels <- strsplit(census_labels, ": ")
   census_labels <- as_data_frame(do.call(rbind, census_labels))
   names(census_labels) <- c("Census Variable", "Detail")
   attributes(result)$census_labels <- census_labels
   if(labels == "short") names(result@data) <- gsub(":.*","",names(result@data))
  }
  return(result)
}

#' Convenience function to load only census data and no geographies.
#' @param dataset A CensusMapper dataset identifier.
#' @param level A geographic aggregation level for downloading data, e.g. CSD, CT, DA.
#' @param regions A json hash describing the geographic regions.
#' @param vectors An R vector containing the CensusMapper variable names of the census variables to download. If no vectors are specified only geographic data will get downloaded.
#' @param geo If set to TRUE, the function will also return the geographic data.
#' @param use_cache If set to TRUE (the default) data will be read from the local cache if available.
#' @keywords canada census data api
#' @export
#' @examples
#' census_data <- cancensus.load_data(dataset='CA16', regions='{"CMA":["59933"]}', vectors=c("v_CA16_408","v_CA16_409","v_CA16_410"), level='CSD')
cancensus.load_data <- function (dataset, level, regions, vectors=c(), use_cache=TRUE, api_key=getOption("cancensus.api_key")) {
  return(cancensus.load(dataset, level, regions, vectors, geo=FALSE, use_cache=use_cache, api_key=api_key))
}

#' Convenience function to load only census geography without data.
#' @param dataset A CensusMapper dataset identifier.
#' @param level A geographic aggregation level for downloading data, e.g. CSD, CT, DA.
#' @param regions A json hash describing the geographic regions.
#' @param format Choose whether you want to use the sf or sp spatial format. Assumes sf as default but can be overwritten by selecting format = "sp".
#' @param use_cache If set to TRUE (the default) data will be read from the local cache if available.
#' @keywords canada census data api
#' @export
#' @examples
#' census_data <- cancensus.load_geo(dataset='CA16', regions='{"CMA":["59933"]}', level='CSD')
cancensus.load_geo <- function (dataset, level, regions, format, use_cache=TRUE, api_key=getOption("cancensus.api_key")) {
  return(cancensus.load(dataset, level, regions, geo=TRUE, format = "sf", use_cache=use_cache, api_key=api_key))
}

#' Convenience function to set the api key for the current session.
#' @param api_key Your CensusMapper API key.
#' @export
#' @examples
#'cancensus.set_api_key('CensusMapper_2e24662e6dde22b46d5a316e81bebddf')
cancensus.set_api_key <- function(api_key){
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
