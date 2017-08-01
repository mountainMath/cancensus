#' CensusMapper API access
#'
#' This function allows convenient access to the CensusMapper API
#'
#' Got to https://censusmapper.ca/api to select variables and geographic regions
#' you want to download data for
#'
#' An API key is required to use this function. Either set the API key on censusmapper
#' censusmapper.api_key='<your API key>'
#' or as environment variable
#' sys.setenv(CM_API_KEY='<your API key>')
#'
#' @param dataset A CensusMapper dataset identifier.
#' @param level A geographic aggregation level for downloading data, e.g. CSD, CT, DA.
#' @param regions A json hash describing the geographic regions.
#' @param vectors An R vector containing the CensusMapper variable names of the census variables to download. If no vectors are specified only geographic data will get downloaded.
#' @param geo If set to TRUE, the function will also return the geographic data.
#' @param use_cache If set to TRUE (the default) data will be read from the local cache if available.
#' @keywords canada census data api
#' @export
#' @examples
#' census_data <- cancensus.load(dataset='CA16', regions='{"CMA":["59933"]}', vectors=c("v_CA16_408","v_CA16_409","v_CA16_410"), level='CSD', geo=TRUE)
cancensus.load <- function (dataset, level, regions, vectors=c(), geo=FALSE,use_cache=TRUE) {
  api_key=Sys.getenv('CM_API_KEY')
  have_api_key=length(api_key)>1

  base_url="https://CensusMapper.ca/api/v1/"
  dir.create('data_cache', showWarnings = FALSE) # make sure cache directory exists
  # load data variables
  if (length(vectors)>0) {
    vectors_string=toJSON(vectors)
    data_param_string=paste(
      paste('regions',regions,sep='='),
      paste('vectors',vectors_string,sep='='),
      paste('level',level,sep='='),
      paste('dataset',dataset,sep='=')
      ,sep='&')
    data_hash=digest(data_param_string,algo='md5')
    data_file=paste('data_cache/CM_data_',data_hash,'.csv',sep='')
    data_base_url=paste0(base_url,'data.csv')
    if (!use_cache || !file.exists(data_file)) {
      if (!have_api_key) stop('No API key set. Either set the key on the censusmapper object\ncensumapper.api_key=<your censusmappper API key>\n or as an environment variable \nSys.setenv(CM_API_KEY=\'<your API key>\')')
      final_data_param_string=paste(data_param_string,paste('api_key',censusmapper.api_key,sep='='),sep='&')
      GET(paste(data_base_url,final_data_param_string,sep='?'),write_disk(data_file,overwrite = TRUE),progress());
    }
    # read the data file and transform to proper data types
    dat <- read.csv(data_file,  na = c("x","F"), colClasses=c("GeoUID"="character","Type"="factor","Region.Name"="factor"),stringsAsFactors=F)
#    dat <- read_csv(data_file, na = c("x","F")) %>%
#      mutate(GeoUID = as.character(GeoUID),
#             Type = as.factor(Type),
#             `Region Name` = as.factor(`Region Name`))
  } else if (!geo) {
    stop('Neither vectors nor geo data specified, nothing to do.')
  }

  if (geo) {
    geo_param_string=paste(
      paste('regions',regions,sep='='),
      paste('level',level,sep='='),
      paste('dataset',dataset,sep='=')
      ,sep='&')
    geo_hash=digest(geo_param_string,algo='md5')
    geo_file=paste('data_cache/CM_geo_',geo_hash,'.geojson',sep='')
    if (!use_cache || !file.exists(geo_file)) {
      if (!have_api_key) stop('No API key set. Either set the key on the censusmapper object\ncensumapper.api_key=<your censusmappper API key>\n or as an environment variable \nSys.setenv(CM_API_KEY=\'<your API key>\')')
      final_geo_param_string=paste(geo_param_string,paste('api_key',censusmapper.api_key,sep='='),sep='&')
      geo_base_url=paste0(base_url,'geo.geojson')
      GET(paste(geo_base_url,final_geo_param_string,sep='?'),write_disk(geo_file,overwrite = TRUE));
    }
    # read the geo file and transform to proper data types
    geos=read_sf(geo_file)
    geos$id <- as.character(geos$id)

    if (exists("dat")) {
      result <- merge(geos, dat, by.x="id", by.y="GeoUID")
    } else {
      result=geos;
    }
  } else {
    result=dat
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
cancensus.load_data <- function (dataset, level, regions, vectors=c(), use_cache=TRUE) {
  return(cancensus.load(dataset, level, regions, vectors=c(), geo=FALSE, use_cache=TRUE))
}

#' Convenience function to load only census data and no geographies.
#' @param dataset A CensusMapper dataset identifier.
#' @param level A geographic aggregation level for downloading data, e.g. CSD, CT, DA.
#' @param regions A json hash describing the geographic regions.
#' @param geo If set to TRUE, the function will also return the geographic data.
#' @param use_cache If set to TRUE (the default) data will be read from the local cache if available.
#' @keywords canada census data api
#' @export
#' @examples
#' census_data <- cancensus.load_geo(dataset='CA16', regions='{"CMA":["59933"]}', level='CSD')
cancensus.load_geo <- function (dataset, level, regions, use_cache=TRUE) {
  return(cancensus.load(dataset, level, regions, geo=TRUE, use_cache=TRUE))
}

#' Convenience function to set the api key for the current session.
#' @param api_key Your CensusMapper API key.
#' @examples
#'cancensus.set_api_key('CensusMapper_2e24662e6dde22b46d5a316e81bebddf')
cancensus.set_api_key <- function(api_key){
  Sys.setenv(CM_API_KEY=api_key)
}
