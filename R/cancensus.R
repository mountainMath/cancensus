#' Access to Canadian census data through the CensusMapper API
#'
#' This function allows convenient access to Canadian census data and boundary
#' files through the CensusMapper API. An API key is required to retrieve data.
#'
#' For help selecting regions and vectors, see \code{\link{list_census_regions}}
#' and \code{\link{list_census_vectors}}, or check out the interactive selection
#' tool at \url{https://censusmapper.ca/api} by calling \code{explore_census_vectors()}
#'
#' @param dataset A CensusMapper dataset identifier.
#' @param regions A named list of census regions to retrieve. Names must be valid census aggregation levels.
#' @param level The census aggregation level to retrieve, defaults to \code{"Regions"}. One of \code{"Regions"}, \code{"PR"}, \code{"CMA"}, \code{"CD"}, \code{"CSD"}, \code{"CT"}, \code{"DA"}, \code{"EA"} (for 1996), or \code{"DB"} (for 2001-2016).
#' @param vectors An R vector containing the CensusMapper variable names of the census variables to download. If no vectors are specified only geographic data will get downloaded.
#' @param geo_format By default is set to \code{NA} and appends no geographic information. To include geographic information with census data, specify one of either \code{"sf"} to return an \code{\link[sf]{sf}} object (requires the \code{sf} package) or \code{"sp"} to return a \code{\link[sp]{SpatialPolygonsDataFrame-class}} object (requires the \code{rgdal} package).
#' @param labels Set to "detailed" by default, but truncated Census variable names can be selected by setting labels = "short". Use \code{label_vectors(...)} to return variable label information in detail.
#' @param use_cache If set to TRUE (the default) data will be read from the local cache if available.
#' @param quiet When TRUE, suppress messages and warnings.
#' @param api_key An API key for the CensusMapper API. Defaults to \code{options()} and then the \code{CM_API_KEY} environment variable.
#'
#' @source Census data and boundary geographies are reproduced and distributed on
#' an "as is" basis with the permission of Statistics Canada (Statistics Canada 1996; 2001; 2006; 2011; 2016).
#'
#' @export
#'
#' @examples
#' # Query the API for data on dwellings in Vancouver, at the census subdivision
#' # level:
#' \dontrun{
#' census_data <- get_census(dataset='CA16', regions=list(CMA="59933"),
#'                           vectors=c("v_CA16_408","v_CA16_409","v_CA16_410"),
#'                           level='CSD')
#'
#' # Query the API for data on dwellings in Vancouver, at the census subdivision
#' # level, and return the associated geography files in \code{sf} format:
#' census_data <- get_census(dataset='CA16', regions=list(CMA="59933"),
#'                           vectors=c("v_CA16_408","v_CA16_409","v_CA16_410"),
#'                           level='CSD', geo_format = "sf")
#'
#' # Make the same query, but this time drop descriptive vector names:
#' census_data <- get_census(dataset='CA16', regions=list(CMA="59933"),
#'                           vectors=c("v_CA16_408","v_CA16_409","v_CA16_410"),
#'                           level='CSD', geo_format = "sf", labels="short")
#'
#' # Get details for truncated vectors:
#' label_vectors(census_data)
#'}
get_census <- function (dataset, regions, level=NA, vectors=c(), geo_format = NA, labels = "detailed", use_cache=TRUE, quiet=FALSE, api_key=getOption("cancensus.api_key")) {
  api_key <- robust_api_key(api_key)
  have_api_key <- !is.null(api_key)
  result <- NULL

  if (is.na(level)) level="Regions"

  # Turn the region list into a valid JSON dictionary.
  if (is.character(regions)) {
    if (!quiet) warning(paste("Passing `regions` as a character vector is",
                              "depreciated, and will be removed in future",
                              "versions"))
  } else if (is.null(names(regions)) || !all(names(regions) %in% VALID_LEVELS)) {
    stop("Regions must be composed of valid census aggregation levels.")
  } else {
    regions <- jsonlite::toJSON(lapply(regions,as.character)) # cast to character in case regions are supplied as numeric/interger
  }

  # Remind to set cache directory

  if(as.character(options("cancensus.cache_path"))==tempdir()) {
    # Cache in tmp dir by default.
    options(cancensus.cache_path = tempdir())
    message(
      paste(
        "Census data is currently stored temporarily.\n\n",
        "In order to speed up performance, reduce API quota usage, and reduce",
        "unnecessary network calls, please set up a persistent cache directory by",
        "setting options(cancensus.cache_path = '<path to cancensus cache directory>')\n\n",
        "You may add this option, together with your API key, to your .Rprofile.\n\n"
      )
    )
  }

  # Check if the aggregation level is valid.
  if (!level %in% VALID_LEVELS) {
    stop("the `level` parameter must be one of 'Regions', 'PR', 'CMA', 'CD', 'CSD', 'CT', 'DA', 'EA' or 'DB'")
  }

  # Check that we can read the requested geo format.
  if (is.na(geo_format)) { # This is ok.
  } else if (!geo_format %in% c("sf", "sp")) {
    stop("the `geo_format` parameter must be one of 'sf', 'sp', or NA")
  } else if (!is.na(geo_format) && !requireNamespace("sf", quietly = TRUE)) {
    stop("the `sf` package is required to return geographies.")
  }

  base_url=paste0(cancensus_base_url(),"/api/v1/")
  # load data variables
  if (length(vectors)>0 || is.na(geo_format)) {
    param_string <- paste0("regions=", regions,
                           # Convert vectors to a JSON list.
                           "&vectors=", jsonlite::toJSON(as.character(vectors)),
                           "&level=", level, "&dataset=", dataset)
    if (is.na(geo_format)) param_string=paste0(param_string,"&geo_hierarchy=true")
    data_file <- cache_path("CM_data_",
                            digest::digest(param_string, algo = "md5"), ".rda")
    if (!use_cache || !file.exists(data_file)) {
      if (!have_api_key) {
        stop(paste("No API key set. Use options(cancensus.api_key = 'XXX') or",
                   "Sys.setenv(CM_API_KEY = 'XXX') to set one."))
      }
      url <- paste0(base_url, "data.csv?", param_string, "&api_key=", api_key)
      response <- if (!quiet) {
        message("Querying CensusMapper API...")
        httr::GET(url, httr::progress())
      } else {
        httr::GET(url)
      }
      handle_cm_status_code(response, NULL)


      # Read the data file and transform to proper data types
      result <- if (requireNamespace("readr", quietly = TRUE)) {
        # Use readr::read_csv if it's available.
        httr::content(response, type = "text", encoding = "UTF-8") %>%
          readr::read_csv(na = cancensus_na_strings,
                          col_types = list(.default = "c")) %>%
          dplyr::mutate_at(c(dplyr::intersect(names(.),c("Population","Households","Dwellings","Area (sq km)")),
                             names(.)[grepl("v_",names(.))]), as.num) %>%
          dplyr::mutate(Type = as.factor(.data$Type),
                        `Region Name` = as.factor(.data$`Region Name`))
      } else {
        httr::content(response, type = "text", encoding = "UTF-8") %>%
          textConnection %>%
          utils::read.csv(colClasses = "character", stringsAsFactors = FALSE, check.names = FALSE) %>%
          dplyr::as_tibble(.name_repair = "minimal") %>%
          dplyr::mutate_at(c(dplyr::intersect(names(.),c("Population","Households","Dwellings","Area (sq km)")),
                             names(.)[grepl("v_",names(.))]), as.num) %>%
          dplyr::mutate(Type = as.factor(.data$Type),
                        `Region Name` = as.factor(.data$`Region Name`))
      }
      if (is.na(geo_format)) result <- result %>% transform_geo(level)
      attr(result, "last_updated") <- Sys.time()
      save(result, file = data_file)
    } else {
      if (!quiet) message("Reading vectors data from local cache.")
      # Load `result` object from cache.
      load(file = data_file)
    }
  }

  if (!is.na(geo_format)) {
    param_string <- paste0("regions=", regions, "&level=", level, "&dataset=",
                           dataset)
    geo_base_name <- paste0("CM_geo_", digest::digest(param_string, algo = "md5"))
    geo_file <- cache_path(geo_base_name, ".geojson")
    if (!use_cache || !file.exists(geo_file)) {
      if (!have_api_key) {
        stop(paste("No API key set. Use options(cancensus.api_key = 'XXX') or",
                   "Sys.setenv(CM_API_KEY = 'XXX') to set one."))
      }
      url <- paste0(base_url, "geo.geojson?", param_string, "&api_key=",
                    api_key)
      response <- if (!quiet) {
        message("Querying CensusMapper API...")
        httr::GET(url, httr::progress())
      } else {
        httr::GET(url)
      }
      handle_cm_status_code(response, NULL)
      write(httr::content(response, type = "text", encoding = "UTF-8"), file = geo_file) # cache result
    } else {
      if (!quiet) message("Reading geo data from local cache.")
    }
    geos <- geojsonsf::geojson_sf(geo_file) %>%
      transform_geo(level)

    result <- if (is.null(result)) {
      geos
    } else if (!is.na(geo_format)) {
      # the sf object needs to be first in join to retain all spatial information
      to_remove <- setdiff(dplyr::intersect(names(geos),names(result)),"GeoUID")
      dplyr::select(result, -dplyr::one_of(to_remove)) %>%
        dplyr::inner_join(geos, ., by = "GeoUID")
    }
  }


  if (!is.na(geo_format) & geo_format=='sf') { # ensure sf format even if library not loaded
    result <- sf::st_as_sf(result)
  }

  if (length(vectors)>0) {
    census_vectors <- names(result)[grep("^v_", names(result))] %>%
      strsplit(., ": ") %>%
      dplyr::as_tibble(x=do.call(rbind, .),.name_repair = "minimal") %>%
      setNames(c("Vector", "Detail"))
    attr(result, "census_vectors") <- census_vectors
    if (labels == "short" | !is.null(names(vectors))) {
      if (!is.na(geo_format) && geo_format=="sp") {names(result@data) <- gsub(":.*","",names(result@data))}
      else {result <- result %>% dplyr::rename(!!!setNames(names(.),gsub(":.*","",names(.))))}
      if (!is.null(names(vectors))) result <- result %>% dplyr::rename(!!! vectors)
    }
  }

  if (!is.na(geo_format) & geo_format=='sp') { # ensure sf format even if library not loaded
    result <- sf::as_Spatial(result)
  }

  return(result)
}

#' Deprecated, use `get_census` instead
#'
#' @description
#' This function will be removed in future versions.
#'
#' @param dataset A CensusMapper dataset identifier.
#' @param regions A named list of census regions to retrieve. Names must be valid census aggregation levels.
#' @param level The census aggregation level to retrieve, defaults to \code{"Regions"}. One of \code{"Regions"}, \code{"PR"}, \code{"CMA"}, \code{"CD"}, \code{"CSD"}, \code{"CT"} or \code{"DA"}.
#' @param geo_format By default is set to \code{NA} and appends no geographic information. To include geographic information with census data, specify one of either \code{"sf"} to return an \code{\link[sf]{sf}} object (requires the \code{sf} package) or \code{"sp"} to return a \code{\link[sp]{SpatialPolygonsDataFrame-class}} object (requires the \code{rgdal} package).
#' @param ... Further arguments passed to \code{get_census}.
#'
#' @source Census data and boundary geographies are reproduced and distributed on
#' an "as is" basis with the permission of Statistics Canada (Statistics Canada
#' 2006; 2011; 2016).
#'
#' @export
#'
#' @examples
#' # Query the API for data geographies in Vancouver, at the census subdivision
#' # level:
#' \dontrun{
#' # Query the API for geographies in Vancouver, at the census subdivision
#' # level, and return the associated geography files in \code{sf} format:
#' census_data <- get_census(dataset='CA16', regions=list(CMA="59933"),
#'                           level='CSD', geo_format = "sf")
#'}
#' @export
#' @keywords internal
get_census_geometry <- function (dataset, regions, level=NA, geo_format = "sf", ...) {
  .Deprecated("get_census")
  stop("This function is no longer supported.")
  #return(get_census(dataset=dataset, regions=regions, level=level, geo_format=geo_format, ...))
}

# This is the set of valid census aggregation levels, also used in the named
# elements of the `regions` parameter.
VALID_LEVELS <- c("Regions","C","PR", "CMA", "CD", "CSD", "CT", "DA", 'EA', "DB")

#' Query the CensusMapper API for available datasets.
#'
#' @param use_cache If set to TRUE (the dfault), data will be read from a temporary local cache for the
#'   duration of the R session, if
#'   available. If set to FALSE, query the API for the data, and
#'   refresh the temporary cache with the result.
#' @param quiet When TRUE, suppress messages and warnings.
#'
#' @return
#'
#' Returns a data frame with a column \code{dataset} containing the code for the
#' dataset, a column \code{description} describing it, a \code{geo_dataset} column
#' identifying the geography dataset the data is based on, a \code{attribution} column
#' with an attribtuion string, a \code{reference} column with a reference identifier, and
#' a \code{reference_url} column with a link to reference materials.
#'
#' @export
#'
#' @examples
#'
#' # List available datasets in CensusMapper
#' list_census_datasets()
list_census_datasets <- function(use_cache = TRUE, quiet = FALSE) {
  cache_file <- file.path(tempdir(),"cancensus_datasets.rda") #cache_path("datasets.rda")
  if (!use_cache || !file.exists(cache_file)) {
    if (!quiet) message("Querying CensusMapper API for available datasets...")
    response <- httr::GET("https://censusmapper.ca/api/v1/list_datasets",
                          httr::accept_json())
    handle_cm_status_code(response, NULL)
    result <- httr::content(response, type = "text", encoding = "UTF-8") %>%
      jsonlite::fromJSON() %>%
      dplyr::as_tibble(.name_repair = "minimal")
    #names(result) <- c("dataset","description","geo_dataset")
    attr(result, "last_updated") <- Sys.time()
    save(result, file = cache_file)
    result
  } else {
    if (!quiet) message("Reading dataset list from temporary cache.")
    load(file = cache_file)
    last_updated <- attr(result, "last_updated")
    if (!quiet && is.null(last_updated) ||
          difftime(Sys.time(), last_updated, units = "days") > 1) {
      warning(paste("Cached dataset list may be out of date. Set `use_cache =",
                    "FALSE` to update it."))
    }
    result
  }
}

#' Get attribution for datasets
#'
#' @param datasets Vector of dataset identifiers
#'
#' @return
#'
#' Returns a string to be used as attribution for the given datasets.
#'
#' @export
#'
#' @examples
#'
#' # Attribution string for the 2006 and 2016 census datasets
#' dataset_attribution(c('CA06','CA16'))
dataset_attribution <- function(datasets){
  attribution <-list_census_datasets(quiet=TRUE) %>%
    dplyr::filter(.data$dataset %in% datasets) %>%
    dplyr::pull(.data$attribution)

  commons <- gsub("\\d{4}","\\\\\\d{4}",attribution) %>%
    unique()

  commons %>% lapply(function(c){
    matches <- attribution[grepl(paste0("^",c,"$"),attribution)]
    parts <- strsplit(c, split = "\\\\d\\{4\\}") %>%
      unlist()
    years <- matches
    for (p in parts){
      years <- gsub(p,"",years)
    }

    year_string <- paste0(years,collapse=", ")
    gsub("\\d{4}",paste0(years,collapse=", "),matches[[1]])
  }) %>%
    unlist() %>%
    paste0(collapse="; ")
}

#' Return Census variable names and labels as a tidy data frame
#'
#' @param x A data frame, \code{sp} or \code{sf} object returned from
#'   \code{get_census} or similar.
#'
#' @return
#'
#' A data frame with a column \code{variable} containing the truncated
#' variable name, and a column \code{label} describing it.
#'
#'@examples
#'\dontrun{
#' # Query census data with truncated labels:
#' label_data <- get_census(dataset='CA16', regions=list(CMA="59933"),
#'                           vectors=c("v_CA16_408","v_CA16_409","v_CA16_410"),
#'                           level='CSD', geo_format = "sf", labels="short")
#'
#' # Get details for truncated vectors:
#' label_vectors(label_data)
#' }
#' @export
label_vectors <-  function(x) {
  if("census_vectors" %in% names(attributes(x))) {
    attr(x, "census_vectors")
  } else {
    warning("Data does not have variables to labels. No Census variables selected as vectors. See ?get_census() for more information. ")}
}

#' Return Census variable names and labels as a tidy data frame (Deprecated)
#'
#' @param x A data frame, \code{sp} or \code{sf} object returned from
#'   \code{get_census} or similar.
#'
#' @return
#'
#' A data frame with a column \code{variable} containing the truncated
#' variable name, and a column \code{label} describing it.
#'
#'@examples
#' \dontrun{
#' # Query census data with truncated labels:
#' census_data <- get_census(dataset='CA16', regions=list(CMA="59933"),
#'                           vectors=c("v_CA16_408","v_CA16_409","v_CA16_410"),
#'                           level='CSD', geo_format = "sf", labels="short")
#'
#' # Get details for truncated vectors:
#' census_vectors(census_data)
#' }
#' @export
census_vectors <-  function(x) {
  warning("census_vectors() is deprecated. Please use label_vectors() to view details for truncated variable labels.")
  if("census_vectors" %in% names(attributes(x))) {
    attr(x, "census_vectors")
  } else {
    warning("Data does not have variables to labels. No Census variables selected as vectors. See ?get_census() for more information. ")}
}



# Internal function to handle unfavourable HTTP responses
handle_cm_status_code <- function(response,path){
  if (httr::status_code(response)!=200) {
    message=httr::content(response,as="text")
    if (!is.null(path)) {
      file.remove(path)
    }
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


# Transform and rename geometry data.
transform_geo <- function(g, level) {
  as_character=c("id","rpid","rgid","ruid","rguid","q")
  as_numeric=c("a","nrr")
  as_factor=c("t")
  as_integer=c("pop","dw","hh","pop2","pop11","pop16","hh11","hh16","dw11","dw16")
  as_character=append(append(as_character,as_numeric),as_integer)

  g <- g %>%
    dplyr::mutate_at(dplyr::intersect(names(g), as_character), as.character) %>%
    dplyr::mutate_at(dplyr::intersect(names(g), as_numeric), as.numeric)  %>%
    dplyr::mutate_at(dplyr::intersect(names(g), as_integer), as.int)  %>%
    dplyr::mutate_at(dplyr::intersect(names(g), as_factor), as.factor)

  # Change names
  # Standard table
  name_change <- dplyr::tibble(
    old=c("id","a" ,"t" ,"dw","hh","pop","pop2","nrr","q","pop11","pop16","hh11","hh16","dw11","dw16"),
    new=c("GeoUID","Shape Area" ,"Type" ,"Dwellings","Households","Population","Adjusted Population (previous Census)","NHS Non-Return Rate","Quality Flags","Population 2011","Population 2016","Households 2011","Households 2016","Dwellings 2011","Dwellings 2016")
  )
  # Geo UID name changes
  if (level=='Regions') {
    l=g$t %>% unique()
    if (length(l)==1) level=l
  }
  if (level=='DB') {
    name_change <- name_change %>% rbind(
      c('rpid','DA_UID'),
      c('rgid','CSD_UID'),
      c('ruid','CT_UID'),
      c('rguid','CMA_UID'))
  }
  if (level=='DA'|level=='EA') {
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
      c('rpid','CD_UID'),
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

  to_remove <- dplyr::intersect(names(g),c("rpid","rgid","ruid","rguid"))
  if (length(to_remove)>0) g <- g %>% dplyr::select(-dplyr::one_of(to_remove))

  return(g)
}

# Append arguments to the path of the local cache directory.
cache_path <- function(...) {
  cache_dir <- getOption("cancensus.cache_path")
  if (!is.character(cache_dir)) {
    stop("Corrupt 'cancensus.cache_path' option. Must be a path.",
         .call = FALSE)
  }
  if (!file.exists(cache_dir)) {
    dir.create(cache_dir, showWarnings = FALSE)
  }
  paste0(cache_dir, "/", ...)
}

.onAttach <- function(libname, pkgname) {
  if (!"cancensus.api_key" %in% names(options())) {
    # Try to get the API key from the CM_API_KEY environment variable, if it has not already been specified.
    options(cancensus.api_key = if (nchar(Sys.getenv("CM_API_KEY")) > 1) { Sys.getenv("CM_API_KEY") } else { NULL })
  }

  if (!"cancensus.cache_path" %in% names(options())) {
    # Cache in tmp dir by default.
    options(cancensus.cache_path = tempdir())
    packageStartupMessage(paste("Census data is currently stored temporarily.\n\n",
                  "In order to speed up performance, reduce API quota usage, and reduce",
                  "unnecessary network calls, please set up a persistent cache directory by",
                  "setting options(cancensus.cache_path = '<path to cancensus cache directory>')\n\n",
                  "You may add this option, together with your API key, to your .Rprofile."))
  }
}

# Suppress warnings for missing bindings for '.' in R CMD check.
if (getRversion() >= "2.15.1") utils::globalVariables(c("."))

#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @importFrom stats setNames
NULL
