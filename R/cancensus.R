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
#' @param geo_format By default is set to \code{NA} and appends no geographic information. To include geographic information with census data, specify one of either \code{"sf"} to return an \code{\link[sf]{sf}} object (requires the \code{sf} package) or \code{"sp"} to return a \code{\link[sp]{SpatialPolygonsDataFrame-class}} object (requires the \code{rgdal} package). If user requests geo-spatial data and neither package is available, a context menu will prompt to install the \code{sf} package.
#' @param resolution Resolution of the geographic data. {cancensus} will download simplified geometries by default. For lower level geometries like DB or DA this will be very close to the high resolution data.
#' Simplification generally increases as the geographic aggregation level increases.
#' If high resolution geometries are required
#' then this option can be set to 'high'. By default this setting is set to \code{'simplified'}.
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
get_census <- function (dataset, regions, level=NA, vectors=c(), geo_format = NA,
                        resolution = 'simplified',
                        labels = "detailed",
                        use_cache=TRUE, quiet=FALSE, api_key=Sys.getenv("CM_API_KEY"))
  {

  # API and data recall checks
  first_run_checks()
  api_key <- robust_api_key(api_key)
  have_api_key <- valid_api_key(api_key)
  result <- NULL
  data_version<-NULL
  geo_version<-NULL

  dataset <- translate_dataset(dataset)

  # Check region selection validity
  if (is.na(level)) level="Regions"

  # Check spatial resolution
  if (is.na(geo_format) && !(resolution %in% c("simplified","high"))) stop("The resolution paramerter needs to be either 'simplified' or 'high'.")

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

  # Check if the aggregation level is valid.
  if (!level %in% VALID_LEVELS) {
    stop("the `level` parameter must be one of 'Regions', 'PR', 'CMA', 'CD', 'CSD', 'CT', 'DA', 'EA' or 'DB'")
  }

  # Check that we can read the requested geo format.
  if (is.na(geo_format)) { # This is ok.
  } else if (!geo_format %in% c("sf", "sp")) {
    stop("the `geo_format` parameter must be one of 'sf', 'sp', or NA")
  } else if (!is.na(geo_format) && !requireNamespace("sf", quietly = TRUE)) {
    stop("The `sf` package is required to return geographies.")
  }

  # Check if SF is installed when asking for spatial data
  if(geo_format == "sf" && !("sf" %in% utils::installed.packages())) {
    if (utils::menu(c("Yes", "No"),
             title= paste("The `sf` package is required to return geographies. Would you like to install?")) == "1") {
      utils::install.packages('sf')
    } else {
      print("Cancelling installation and retrieving tabular data only.")
      geo_format <- NA
    }
  }

  base_url=paste0(cancensus_base_url(),"/api/v1/")
  # load data variables
  if (length(vectors)>0 || is.na(geo_format)) {
    params <- list(regions=as.character(regions),
                   vectors=jsonlite::toJSON(vectors),
                   level=level,
                   dataset=dataset,
                   api_key=api_key)
    param_string <- paste0("regions=", as.character(regions),
                           # Convert vectors to a JSON list.
                           "&vectors=", jsonlite::toJSON(as.character(vectors)),
                           "&level=", level, "&dataset=", dataset)
    if (is.na(geo_format)) param_string=paste0(param_string,"&geo_hierarchy=true")
    if (is.na(geo_format)) params["geo_hierarchy"]="true"
    data_base_name <- paste0("CM_data_",digest::digest(param_string, algo = "md5"))
    data_file <- cache_path(data_base_name, ".rda")
    meta_file <- paste0(data_file, ".meta")
    if (!use_cache || !file.exists(data_file)) {
      if (!have_api_key) {
        stop(paste("No API key set. Use set_api_key('<your API ket>`) to set one, or set_api_key('<your API ket>`, install = TRUE) to save is permanently in our .Renviron."))
      }
      url <- paste0(base_url, "data.csv")
      response <- if (!quiet) {
        message("Querying CensusMapper API...")
        httr::POST(url, httr::progress(), body=params)
      } else {
        httr::POST(url, body=params)
      }
      handle_cm_status_code(response, NULL)
      data_version <- response$headers$`data-version`


      # Read the data file and transform to proper data types
      result <- if (requireNamespace("readr", quietly = TRUE)) {
        # Use readr::read_csv if it's available.
        httr::content(response, type = "text", encoding = "UTF-8") %>%
          readr::read_csv(na = cancensus_na_strings,
                          col_types = list(.default = "c"))
      } else {
        check_recalled_data_and_warn(meta_file,params)
        httr::content(response, type = "text", encoding = "UTF-8") %>%
          textConnection %>%
          utils::read.csv(colClasses = "character", stringsAsFactors = FALSE, check.names = FALSE) %>%
          dplyr::as_tibble(.name_repair = "minimal")
      }
      result <- result %>%
        dplyr::mutate_at(c(dplyr::intersect(names(.),c("Population","Households","Dwellings","Area (sq km)")),
                           names(.)[grepl("v_",names(.))]), as.num) %>%
        dplyr::mutate(Type = as.factor(.data$Type),
                      `Region Name` = as.factor(.data$`Region Name`))

      if (is.na(geo_format)) result <- result %>% transform_geo(level)
      attr(result, "last_updated") <- Sys.time()
      save(result, file = data_file)
      file_info <- file.info(data_file)
      metadata <- dplyr::tibble(dataset=dataset,regions=as.character(regions),
                                 level=level,
                                 vectors=jsonlite::toJSON(as.character(vectors))  %>% as.character(),
                                 created_at=Sys.time(),
                         version=data_version,size=file_info$size)
      saveRDS(metadata, file = meta_file)
    } else {
      if (!quiet) message("Reading vectors data from local cache.")
      # Load `result` object from cache.
      load(file = data_file)
    }
    touch_metadata(meta_file,params)
  }

  if (!is.na(geo_format)) {
    params <- list(regions=regions,
                   level=level,
                   dataset=dataset,
                   resolution=resolution,
                   api_key=api_key)
    param_string <- paste0("regions=", regions,
                           "&level=", level,
                           "&dataset=", dataset)
    if (resolution !="simplified") param_string <- paste0(param_string,"&resolution=",resolution)
    geo_base_name <- paste0("CM_geo_", digest::digest(param_string, algo = "md5"))
    geo_file <- cache_path(geo_base_name, ".geojson")
    meta_file <- paste0(geo_file, ".meta")
    if (!use_cache || !file.exists(geo_file)) {
      if (!have_api_key) {
        stop(paste("No API key set. Use set_api_key('<your API ket>`) to set one, or set_api_key('<your API ket>`, install = TRUE) to save is permanently in our .Renviron."))
      }
      url <- paste0(base_url, "geo.geojson")
      response <- if (!quiet) {
        message("Querying CensusMapper API...")
        httr::POST(url, httr::progress(),body=params)
      } else {
        httr::POST(url,body=params)
      }
      handle_cm_status_code(response, NULL)
      geo_version <- response$headers$`data-version`
      write(httr::content(response, type = "text", encoding = "UTF-8"), file = geo_file) # cache result
      file_info <- file.info(geo_file)
      metadata <- dplyr::tibble(dataset=dataset,regions=as.character(regions),
                                level=level,created_at=Sys.time(),
                                resolution=resolution,
                         version=geo_version,size=file_info$size)
      saveRDS(metadata, file = meta_file)
    } else {
      if (!quiet) message("Reading geo data from local cache.")
      check_recalled_data_and_warn(meta_file,params)
    }
    geos <- geojsonsf::geojson_sf(geo_file) %>%
      sf::st_sf(agr="constant") %>% # need to set this, otherwise sf complains
       transform_geo(level) #%>%
      # sf::st_sf(agr="constant") # just in case


    result <- if (is.null(result)) {
      geos
    } else if (!is.na(geo_format)) {
      # the sf object needs to be first in join to retain all spatial information
      to_remove <- setdiff(dplyr::intersect(names(geos),names(result)),"GeoUID")
      dplyr::select(result, -dplyr::one_of(to_remove)) %>%
        dplyr::inner_join(geos, ., by = "GeoUID")
    }
    touch_metadata(meta_file,params)
  }

  # ensure sf format even if library not loaded and set agr columns
  if (!is.na(geo_format) & geo_format=='sf') {
    numerics <- result %>% dplyr::select_if(function(d)is.numeric(d)|is.integer(d)) %>%
      names()
    non_numerics <- setdiff(names(result),c(numerics,"geometry"))
    agr_cols <- c(setNames(rep_len("aggregate",length(numerics)),numerics),
                  setNames(rep_len("identity",length(non_numerics)),non_numerics))
    result <- result %>%
      #sf::st_sf(agr=agr_cols) # something wrong here, does not work for `Region Name` column. maybe bug in sf?
      sf::st_sf(agr="constant")
  }


  if (length(vectors)>0) {
    census_vectors <- names(result)[grep("^v_", names(result))] %>%
      strsplit(., ": ") %>%
      dplyr::as_tibble(x=do.call(rbind, .),.name_repair = "minimal") %>%
      setNames(c("Vector", "Detail"))
    attr(result, "census_vectors") <- census_vectors
    if (labels == "short" | !is.null(names(vectors))) {
      to_rename <- setNames(names(result),gsub(":.*","",names(result)))
      to_rename <- to_rename[names(to_rename)!=as.character(to_rename)]
      if (length(to_rename)>0) result <- result %>% dplyr::rename(!!!to_rename)
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
VALID_LEVELS <- c("Regions","C","PR", "CMA", "CD", "CSD", "ADA","CT", "DA", 'EA', "DB")

#' Query the CensusMapper API for available datasets.
#'
#' @param use_cache If set to TRUE (the default), data will be read from a temporary local cache for the
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
#' with an attribution string, a \code{reference} column with a reference identifier, and
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
  datasets <-   lapply(datasets,translate_dataset) %>% unlist()
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
#' @keywords internal
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


name_change_for_level <- function(level){
  if (level=='DB') {
    name_change <- c('DA_UID'='rpid',
                     'CSD_UID'='rgid',
                     'CT_UID'='ruid',
                     'CMA_UID'='rguid')
  } else  if (level=='DA'|level=='EA') {
    name_change <- c('CSD_UID'='rpid',
                     'CD_UID'='rgid',
                     'CT_UID'='ruid',
                     'CMA_UID'='rguid')
  } else if (level=='CT') {
    name_change <- c('CMA_UID'='rpid',
                     'PR_UID'='rgid',
                     'CSD_UID'='ruid',
                     'CD_UID'='rguid')
  } else if (level=='CSD') {
    name_change <- c('CD_UID'='rpid',
                     'PR_UID'='rgid',
                     'CMA_UID'='ruid')
  } else if (level=='CD') {
    name_change <- c('PR_UID'='rpid',
                     'C_UID'='rgid')
  } else if (level=='CMA') {
    name_change <- c('PR_UID'='rpid',
                     'C_UID'='rgid')
  } else if (level=='PR') {
    name_change <- c('C_UID'='rpid')
  } else if (level=='C') {
    name_change <- c()
  } else {
    name_change <- c()
    warning(paste0("Unknown level ",level))
  }
  name_change
}

base_name_change <- c("GeoUID"="id",
                      "Shape Area"="a",
                      "Type"="t",
                      "Dwellings"="dw",
                      "Households"="hh",
                      "Population"="pop",
                      "Adjusted Population (previous Census)"="pop2",
                      "NHS Non-Return Rate"="nrr",
                      "Quality Flags"="q",
                      "Population 2011"="pop11",
                      "Population 2016"="pop16",
                      "Households 2011"="hh11",
                      "Households 2016"="hh16",
                      "Dwellings 2011"="dw11",
                      "Dwellings 2016"="dw16")

# Transform and rename geometry data.
transform_geo <- function(g, level) {
  as_character=c("id","rpid","rgid","ruid","rguid","q")
  as_numeric=c("a","nrr")
  as_factor=c("t")
  as_integer=c("pop","dw","hh","pop2","pop11","pop16","hh11","hh16","dw11","dw16")
  #as_character=c(as_character,as_numeric,as_integer)

  to_rename <- base_name_change[as.character(base_name_change) %in% names(g)]

  g <- g %>%
    dplyr::mutate_at(dplyr::intersect(names(g), as_character), as.character) %>%
    dplyr::mutate_at(dplyr::intersect(names(g), as_numeric), as.numeric)  %>%
    dplyr::mutate_at(dplyr::intersect(names(g), as_integer), as.int)  %>%
    dplyr::mutate_at(dplyr::intersect(names(g), as_factor), as.factor) %>%
    dplyr::rename(!!to_rename)

  if (level != "Regions") {
    rc <- name_change_for_level(level)[as.character(name_change_for_level(level)) %in% names(g)]
    if (length(rc)>0) g <- g %>%  dplyr::rename(!!!rc)
  } else if ("Type" %in% names(g)) {
    g <- g$Type %>%
      unique %>%
      lapply(function(t){
        g <- g %>%  dplyr::filter(.data$Type==t)
        rc <- name_change_for_level(t)[as.character(name_change_for_level(t)) %in% names(g)]
        if (length(rc)>0) {
          g <- g %>%
            dplyr::rename(!!!rc) %>%
            dplyr::mutate_at(names(rc),function(d)dplyr::na_if(d,""))
        }
        g
      }) %>%
      dplyr::bind_rows()
  }

  to_remove <- dplyr::intersect(c("rpid","rgid","ruid","rguid"),names(g))
  if (length(to_remove)>0) g <- g %>% dplyr::select(-dplyr::one_of(to_remove))

  return(g)
}



.onAttach <- function(libname, pkgname) {
 # if (!"cancensus.api_key" %in% names(options())) {
    # Try to get the API key from the CM_API_KEY environment variable, if it has not already been specified.
#    options(cancensus.api_key = if (nchar(Sys.getenv("CM_API_KEY")) > 1) { Sys.getenv("CM_API_KEY") } else { NULL })
#  }


  if (!("cancensus.cache_path" %in% names(options())) & nchar(Sys.getenv("CM_CACHE_PATH"))==0) {
    # Cache in tmp dir by default.
    packageStartupMessage(cm_no_cache_path_message)
  }
}

# Suppress warnings for missing bindings for '.' in R CMD check.
if (getRversion() >= "2.15.1") utils::globalVariables(c("."))

#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @importFrom stats setNames
NULL
