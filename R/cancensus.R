#' Access to Canadian census data through the CensusMapper API
#'
#' This function allows convenient access to Canadian census data and boundary
#' files through the CensusMapper API. An API key is required to retrieve data.
#'
#' \code{get_census_geometry} is a convenience function
#' that retrieves only Census geography boundaries.
#'
#' For help selecting regions and vectors, see \code{\link{list_census_regions}}
#' and \code{\link{list_census_vectors}}, or check out the interactive selection
#' tool at \url{https://censusmapper.ca/api}.
#'
#' @param dataset A CensusMapper dataset identifier.
#' @param regions A named list of census regions to retrieve. Names must be valid census aggregation levels.
#' @param level The census aggregation level to retrieve, defaults to \code{"Regions"}. One of \code{"Regions"}, \code{"PR"}, \code{"CMA"}, \code{"CD"}, \code{"CSD"}, \code{"CT"} or \code{"DA"}.
#' @param vectors An R vector containing the CensusMapper variable names of the census variables to download. If no vectors are specified only geographic data will get downloaded.
#' @param geo_format By default is set to \code{NA} and appends no geographic information. To include geographic information with census data, specify one of either \code{"sf"} to return an \code{\link[sf]{sf}} object (requires the \code{sf} package) or \code{"sp"} to return a \code{\link[sp]{SpatialPolygonsDataFrame}} object (requires the \code{rgdal} package).
#' @param labels Set to "detailed" by default, but truncated Census variable names can be selected by setting labels = "short". Use \code{label_vectors(...)} to return variable label information in detail.
#' @param use_cache If set to TRUE (the default) data will be read from the local cache if available.
#' @param quiet When TRUE, suppress messages and warnings.
#' @param api_key An API key for the CensusMapper API. Defaults to \code{options()} and then the \code{CM_API_KEY} environment variable.
#' @param ... Further arguments passed to \code{get_census}.
#'
#' @keywords canada census data api
#'
#' @source Census data and boundary geographies are reproduced and distributed on
#' an "as is" basis with the permission of Statistics Canada (Statistics Canada
#' 2006; 2011; 2016).
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
#' # Make the same query, but return geography in \code{sp} format:
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
  api_key <- if (is.null(api_key) && nchar(Sys.getenv("CM_API_KEY")) > 1) { Sys.getenv("CM_API_KEY") } else { api_key }
  have_api_key <- !is.null(api_key)
  result <- NULL

  if (is.na(level)) level="Regions"

  # Turn the region list into a valid JSON dictionary.
  if (is.character(regions)) {
    if (!quiet) warning(paste("passing `regions` as a character vector is",
                              "depreciated, and will be removed in future",
                              "versions"))
  } else if (is.null(names(regions)) || !all(names(regions) %in% VALID_LEVELS)) {
    stop("regions must be composed of valid census aggregation levels.")
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
      na_strings <- c("x", "F", "...", "..", "-")
      # Read the data file and transform to proper data types
      result <- if (requireNamespace("readr", quietly = TRUE)) {
        # Use readr::read_csv if it's available.
        httr::content(response, type = "text", encoding = "UTF-8") %>%
          readr::read_csv(na = na_strings,
                          col_types = list(.default = "d", GeoUID = "c",
                                           Type = "c", "Region Name" = "c")) %>%
          dplyr::mutate(Type = as.factor(.data$Type),
                        `Region Name` = as.factor(.data$`Region Name`))
      } else {
        httr::content(response, type = "text", encoding = "UTF-8") %>%
          textConnection %>%
          utils::read.csv(na = na_strings,
                          colClasses = c("GeoUID" = "character",
                                         "Type" = "factor",
                                         "Region Name" = "factor"),
                          stringsAsFactors = FALSE, check.names = FALSE) %>%
          dplyr::as_tibble()
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
    geos <- if (geo_format == "sf") {
      sf::read_sf(geo_file) %>%
        transform_geo(level)
    } else { # geo_format == "sp"
      geos <- tryCatch({
        rgdal::readOGR(geo_file,geo_base_name)
      }, error = function(e) {
        rgdal::readOGR(geo_file, "OGRGeoJSON")
      }, silent = TRUE)
      geos@data <- transform_geo(geos@data, level)
      geos
    }

    result <- if (is.null(result)) {
      geos
    } else if (geo_format == "sf") {
      # the sf object needs to be first in join to retain all spatial information
      dplyr::select(result, -.data$Population, -.data$Dwellings,
                    -.data$Households, -.data$Type) %>%
        dplyr::inner_join(geos, ., by = "GeoUID")
    } else { # geo_format == "sp"
      geos@data <- dplyr::select(geos@data, -.data$Population, -.data$Dwellings,
                                 -.data$Households, -.data$Type)
      sp::merge(geos, result, by = "GeoUID")
    }
  }


  if (!is.na(geo_format) & geo_format=='sf') { # ensure sf format even if library not loaded
    result <- sf::st_as_sf(result)
  }

  if (length(vectors)>0) {
    census_vectors <- names(result)[grep("^v_", names(result))]
    census_vectors <- strsplit(census_vectors, ": ")
    census_vectors <- dplyr::as_data_frame(do.call(rbind, census_vectors))
    names(census_vectors) <- c("Vector", "Detail")
    attr(result, "census_vectors") <- census_vectors
    if (labels == "short" | !is.null(names(vectors))) {
      if (!is.na(geo_format) && geo_format=="sp") {names(result@data) <- gsub(":.*","",names(result@data))}
      else {names(result) <- gsub(":.*","",names(result))}
      if (!is.null(names(vectors))) result <- result %>% dplyr::rename(!!! vectors)
    }
  }

  return(result)
}

#' @rdname get_census
#' @export
#'
#' @examples
#' \dontrun{
#' # Query the API for census subdivision boundary geometry within Vancouver.
#' vc_csds <- get_census_geometry(dataset='CA16', regions=list(CMA="59933"),
#'                                level='CSD', geo_format = "sf")
#'}
get_census_geometry <- function (dataset, level, regions, geo_format = "sf", ...) {
  return(get_census(dataset, level, regions, vectors=c(), geo_format=geo_format, ...))
}

# This is the set of valid census aggregation levels, also used in the named
# elements of the `regions` parameter.
VALID_LEVELS <- c("Regions","C","PR", "CMA", "CD", "CSD", "CT", "DA", "DB")

#' Query the CensusMapper API for available datasets.
#'
#' @param use_cache If set to TRUE, data will be read from a local cache, if
#'   available. If set to FALSE (the default), query the API for the data, and
#'   refresh the local cache with the result.
#' @param quiet When TRUE, suppress messages and warnings.
#'
#' @return
#'
#' Returns a data frame with a column \code{dataset} containing the code for the
#' dataset, and a column \code{description} describing it.
#'
#' @export
#'
#' @examples
#'
#' # List available datasets in CensusMapper
#' list_census_datasets()
list_census_datasets <- function(use_cache = FALSE, quiet = FALSE) {
  cache_file <- cache_path("datasets.rda")
  if (!use_cache || !file.exists(cache_file)) {
    if (!quiet) message("Querying CensusMapper API for available datasets...")
    response <- httr::GET("https://censusmapper.ca/api/v1/list_datasets",
                          httr::accept_json())
    handle_cm_status_code(response, NULL)
    result <- httr::content(response, type = "text", encoding = "UTF-8") %>%
      jsonlite::fromJSON() %>%
      dplyr::as_data_frame()
    attr(result, "last_updated") <- Sys.time()
    save(result, file = cache_file)
    result
  } else {
    if (!quiet) message("Reading dataset list from local cache.")
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

#' Query the CensusMapper API for available vectors for a given dataset.
#'
#' @param dataset The dataset to query for available vectors, e.g.
#'   \code{"CA16"}.
#' @param use_cache If set to TRUE, data will be read from a local cache, if
#'   available. If set to FALSE (the default), query the API for the data, and
#'   refresh the local cache with the result.
#' @param quiet When FALSE, shows messages and warnings. Set to TRUE by default.
#'
#' @return
#' Returns a data frame detailing the available Census vectors (i.e. variables) for a given Census
#' dataset. This data frame has columns \code{vector} containing the short code for the
#' variable, \code{type} describing whether it's a female, male, or total aggregate, \code{label}
#' indicating the name of the variable, \code{units} indicating whether the value represents a
#' numeric integer, percentage, dollar figure, or ratio, \code{parent_vector} to show hierarchical
#' relationship, \code{aggregation} indicating whether the value is additive or a transformation,
#' and a column \code{details} with a detailed description of the variable generated by traversing
#' all labels within its hierarchical structure.
#'
#' @export
#'
#' @examples
#'
#' # List all vectors for a given Census dataset in CensusMapper
#' list_census_vectors('CA16')
list_census_vectors <- function(dataset, use_cache = FALSE, quiet = TRUE) {
  cache_file <- cache_path(dataset, "_vectors.rda")
  if (!use_cache || !file.exists(cache_file)) {
    url <- paste0("https://censusmapper.ca/api/v1/vector_info/", dataset,
                  ".csv")
    response <- if (!quiet) {
      message("Querying CensusMapper API for vectors data...")
      httr::GET(url, httr::progress())
    } else {
      httr::GET(url)
    }
    handle_cm_status_code(response, NULL)
    content <- httr::content(response, type = "text", encoding = "UTF-8")
    result <- if (!requireNamespace("readr", quietly = TRUE)) {
      dplyr::as_data_frame(utils::read.csv(textConnection(content), stringsAsFactors = FALSE))
    } else {
      readr::read_csv(content)
    }
    result <- dplyr::mutate(
      result, type = factor(.data$type),
      units = factor(units, levels = as.character(1:5),
                     labels = c("Number", "Percentage ratio (0.0-1.0)",
                                "Currency", "Ratio", "Percentage (0-100)")),
      aggregation = dplyr::case_when(
        add == "1" ~ "Additive",
        add == "0" ~ "Not additive",
        grepl("^2.", add) ~ gsub(".", ", ", gsub("^2.", "Average of ", add),
                                 fixed = TRUE),
        grepl("^3.", add) ~ gsub(".", ", ", gsub("^3.", "Median of ", add),
                                 fixed = TRUE)
      )) %>%
    dplyr::select(.data$vector, .data$type, .data$label, .data$units,
                  parent_vector = .data$parent, .data$aggregation,
                  .data$details)
    attr(result, "last_updated") <- Sys.time()
    attr(result, "dataset") <- dataset
    save(result, file = cache_file)
    result
  } else {
    if (!quiet) message("Reading vector information from local cache.")
    load(file = cache_file)
    last_updated <- attr(result, "last_updated")
    if (!quiet && is.null(last_updated) ||
          difftime(Sys.time(), last_updated, units = "days") > 1) {
      warning(paste("Cached vectors list may be out of date. Set `use_cache =",
                    "FALSE` to update it."))
    }
    attr(result, "dataset") <- dataset # just in case, catching cached legacy datasets
    result
  }
}

#' List all parent variables from vector hierarchical based on a (sub-)list of census
#' variables returned by
#' \code{list_census_vectors} or \code{search_census_vectors}.
#'
#' @param vector_list The list of vectors to be used
#'
#' @export
#'
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' list_census_vectors("CA16") %>%
#'   filter(vector == "v_CA16_4092") %>%
#'   parent_census_vectors()
parent_census_vectors <- function(vector_list){
  base_list <- vector_list
  dataset <- attr(base_list, "dataset")
  n=0
  vector_list <-
    list_census_vectors(dataset, use_cache = TRUE, quiet = TRUE) %>%
    dplyr::filter(vector %in% base_list$parent_vector) %>%
    dplyr::distinct(vector, .keep_all = TRUE)
  while (n!=nrow(vector_list)) {
    n=nrow(vector_list)
    new_list <- list_census_vectors(dataset, use_cache = TRUE, quiet = TRUE) %>%
      dplyr::filter(vector %in% vector_list$parent_vector)
    vector_list <- vector_list %>% rbind(new_list) %>%
      dplyr::distinct(vector, .keep_all = TRUE)
  }
  attr(vector_list, "dataset") <- dataset
  return(vector_list)
}

#' List all child variables from vector hierarchical based on a (sub-)list of census
#' variables returned by
#' \code{list_census_vectors} or \code{search_census_vectors}.
#'
#' @param vector_list The list of vectors to be used
#' @param leaves_only Boolean flag to indicate if only leaf vectors should be returned,
#' i.e. vectors that don't have children
#'
#' @export
#'
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' list_census_vectors("CA16") %>%
#'   filter(vector == "v_CA16_4092") %>%
#'   child_census_vectors(TRUE)
child_census_vectors <- function(vector_list, leaves_only=FALSE){
  base_list <- vector_list
  dataset <- attr(base_list,'dataset')
  n=0
  if (!is.null(dataset)) {
    vector_list <-
      list_census_vectors(dataset, use_cache = TRUE, quiet = TRUE) %>%
      dplyr::filter(.data$parent_vector %in% base_list$vector) %>%
      dplyr::distinct(vector, .keep_all = TRUE)
    while (n!=nrow(vector_list)) {
      n=nrow(vector_list)
      new_list <- list_census_vectors(dataset, use_cache = TRUE, quiet = TRUE) %>%
        dplyr::filter(.data$parent_vector %in% vector_list$vector)
      vector_list <- vector_list %>% rbind(new_list) %>%
        dplyr::distinct(vector, .keep_all = TRUE)
    }
    # only keep leaves if leaves_only==TRUE
    if (leaves_only) {
      vector_list <- vector_list %>%
        dplyr::filter(!(vector %in% list_census_vectors(dataset, use_cache = TRUE, quiet = TRUE)$parent_vector))
    }
    attr(vector_list, "dataset") <- dataset
  }
  return(vector_list)
}

#' Query the CensusMapper API for vectors with descriptions matching a searchterm.
#'
#' @param searchterm The term to search for e.g. \code{"Ojibway"}.
#' Search terms are case insensitive. If unable to find a given search term,
#' this function will suggest the correct spelling to use when possible.
#' @param dataset The dataset to query for available vectors, e.g.
#'   \code{"CA16"}.
#' @param type One of \code{NA}, \code{'Total'}, \code{'Male'} or \code{'Female'}.
#' If specified, only return variables of specified `type`.
#' @param ... Further arguments passed on to \code{\link{list_census_vectors}}.
#'
#' @export
#'
#' @examples
#' search_census_vectors('Ojibway', 'CA16')
#'\dontrun{
#' # This will return a warning that no match was found, but will suggest similar terms.
#' search_census_vectors('Ojibwe', 'CA16', 'Total')
#' }
search_census_vectors <- function(searchterm, dataset, type=NA, ...) {
  #to do: add caching of vector list here
  veclist <- list_census_vectors(dataset, ...)
  result <- veclist[grep(searchterm, veclist$label, ignore.case = TRUE),]

  # filter by type if needed
  if (!is.na(type) && length(rownames(result)) > 0) {
    result <- result[result$type==type,]
  }

  # Check if searchterm returned anything
  if (length(rownames(result)) > 0 ) {
    attr(result, "dataset") <- dataset
    return(result)
  }
  # If nothing matches, throw a warning and suggested alternatives.
  # If no suggested alternatives because the typo is too egregious, throw an error.
  else {
    # Check for similarly named terms. Uses base function agrep which is based on the Levenshtein edit distance for string similarity.
    # Default is set to 0.1 - can expand this to be more tolerant still.
    hintlist <- dplyr::as_tibble(unique(agrep(searchterm, veclist$label, ignore.case = TRUE, value = TRUE)))
    names(hintlist) <- "Similarly named objects"
    #
    if (length(hintlist) > 0) {
    warning("No results found. Please use accurate spelling. See above for list of variables with similar named terms.")
    print(hintlist)
    } else {
      stop("No results found.")
    }
  }
}

#' Query the CensusMapper API for available regions in a given dataset.
#'
#' @param dataset The dataset to query for available regions, e.g.
#'   \code{"CA16"}.
#' @param use_cache If set to TRUE, data will be read from a local cache, if
#'   available. If set to FALSE (the default), query the API for the data, and
#'   refresh the local cache with the result.
#' @param quiet When TRUE, suppress messages and warnings.
#'
#' @return
#'
#' Returns a data frame with the following columns:
#'
#' \describe{
#'   \item{\code{region}}{The region identifier.}
#'
#'   \item{\code{name}}{The name of that region.}
#'
#'   \item{\code{level}}{The census aggregation level of that region.}
#'
#'   \item{\code{pop}}{The population of that region.}
#'
#'   \item{\code{municipal_status}}{Additional identifiers to distinguish the
#'     municipal status of census subdivisions.}
#'
#'   \item{\code{CMA_UID}}{The identifier for the Census Metropolitan Area the region is in (if any).}
#'
#'   \item{\code{CD_UID}}{The identifier for the Census District the region is in (if any).}
#'
#'   \item{\code{PR_UID}}{The identifier for the Province the region is in (if unique).}
#' }
#'
#' @export
#'
#' @examples
#' list_census_regions('CA16')
list_census_regions <- function(dataset, use_cache = FALSE, quiet = FALSE) {
  cache_file <- cache_path(dataset, "_regions.rda")
  if (!use_cache || !file.exists(cache_file)) {
    if (!quiet) message("Querying CensusMapper API for regions data...")
    response <- httr::GET(paste0("https://censusmapper.ca/data_sets/", dataset,
                                 "/place_names.csv"))
    handle_cm_status_code(response, NULL)
    content <- httr::content(response, type = "text", encoding = "UTF-8")
    result <- if (!requireNamespace("readr", quietly = TRUE)) {
      dplyr::as_data_frame(utils::read.csv(textConnection(content), colClasses = 'character',stringsAsFactors = FALSE))
    } else {
      readr::read_csv(content,col_types = readr::cols(.default='c'))
    }
    result <- dplyr::select(result, region = .data$geo_uid, .data$name,
                            level = .data$type, pop = .data$population,
                            municipal_status = .data$flag, .data$CMA_UID,
                            .data$CD_UID, .data$PR_UID)  %>%
      dplyr::mutate(pop=as.integer(.data$pop))
    attr(result, "last_updated") <- Sys.time()
    save(result, file = cache_file)
    result
  } else {
    if (!quiet) message("Reading regions list from local cache.")
    load(file = cache_file)
    last_updated <- attr(result, "last_updated")
    if (!quiet && is.null(last_updated) ||
          difftime(Sys.time(), last_updated, units = "days") > 1) {
      warning(paste("Cached regions list may be out of date. Set `use_cache =",
                    "FALSE` to update it."))
    }
    result
  }
}

#' Query the CensusMapper API for regions with names matching a searchterm.
#'
#' @param searchterm The term to search for e.g. \code{"Victoria"}.
#' Search terms are case insensitive. If unable to find a given search term,
#' this function will suggest the correct spelling to use when possible.
#' @param dataset The dataset to query for available regions, e.g.
#'   \code{"CA16"}.
#' @param level One of \code{NA}, \code{'C'}, \code{'PR'}, \code{'CMA'}, \code{'CD'}, or \code{'CSD'}.
#' If specified, only return variables of specified `level`.
#' @param ... Further arguments passed on to \code{\link{list_census_regions}}.
#'
#' @export
#'
#' @examples
#' search_census_regions('Victoria', 'CA16')
#'
#' \dontrun{
#' # This will return a warning that no match was found, but will suggest similar named regions.
#' search_census_regions('Victorea', 'CA16')
#'
#' # This will limit region results to only include CMA level regions
#' search_census_regions('Victoria', 'CA16', level = "CMA")
#' }
search_census_regions <- function(searchterm, dataset, level=NA, ...) {
  reglist <- list_census_regions(dataset, ...)
  result <- reglist[grep(searchterm, reglist$name, ignore.case = TRUE),]

  # filter by type if needed
  if (!is.na(level) && length(rownames(result)) > 0) {
    result <- result[result$level==level,]
  }

  # Check if searchterm returned anything
  if (length(rownames(result)) > 0 ) {
    attr(result, "dataset") <- dataset
    return(result)
  }
  # If nothing matches, throw a warning and suggested alternatives.
  # If no suggested alternatives because the typo is too egregious, throw an error.
  else {
    # Check for similarly named terms. Uses base function agrep which is based on the Levenshtein edit distance for string similarity.
    # Default is set to 0.1 - can expand this to be more tolerant still.
    hintlist <- dplyr::as_tibble(unique(agrep(searchterm, reglist$name, ignore.case = TRUE, value = TRUE)))
    names(hintlist) <- "Similar named regions"
    #
    if (length(hintlist) > 0) {
      warning("No results found. Please use accurate spelling. See above for list of similarly named regions.")
      print(hintlist)
    } else {
      stop("No results found.")
    }
  }
}

#' Convert a (suitably filtered) data frame from
#' \code{\link{list_census_regions}} to a list suitable for passing to
#' \code{\link{get_census}}.
#'
#' @param tbl A data frame, suitably filtered, as returned by
#'   \code{\link{list_census_regions}}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(dplyr, warn.conflicts = FALSE)
#'
#' # Query the CensusMapper API for the total occupied dwellings
#' # of 20 random Census Subdivisions, in Census 2016.
#' regions <- list_census_regions("CA16") %>%
#'   filter(level == "CSD") %>%
#'   sample_n(20) %>%
#'   as_census_region_list()
#'
#' occupied <- get_census("CA16", regions = regions,
#'                             vectors = c("v_CA16_408"),
#'                             level = "Regions")
#' }
as_census_region_list <- function(tbl) {
  # This isn't bulletproof validation, but it should deter some misuse.
  if (!all(c("level", "region") %in% names(tbl))) {
    stop(paste("`as_region_list()` can only handle data frames",
               "returned by `list_regions()`."))
  }
  nested <- dplyr::group_by(tbl, .data$level) %>%
    # Use the dark magic of list columns...
    dplyr::summarise(regions = list(.data$region))

  regions <- nested$regions
  names(regions) <- nested$level
  regions
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
    dplyr::mutate_at(dplyr::intersect(names(g), as_character),
                     dplyr::funs(as.character)) %>%
    dplyr::mutate_at(dplyr::intersect(names(g), as_numeric),
                     dplyr::funs(as.numeric))  %>%
    dplyr::mutate_at(dplyr::intersect(names(g), as_integer),
                     dplyr::funs(as.integer))  %>%
    dplyr::mutate_at(dplyr::intersect(names(g), as_factor),
                     dplyr::funs(as.factor))

  #change names
  #standard table
  name_change <- dplyr::data_frame(
    old=c("id","a" ,"t" ,"dw","hh","pop","pop2","nrr","q","pop11","pop16","hh11","hh16","dw11","dw16"),
    new=c("GeoUID","Shape Area" ,"Type" ,"Dwellings","Households","Population","Adjusted Population (previous Census)","NHS Non-Return Rate","Quality Flags","Population 2011","Population 2016","Households 2011","Households 2016","Dwellings 2011","Dwellings 2016")
  )
  #geo uid name changes
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
NULL
