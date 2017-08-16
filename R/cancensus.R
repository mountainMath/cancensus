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
        dplyr::mutate_at(dplyr::intersect(names(g), as_character),
                         dplyr::funs(as.character)) %>%
        dplyr::mutate_at(dplyr::intersect(names(g), as_numeric),
                         dplyr::funs(as.numeric))  %>%
        dplyr::mutate_at(dplyr::intersect(names(g), as_integer),
                         dplyr::funs(as.integer))  %>%
        dplyr::mutate_at(dplyr::intersect(names(g), as_factor),
                         dplyr::funs(as.factor))

      #change names
      #standar table
      name_change <- dplyr::data_frame(
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
        dplyr::select(result, -Population, -Dwellings, -Households, -Type) %>%
          dplyr::inner_join(geos, by = "GeoUID")
      } else {
        geos
      }
    } else { # geo_format == "sp"
      geos <- rgdal::readOGR(geo_file, "OGRGeoJSON")
      geos@data <- geos@data %>% transform_geo
      if (!is.null(result)) {
        geos@data <- dplyr::select(geos@data, -Population, -Dwellings,
                                   -Households, -Type)
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
     if (!is.na(geo_format) && geo_format=="sp") {names(result@data) <- gsub(":.*","",names(result@data))}
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

# Depreciated method for setting the API key. Still exported for now.
#' @export
cancensus.set_api_key <- function(api_key){
  warning(paste0("cancensus.set_api_key() is depreciated, and will be removed ",
                 "in future versions. Use options(cancensus.api_key = \"",
                 api_key, "\") or Sys.setenv(CM_API_KEY = \"", api_key,
                 "\") instead."))
  options(cancensus.api_key = api_key)
}

#' Query the CensusMapper API for available datasets.
#'
#' @param use_cache If set to TRUE (the default) data will be read from a local
#'   cache, if available. If set to FALSE, query the API for the data, and
#'   refresh the local cache with the result.
#'
#' @return
#'
#' A data frame with a column \code{dataset} containing the code for the
#' dataset, and a column \code{description} describing it.
#'
#' @export
#'
#' @importFrom dplyr %>%
cancensus.list_datasets <- function(use_cache = TRUE) {
  cache_dir <- system.file("cache/", package = "cancensus")
  cache_file <- paste0(cache_dir, "datasets.rda")
  if (!use_cache || !file.exists(cache_file)) {
    message("Querying CensusMapper API for available datasets...")
    response <- httr::GET("https://censusmapper.ca/api/v1/list_datasets",
                          httr::accept_json())
    cancensus.handle_status_code(response, NULL)
    result <- httr::content(response, type = "text", encoding = "UTF-8") %>%
      jsonlite::fromJSON() %>%
      dplyr::as_data_frame()
    save(result, file = cache_file)
    result
  } else {
    message("Reading dataset list from local cache.")
    load(file = cache_file)
    result
  }
}

#' Query the CensusMapper API for available vectors for a given dataset.
#'
#' @param dataset The dataset to query for available vectors, e.g.
#'   \code{"CA16"}.
#' @param use_cache If set to TRUE (the default) data will be read from the local cache if available.
#'
#' @export
#'
#' @importFrom dplyr %>%
cancensus.list_vectors <- function(dataset, use_cache=TRUE) {
  # TODO: Validate dataset?
  result <- NULL
  dir.create('data_cache', showWarnings = FALSE) # make sure cache directory exists
  vector_file=paste0("data_cache/vector_list_",dataset,".csv")
  if (!use_cache || !file.exists(vector_file)) {
    url <- paste0("https://censusmapper.ca/api/v1/vector_info/", dataset, ".csv")
    response <- httr::GET(url, httr::write_disk(vector_file, overwrite = TRUE),
                          httr::progress())
    cancensus.handle_status_code(response,vector_file)
  } else {
    message("Reading vector information from local cache.")
  }
  if  (!requireNamespace("readr", quietly = TRUE)) {
    result <- utils::read.csv(vector_file, encoding = "UTF-8",stringsAsFactors = FALSE)
    class(result) <- c("tbl_df", "tbl", "data.frame")
  } else {
    result <- readr::read_csv(vector_file)
  }
  dplyr::mutate(
      result, type = factor(type),
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
    dplyr::select(vector, type, label, units, parent_vector = parent,
                  aggregation)
}

#' Query the CensusMapper API for vectors with descriptions matching a searchterm.
#'
#' @param searchterm The term to search for e.g. \code{"Ojibway"}.
#' Search terms are case insensitive. If unable to find a given search term,
#' this function will suggest the correct spelling to use when possible.
#' @param dataset The dataset to query for available vectors, e.g.
#'   \code{"CA16"}.
#'
#' @export
#'
#' @examples
#' cancensus.set_vectors('Ojibway', 'CA16')
#'
#' # This will return a warning that no match was found, but will suggest similar terms.
#' cancensus.set_vectors('Ojibwe', 'CA16')
cancensus.search_vectors <- function(searchterm, dataset) {
  #to do: add caching of vector list here
  veclist <- cancensus.list_vectors(dataset)
  sublist <- veclist[grep(searchterm, veclist$label, ignore.case = TRUE),]
  result <- sublist
  # This part below is extremely inelegant and I look forward to someone adjusting it.
  # Depth was tested on language for CA16 and CA11, as it looks like language has the most deeply nested variables. 
  if (any(!is.na(sublist$parent_vector))) {
    parlist <- veclist[match(sublist$parent_vector, veclist$vector),]
    result$parent1 <- parlist$label
    if (any(!is.na(parlist$parent_vector))) {
      par2list <- veclist[match(parlist$parent_vector, veclist$vector),]
      result$parent2 <- par2list$label
      if (any(!is.na(par2list$parent_vector))) {
        par3list <- veclist[match(par2list$parent_vector, veclist$vector),]
        result$parent3 <- par3list$label
        if (any(!is.na(par3list$parent_vector))) {
          par4list <- veclist[match(par3list$parent_vector, veclist$vector),]
          result$parent4 <- par4list$label
          if (any(!is.na(par4list$parent_vector))) {
            par5list <- veclist[match(par4list$parent_vector, veclist$vector),]
            result$parent5 <- par5list$label
            if (any(!is.na(par5list$parent_vector))) {
              par6list <- veclist[match(par5list$parent_vector, veclist$vector),]
              result$parent6 <- par6list$label
              if (any(!is.na(par6list$parent_vector))) {
                par7list <- veclist[match(par6list$parent_vector, veclist$vector),]
                result$parent7 <- par7list$label
              }
            }
          }
        }
      }
    }
  }
  # Check if searchterm returned anything
  if (length(rownames(result)) > 0 ) return(result)
  # If nothing matches, throw a warning and suggested alternatives.
  # If no suggested alternatives because the typo is too egregious, throw an error.
  else {
    # Check for similarly named terms. Uses base function agrep which is based on the Levenshtein edit distance for string similarity.
    # Default is set to 0.1 - can expand this to be more tolerant still.
    hintlist <- as_tibble(unique(agrep(searchterm, veclist$label, ignore.case = TRUE, value = TRUE)))
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
#' @param use_cache If set to TRUE (the default) data will be read from a local
#'   cache, if available. If set to FALSE, query the API for the data, and
#'   refresh the local cache with the result.
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
#' }
#'
#' @export
#'
#' @importFrom dplyr %>%
cancensus.list_regions <- function(dataset, use_cache = TRUE) {
  # TODO: Validate dataset?
  cache_dir <- system.file("cache/", package = "cancensus")
  cache_file <- paste0(cache_dir, dataset, "_regions.rda")
  if (!use_cache || !file.exists(cache_file)) {
    message("Querying CensusMapper API for regions data...")
    response <- httr::GET(paste0("https://censusmapper.ca/data_sets/", dataset,
                                 "/place_names.csv"))
    cancensus.handle_status_code(response, NULL)
    content <- httr::content(response, type = "text", encoding = "UTF-8")
    result <- if (!requireNamespace("readr", quietly = TRUE)) {
      dplyr::as_data_frame(utils::read.csv(content, stringsAsFactors = FALSE))
    } else {
      readr::read_csv(content)
    }
    result <- dplyr::select(result, region = geo_uid, name, level = type,
                            pop = population, municipal_status = flag)
    save(result, file = cache_file)
    result
  } else {
    message("Reading regions list from local cache.")
    load(file = cache_file)
    result
  }
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

.onLoad <- function(libname, pkgname) {
  if (!"cancensus.api_key" %in% names(options())) {
    # Try to get the API key from the CM_API_KEY environment variable, if it has not already been specified.
    options(cancensus.api_key = if (nchar(Sys.getenv("CM_API_KEY")) > 1) { Sys.getenv("CM_API_KEY") } else { NULL })
  }
}

