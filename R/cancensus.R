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
#' @param level The census aggregation level to retrieve, defaults to \code{"Regions"}. One of \code{"Regions"}, \code{"PR"}, \code{"CMA"}, \code{"CA"}, \code{"CMACA"}, \code{"CD"}, \code{"CSD"}, \code{"CT"}, \code{"DA"}, \code{"EA"} (for 1996), or \code{"DB"} (for 2001-2016). \code{"CMA"}, \code{"CA"} and \code{"CMACA"} all refer to the same level, which contains both census metropolitan areas and census agglomerations.
#' @param vectors An R vector containing the CensusMapper variable names of the census variables to download. If no vectors are specified only geographic data will get downloaded. Naming the elements of the vector sets the names of the columns, e.g. \code{c(pop = "v_CA21_1")}. With \code{labels = "labelled"}, if a name is already used by another column, like \code{Population}, that column gets a trailing underscore, e.g. \code{Population_}.
#' @param geo_format By default is set to \code{NA} and appends no geographic information. To include geographic information with census data, specify one of either \code{"sf"} to return an \code{\link[sf]{sf}} object (requires the \code{sf} package) or \code{"sp"} to return a \code{\link[sp]{SpatialPolygonsDataFrame-class}} object (requires the \code{rgdal} package). If user requests geo-spatial data and neither package is available, a context menu will prompt to install the \code{sf} package.
#' @param resolution Resolution of the geographic data. By default simplified geometries will be download. For lower level geometries like DB or DA this will be very close to the high resolution data.
#' Simplification generally increases as the geographic aggregation level increases.
#' If high resolution geometries are required
#' then this option can be set to 'high'. By default this setting is set to \code{'simplified'}.
#' @param labels Defaults to the \code{cancensus.labels} option, and to "detailed" if that is not set. The default is planned to change to "labelled" in a future version, so to keep code working set this parameter, or set the option with e.g. \code{options(cancensus.labels = "detailed")}. A message points this out once per session if neither is set. "detailed" names Census variable columns by vector and label, e.g. \code{"v_CA21_1: Population, 2021"}. Truncated Census variable names can be selected by setting labels = "short". Setting labels = "labelled" also names the columns by vector only, and stores the label of each variable in the \code{"label"} attribute of its column, which is displayed e.g. by the RStudio data viewer. The other columns get short names and types that are the same across datasets and with or without \code{geo_format}, e.g. \code{Name}, \code{Area}, \code{Population} and \code{Population_Previous}, and also have a \code{"label"}, e.g. \code{"Population 2021"} for \code{Population}, with the year being that of the census the column refers to. Columns named by naming \code{vectors} get the label of their variable. Use \code{label_vectors(...)} to return variable label information in detail.
#' @param use_cache If set to TRUE (the default) data will be read from the local cache if available.
#' @param quiet When TRUE, suppress messages and warnings.
#' @param api_key An API key for the CensusMapper API. Defaults to \code{options()} and then the \code{CM_API_KEY} environment variable.
#' @param retry Integer If greater than zero, automatically retry failed API requests with exponential backoff for specified maximum number of times. Defaults to 0.
#' @param language Language of the Census variable labels, one of \code{"en"} or \code{"fr"}. Defaults to the \code{cancensus.language} option, and to \code{"en"} if that is not set. This applies to the column names when \code{labels = "detailed"}, to the \code{"label"} attribute when \code{labels = "labelled"}, and to \code{label_vectors(...)}. It does not apply to region names or the names of the other columns. Not all datasets have French labels, vectors without French labels fall back to English with a warning once per session and dataset. Changing the language does not download the data again.
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
#'
#' # Name columns by vector, with the labels in the "label" attribute of the columns:
#' census_data <- get_census(dataset='CA16', regions=list(CMA="59933"),
#'                           vectors=c("v_CA16_408","v_CA16_409","v_CA16_410"),
#'                           level='CSD', labels="labelled")
#' attr(census_data$v_CA16_408, "label")
#'
#' # Get the labels in French:
#' census_data <- get_census(dataset='CA16', regions=list(CMA="59933"),
#'                           vectors=c("v_CA16_408","v_CA16_409","v_CA16_410"),
#'                           level='CSD', language="fr")
#'}
get_census <- function (dataset, regions, level=NA, vectors=c(), geo_format = NA,
                        resolution = 'simplified',
                        labels = getOption("cancensus.labels", "detailed"),
                        use_cache=TRUE, quiet=FALSE, api_key=Sys.getenv("CM_API_KEY"),
                        retry=0, language = getOption("cancensus.language", "en")) {

  language <- validate_language(language)
  valid_labels <- c("detailed", "short", "labelled")
  if (!(is.character(labels) && length(labels) == 1 && labels %in% valid_labels)) {
    cc_abort("invalid_labels", choices = paste0("'", valid_labels, "'"), .language = language)
  }
  if (missing(labels) && is.null(getOption("cancensus.labels")) && !quiet) message_labels_default(language)

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
  if (is.na(geo_format) && !(resolution %in% c("simplified","high"))) cc_abort("invalid_resolution", .language = language)

  # Turn the region list into a valid JSON dictionary.
  if (is.character(regions)) {
    if (!quiet) cc_warn("regions_character_deprecated", .language = language)
  } else if (is.null(names(regions)) || !all(names(regions) %in% VALID_LEVELS)) {
    cc_abort("invalid_regions", .language = language)
  } else {
    regions <- jsonlite::toJSON(lapply(regions,as.character)) # cast to character in case regions are supplied as numeric/interger
  }

  # Check if the aggregation level is valid.
  if (!level %in% VALID_LEVELS) {
    cc_abort("invalid_level", choices = paste0("'", VALID_LEVELS, "'"), .language = language)
  }

  # Check that we can read the requested geo format.
  if (is.na(geo_format)) { # This is ok.
  } else if (!geo_format %in% c("sf", "sp")) {
    cc_abort("invalid_geo_format", .language = language)
  } else if (!is.na(geo_format) && !requireNamespace("sf", quietly = TRUE)) {
    cc_abort("sf_required", .language = language)
  }

  # sf availability is already guaranteed by the check above; the previous
  # installed.packages()-based install prompts were unreachable and scanned
  # the entire package library on every spatial call

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
        cc_abort("no_api_key", .language = language)
      }
      url <- paste0(base_url, "data.csv")
      if (!quiet) cc_inform("querying_data", .language = language)

      # Define the API call
      make_data_call <- function() {
        cm_POST(url, body=params, quiet=quiet)
      }

      # Execute with or without retry
      response <- if (retry>0) {
        retry_api_call(make_data_call, max_retries = retry+1, quiet = quiet, language = language)
      } else {
        make_data_call()
      }

      handle_cm_status_code(response, NULL, language = language)

      # Report download size
      content_length <- as.numeric(response$headers$`content-length`)
      if (!quiet && length(content_length)>0 && !is.na(content_length) && content_length > 0) {
        cc_inform("downloaded_data", size = format_bytes(content_length, language), .language = language)
      }

      data_version <- response$headers$`data-version`


      # Read the data file and transform to proper data types
      result <- if (requireNamespace("readr", quietly = TRUE)) {
        # Use readr::read_csv if it's available.
        httr::content(response, type = "text", encoding = "UTF-8") %>%
          I() %>%
          readr::read_csv(na = cancensus_na_strings,
                          col_types = list(.default = "c"))
      } else {
        httr::content(response, type = "text", encoding = "UTF-8") %>%
          textConnection %>%
          utils::read.csv(colClasses = "character", stringsAsFactors = FALSE, check.names = FALSE) %>%
          dplyr::as_tibble(.name_repair = "minimal")
      }

      result <- result %>%
        dplyr::mutate_at(c(dplyr::intersect(names(.),c("Population","Households","Dwellings","Area (sq km)")),
                           names(.)[grepl("^v_",names(.))]), as.num) %>%
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
      warn_if_cache_not_persistent(quiet = quiet, language = language)
    } else {
      if (!quiet) cc_inform("reading_data_cache", .language = language)
      check_recalled_data_and_warn(meta_file, params, language = language)
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
        cc_abort("no_api_key", .language = language)
      }
      url <- paste0(base_url, "geo.geojson")
      if (!quiet) cc_inform("querying_geometry", .language = language)

      # Define the API call
      make_geo_call <- function() {
        cm_POST(url, body=params, quiet=quiet)
      }

      # Execute with or without retry
      response <- if (retry>0) {
        retry_api_call(make_geo_call, max_retries = retry+1, quiet = quiet, language = language)
      } else {
        make_geo_call()
      }

      handle_cm_status_code(response, NULL, language = language)

      # Report download size
      content_length <- as.numeric(response$headers$`content-length`)
      if (!quiet && length(content_length)>0 && !is.na(content_length) && content_length > 0) {
        cc_inform("downloaded_geometry", size = format_bytes(content_length, language), .language = language)
      }

      geo_version <- response$headers$`data-version`
      write(httr::content(response, type = "text", encoding = "UTF-8"), file = geo_file) # cache result
      file_info <- file.info(geo_file)
      metadata <- dplyr::tibble(dataset=dataset,regions=as.character(regions),
                                level=level,created_at=Sys.time(),
                                resolution=resolution,
                         version=geo_version,size=file_info$size)
      saveRDS(metadata, file = meta_file)
      warn_if_cache_not_persistent(quiet = quiet, language = language)
    } else {
      if (!quiet) cc_inform("reading_geo_cache", .language = language)
      check_recalled_data_and_warn(meta_file, params, language = language)
    }
    geos <- geojsonsf::geojson_sf(geo_file) %>%
      sf::st_sf(agr="constant") %>% # need to set this, otherwise sf complains
       transform_geo(level) #%>%
      # sf::st_sf(agr="constant") # just in case

    if (!quiet) {
      cc_inform("processing_features", .n = nrow(geos), .language = language)
    }

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

  # ensure sf format even if library not loaded
  if (!is.na(geo_format) & geo_format=='sf') {
    result <- result %>%
      sf::st_sf(agr="constant")
  }


  # before vectors get renamed, a name chosen for a vector keeps the label of that vector
  if (labels == "labelled") result <- label_census_columns(result, dataset, language)

  if (length(vectors)>0) {
    vector_columns <- names(result)[grep("^v_", names(result))]
    census_vectors <- parse_vector_columns(vector_columns) %>%
      translate_census_vectors(dataset, language, quiet = quiet)
    if (labels != "detailed" | !is.null(names(vectors))) {
      to_rename <- setNames(names(result),gsub(":.*","",names(result)))
      to_rename <- to_rename[names(to_rename)!=as.character(to_rename)]
      if (length(to_rename)>0) result <- result %>% dplyr::rename(!!!to_rename)
      if (labels == "labelled") {
        for (i in seq_len(nrow(census_vectors))) {
          attr(result[[census_vectors$Vector[i]]], "label") <- census_vectors$Detail[i]
        }
      }
      if (!is.null(names(vectors))) {
        # vectors can be partly named, the unnamed ones keep their vector code as column name
        to_rename <- vectors[as.character(vectors) %in% names(result) & nzchar(names(vectors))]
        if (labels == "labelled") result <- add_underscore_to_clashes(result, names(to_rename), language)
        if (length(to_rename)>0) result <- result %>% dplyr::rename(!!! to_rename)
      }
    } else if (language != "en") {
      to_rename <- setNames(vector_columns, paste0(census_vectors$Vector, ": ", census_vectors$Detail))
      to_rename <- to_rename[names(to_rename)!=as.character(to_rename)]
      if (length(to_rename)>0) result <- result %>% dplyr::rename(!!!to_rename)
    }
  }

  if (!is.na(geo_format) & geo_format=='sp') { # ensure sf format even if library not loaded
    result <- sf::as_Spatial(result)
  }

  # set this last since renaming sf objects and converting to sp drop the attribute
  if (length(vectors)>0) attr(result, "census_vectors") <- census_vectors

  return(result)
}

# Prepare users for the planned change of the default, once per session. This is a message and not a
# warning since warnings turn into errors under options(warn = 2), which would break working code.
message_labels_default <- function(language = NULL) {
  cache_key <- "labels_default_message"
  if (isTRUE(session_cache_get(cache_key))) return(invisible(NULL))
  session_cache_set(cache_key, TRUE)
  cc_inform("labels_default", .language = language)
  invisible(NULL)
}

# Split column names of the form "<vector>: <label>" into vector and label. Only the first colon
# separates the two, labels can contain further colons, e.g. "Size of census family: 2 persons".
parse_vector_columns <- function(columns) {
  dplyr::tibble(Vector = sub(":.*$", "", columns),
                Detail = trimws(sub("^[^:]*:?", "", columns)))
}

# Replace the English labels the API attaches to the data with the labels for the requested language.
# Downloaded data is independent of language, so this does not affect the data cache. If the labels
# can't be retrieved the data is still usable with English labels, so this does not stop.
translate_census_vectors <- function(census_vectors, dataset, language, quiet = FALSE) {
  if (language == "en" || nrow(census_vectors) == 0) return(census_vectors)
  vector_info <- tryCatch(fetch_census_vectors(dataset, quiet = TRUE, language = language), error = function(e) {
    if (!quiet) cc_warn("french_labels_fetch_failed", error = conditionMessage(e), .language = language)
    NULL
  })
  if (is.null(vector_info)) return(census_vectors)
  localized <- vector_info %>%
    dplyr::filter(.data$vector %in% census_vectors$Vector) %>%
    localize_census_vectors(language, warn = !quiet)
  matches <- match(census_vectors$Vector, localized$vector)
  found <- !is.na(matches)
  census_vectors$Detail[found] <- localized$label[matches[found]]
  census_vectors
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
  cc_abort("function_unsupported")
  #return(get_census(dataset=dataset, regions=regions, level=level, geo_format=geo_format, ...))
}

# This is the set of valid census aggregation levels, also used in the named
# elements of the `regions` parameter.
VALID_LEVELS <- c("Regions","C","PR", "CMA", "CA", "CMACA", "CD", "CSD", "ADA","CT", "DA", 'EA', "DB")

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
#' \dontrun{
#' list_census_datasets()
#' }
list_census_datasets <- function(use_cache = TRUE, quiet = FALSE) {
  cache_file <- file.path(tempdir(),"cancensus_datasets.rda") #cache_path("datasets.rda")
  if (!use_cache || !file.exists(cache_file)) {
    if (!quiet) cc_inform("querying_datasets")
    response <- cm_GET(paste0(cancensus_base_url(),"/api/v1/list_datasets"),
                       httr::accept_json())
    handle_cm_status_code(response, NULL)
    result <- httr::content(response, type = "text", encoding = "UTF-8") %>%
      jsonlite::fromJSON() %>%
      dplyr::as_tibble(.name_repair = "minimal")
    attr(result, "last_updated") <- Sys.time()
    save(result, file = cache_file)
    result
  } else {
    if (!quiet) cc_inform("reading_datasets_cache")
    load(file = cache_file)
    last_updated <- attr(result, "last_updated")
    if (!quiet && (is.null(last_updated) ||
          difftime(Sys.time(), last_updated, units = "days") > 1)) {
      cc_warn("datasets_cache_outdated")
    }
    result
  }
}

#' Get attribution for datasets
#'
#' @param datasets Vector of dataset identifiers
#' @param language Language of the attribution, one of \code{"en"} or \code{"fr"}. Defaults to the
#'   \code{cancensus.language} option, and to \code{"en"} if that is not set.
#'
#' @return
#'
#' Returns a string to be used as attribution for the given datasets.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' # Attribution string for the 2006 and 2016 census datasets
#' dataset_attribution(c('CA06','CA16'))
#' }
dataset_attribution <- function(datasets, language = getOption("cancensus.language", "en")){
  language <- validate_language(language)
  datasets <-   lapply(datasets,translate_dataset) %>% unlist()
  dataset_list <- list_census_datasets(quiet=TRUE) %>%
    dplyr::filter(.data$dataset %in% datasets)
  attribution <- dataset_list$attribution
  if (language == "fr" && "attribution_fr" %in% names(dataset_list)) {
    has_translation <- !is_blank(dataset_list$attribution_fr)
    attribution[has_translation] <- dataset_list$attribution_fr[has_translation]
  }

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
    cc_warn("no_census_vectors")}
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
  cc_warn("census_vectors_deprecated")
  if("census_vectors" %in% names(attributes(x))) {
    attr(x, "census_vectors")
  } else {
    cc_warn("no_census_vectors")}
}



# Internal function to handle unfavourable HTTP responses
handle_cm_status_code <- function(response, path, language = NULL){
  if (httr::status_code(response)!=200) {
    message=httr::content(response,as="text")
    if (!is.null(path)) {
      file.remove(path)
    }
    if (httr::status_code(response)==401) {
      # Problem with API key
      cc_abort("download_failed_unauthorized", details = message, .language = language)
    } else if (httr::status_code(response)==500) {
      cc_abort("download_failed_server", details = message, .language = language)
    } else {
      cc_abort("download_failed", details = message, .language = language)
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
  } else if (level %in% c('CMA','CA','CMACA')) {
    name_change <- c('PR_UID'='rpid',
                     'C_UID'='rgid')
  } else if (level=='PR') {
    name_change <- c('C_UID'='rpid')
  } else if (level=='C') {
    name_change <- c()
  } else {
    name_change <- c()
    cc_warn("unknown_level", level = level)
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

# Column names and labels for the columns that are not census vectors, used for labels = "labelled".
# Columns are in the order they get returned in. Column names are the same across datasets and for data
# and geometry requests, the year goes in the label. {year} is the year of the census the data refers
# to and {previous_year} that of the census before it.
census_column_labels <- dplyr::tribble(
  ~column, ~short_name, ~en, ~fr,
  "GeoUID", "GeoUID", "Geographic identifier", "Identifiant g\u00e9ographique",
  "Type", "Type", "Geographic level", "Niveau g\u00e9ographique",
  "Region Name", "Name", "Region name", "Nom de la r\u00e9gion",
  "name", "Name", "Region name", "Nom de la r\u00e9gion",
  "Area (sq km)", "Area", "Area (sq km)", "Superficie (km\u00b2)",
  "Shape Area", "Area", "Area (sq km)", "Superficie (km\u00b2)",
  "Population", "Population", "Population {year}", "Population {year}",
  "Dwellings", "Dwellings", "Dwellings {year}", "Logements {year}",
  "Households", "Households", "Households {year}", "M\u00e9nages {year}",
  "Adjusted Population (previous Census)", "Population_Previous", "Population {previous_year}", "Population {previous_year}",
  "Population 2016", "Population_Previous", "Population 2016", "Population 2016",
  "Population 2011", "Population_Previous", "Population 2011", "Population 2011",
  "Dwellings 2016", "Dwellings_Previous", "Dwellings 2016", "Logements 2016",
  "Dwellings 2011", "Dwellings_Previous", "Dwellings 2011", "Logements 2011",
  "Households 2016", "Households_Previous", "Households 2016", "M\u00e9nages 2016",
  "Households 2011", "Households_Previous", "Households 2011", "M\u00e9nages 2011",
  "NHS Non-Return Rate", "NHS_Non_Response", "NHS non-response rate {year}", "Taux de non-r\u00e9ponse de l'ENM {year}",
  "Quality Flags", "Quality", "Data quality flags", "Indicateurs de qualit\u00e9 des donn\u00e9es",
  "key", "Key", "Geographic key", "Cl\u00e9 g\u00e9ographique",
  "C_UID", "C_UID", "Canada identifier", "Identifiant du Canada",
  "PR_UID", "PR_UID", "Province or territory identifier", "Identifiant de la province ou du territoire",
  "CD_UID", "CD_UID", "Census division identifier", "Identifiant de la division de recensement",
  "CMA_UID", "CMA_UID", "Census metropolitan area or census agglomeration identifier", "Identifiant de la r\u00e9gion m\u00e9tropolitaine de recensement ou de l'agglom\u00e9ration de recensement",
  "CSD_UID", "CSD_UID", "Census subdivision identifier", "Identifiant de la subdivision de recensement",
  "CT_UID", "CT_UID", "Census tract identifier", "Identifiant du secteur de recensement",
  "DA_UID", "DA_UID", "Dissemination area identifier", "Identifiant de l'aire de diffusion"
)

# The year of the census the base columns of a dataset refer to. Tax and cross-tabulation datasets
# report these columns for the census they use the geography of, e.g. TX2018 has the 2016 population.
# Returns NA if the year can't be determined.
census_year_for_dataset <- function(dataset) {
  geo_dataset <- dataset
  if (!grepl("^CA[0-9]+$", dataset)) {
    datasets <- tryCatch(list_census_datasets(quiet = TRUE), error = function(e) NULL)
    geo_dataset <- datasets$geo_dataset[match(dataset, datasets$dataset)]
    if (length(geo_dataset) == 0 || is.na(geo_dataset) || !grepl("^CA[0-9]+$", geo_dataset)) return(NA_integer_)
  }
  year <- as.integer(sub("^CA", "", geo_dataset))
  if (year < 100) year <- year + 2000L
  year
}

# Harmonize the columns that are not census vectors for labels = "labelled": give them the same names,
# types and order for data and geometry requests, and set their "label" attribute.
label_census_columns <- function(result, dataset, language) {
  # requests for both data and geometry have these twice, with the same values
  duplicates <- c("Region Name"="name", "Area (sq km)"="Shape Area")
  duplicates <- duplicates[names(duplicates) %in% names(result) & duplicates %in% names(result)]
  if (length(duplicates) > 0) result <- result %>% dplyr::select(-dplyr::all_of(as.character(duplicates)))

  columns <- census_column_labels[census_column_labels$column %in% names(result), ]
  rename <- !duplicated(columns$short_name)
  column_names <- ifelse(rename, columns$short_name, columns$column)
  to_rename <- setNames(columns$column, column_names)[rename & column_names != columns$column]
  if (length(to_rename) > 0) result <- result %>% dplyr::rename(!!!to_rename)

  for (column in intersect(column_names, "Name")) result[[column]] <- as.character(result[[column]])
  counts <- c("Population", "Dwellings", "Households")
  for (column in intersect(column_names, c(counts, paste0(counts, "_Previous")))) {
    result[[column]] <- as.numeric(result[[column]])
  }

  year <- census_year_for_dataset(dataset)
  column_labels <- columns[[language]]
  column_labels <- gsub("{year}", if (is.na(year)) "" else year, column_labels, fixed = TRUE)
  column_labels <- gsub("{previous_year}", if (is.na(year)) "" else year - 5L, column_labels, fixed = TRUE)
  column_labels <- trimws(column_labels)
  for (i in seq_along(column_names)) attr(result[[column_names[i]]], "label") <- column_labels[i]

  # geometries stay last, sf keeps them when selecting
  ordered <- setdiff(c(column_names, setdiff(names(result), column_names)), attr(result, "sf_column"))
  result %>% dplyr::select(dplyr::all_of(ordered))
}

# For labels = "labelled", names chosen for vectors take priority over the names of the other columns,
# these get a trailing underscore instead to keep names unique, e.g. Population_ if a vector gets named
# Population. Other label options leave the column names as they are, and such names remain an error.
add_underscore_to_clashes <- function(result, chosen_names, language = NULL) {
  clashes <- setdiff(intersect(chosen_names, names(result)), grep("^v_", names(result), value = TRUE))
  if (length(clashes) == 0) return(result)
  new_names <- paste0(clashes, "_")
  taken <- intersect(new_names, c(names(result), chosen_names))
  if (length(taken) > 0) {
    cc_abort("vector_names_clash", columns = paste0("`", taken, "`"), .language = language)
  }
  dplyr::rename(result, !!!setNames(clashes, new_names))
}

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


# Suppress warnings for missing bindings for '.' in R CMD check.
if (getRversion() >= "2.15.1") utils::globalVariables(c("."))

#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @importFrom stats setNames
NULL
