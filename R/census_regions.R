#' Query the CensusMapper API for available regions in a given dataset.
#'
#' @param dataset The dataset to query for available regions, e.g.
#'   \code{"CA16"}.
#' @param use_cache If set to TRUE (the default), data will be read from a local cache
#'   that is maintained for the duration of the R session, if
#'   available. If set to FALSE, query the API for the data, and
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
list_census_regions <- function(dataset, use_cache = TRUE, quiet = FALSE) {
  cache_file <- file.path(tempdir(),paste0(dataset, "_regions.rda"))
  #cache_file <- cache_path(dataset, "_regions.rda")
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
