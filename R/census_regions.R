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
#' \dontrun{
#' list_census_regions('CA16')
#' }
list_census_regions <- function(dataset, use_cache = TRUE, quiet = FALSE) {
  dataset <- translate_dataset(dataset)
  session_cached(paste0(dataset, "_regions"), use_cache, quiet = quiet,
                 outdated_id = "regions_cache_outdated", fetch = function() {
    if (!quiet) cc_inform("querying_regions")
    response <- cm_GET(paste0(cancensus_base_url(),"/data_sets/", dataset,
                              "/place_names.csv"))
    handle_cm_status_code(response, NULL)
    result <- cm_read_csv(response) %>%
      dplyr::select(region = "geo_uid", "name",
                    level = "type", pop = "population",
                    municipal_status = "flag", "CMA_UID",
                    "CD_UID", "PR_UID")  %>%
      dplyr::mutate(pop=as.integer(.data$pop))
    # census agglomerations come labelled as CMA
    result$level[result$level=="CMA"&result$municipal_status == "K"] <- "CA"
    result
  })
}

#' Query the CensusMapper API for regions with names matching a searchterm.
#'
#' @description Runs a query against the CensusMapper API to retrieve region data with
#' names matching specific queries. Users can optionally specify the target geography level
#' (e.g. \code{level = 'CMA'}, \code{level = 'CSD'}, etc.). Alternatively, calling
#' \code{explore_census_vectors()} will launch the interactive region selection tool on
#' the CensusMapper site in a new web page or tab.
#'
#' @param searchterm The term to search for e.g. \code{"Victoria"}.
#' Search terms are case insensitive. If unable to find a given search term,
#' this function will suggest the correct spelling to use when possible.
#' @param dataset The dataset to query for available regions, e.g.
#'   \code{"CA16"}.
#' @param level One of \code{NA}, \code{'C'}, \code{'PR'}, \code{'CMA'}, \code{'CD'}, or \code{'CSD'}.
#' If specified, only return variables of specified `level`.
#' @param ... Further arguments passed on to \code{\link{list_census_regions}}.
#' @return A census region list of the same format as `list_census_regions()` containing the matches.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # This will return a warning that no match was found, but will suggest similar named regions.
#' search_census_regions('Victorea', 'CA16')
#'
#' # This will limit region results to only include CMA level regions
#' search_census_regions('Victoria', 'CA16', level = "CMA")
#' }
search_census_regions <- function(searchterm, dataset, level=NA, ...) {
  search_census_list(list_census_regions(dataset, ...), "name", searchterm, dataset,
                     filter_column = "level", filter_value = level,
                     hint_title = "Similar named regions", warn_id = "no_results_similar_regions")
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
    cc_abort("region_list_invalid")
  }
  nested <- dplyr::group_by(tbl, .data$level) %>%
    # Use the dark magic of list columns...
    dplyr::summarise(regions = list(.data$region))

  regions <- nested$regions
  names(regions) <- nested$level
  regions
}

#' Convenience function for creating unique names from region list
#'
#' @description Names of municipalities are not always unique, especially at the CSD level. This function
#' takes as input a subset of a regions list as generated from `list_census_regions()` and de-duplicates names as
#' needed by adding the municipal status in parenthesis. If this does not de-duplicate the name then the
#' geographic identifier will be further added in parenthesis behind that.
#'
#' @param region_list a subset of a regions list as gotten from `list_census_regions()`
#' @return The same list of regions with an extra column `Name` with de-duplicated names.
#' @export
#'
#' @examples
#' \dontrun{
#' # This will return a warning that no match was found, but will suggest similar named regions.
#' library(dplyr)
#' list_census_regions("CA21") %>%
#'   filter(level=="CSD", CMA_UID=="59933") %>%
#'   add_unique_names_to_region_list()
#' }
add_unique_names_to_region_list <- function(region_list) {
  gs <- dplyr::group_vars(region_list)
  r<-region_list %>%
    dplyr::group_by(.data$name) %>%
    dplyr::mutate(count=dplyr::n()) %>%
    dplyr::mutate(Name=dplyr::case_when(.data$count==1 ~ name,
                                        TRUE ~ paste0(.data$name," (",.data$municipal_status,")"))) %>%
    dplyr::group_by(.data$Name) %>%
    dplyr::mutate(count=dplyr::n()) %>%
    dplyr::mutate(Name=dplyr::case_when(.data$count==1 ~ Name,
                                        TRUE ~ paste0(.data$Name," (",.data$region,")"))) %>%
    dplyr::select(-"count") %>%
    dplyr::ungroup()

  if (length(gs)>0) {
    r <- r %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(gs)))
  }
  r
}


#' Lookup a municipal geography type from code - BETA
#'
#' @description Retrieved Census divisions and subdivisions include a code indicating the
#' municipality type or municipal status. There are 12 CD and 53 CSD distinct status
#' codes based on official designations used by provinces, territories, and federal
#' authorities. These are often used to distinguish Census divisions and subdivisions
#' with similar or identical names. Using \code{muni_status(code)} provides
#' some additional context in understanding the specific type referenced by the code.
#' For additional information consult the latest data dictionaries for relevant geographic levels.
#' Information for CSDs is at
#' \url{https://www12.statcan.gc.ca/census-recensement/2016/ref/dict/geo012-eng.cfm} and
#' for CDs at \url{https://www12.statcan.gc.ca/census-recensement/2016/ref/dict/geo008-eng.cfm}.
#'
#' @param code code/municipality status.
#'
#'
#'
#' @examples
#' muni_status("C")
#' muni_status("RD")
#' muni_status("CT")
#' muni_status("CV")
#'
