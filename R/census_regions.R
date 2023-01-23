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
  cache_file <- file.path(tempdir(),paste0(dataset, "_regions.rda"))

  if (!use_cache || !file.exists(cache_file)) {
    if (!quiet) message("Querying CensusMapper API for regions data...")
    response <- httr::GET(paste0("https://censusmapper.ca/data_sets/", dataset,
                                 "/place_names.csv"))
    handle_cm_status_code(response, NULL)
    content <- httr::content(response, type = "text", encoding = "UTF-8")
    result <- if (!requireNamespace("readr", quietly = TRUE)) {
      dplyr::as_tibble(utils::read.csv(textConnection(content),
                                       colClasses = 'character',
                                       stringsAsFactors = FALSE),
                       .name_repair = "minimal")
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
    result$level[result$level=="CMA"&result$municipal_status == "K"] <- "CA"
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
    result$level[result$level=="CMA"&result$municipal_status == "K"] <- "CA"
    result
  }
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
    hintlist <- dplyr::as_tibble(unique(agrep(searchterm, reglist$name, ignore.case = TRUE, value = TRUE)),
                                 .name_repair = "minimal")
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
  gs <- dplyr::groups(region_list)
  r<-region_list %>%
    dplyr::group_by(.data$name) %>%
    dplyr::mutate(count=dplyr::n()) %>%
    dplyr::mutate(Name=dplyr::case_when(.data$count==1 ~ name,
                                        TRUE ~ paste0(.data$name," (",.data$municipal_status,")"))) |>
    dplyr::group_by(.data$Name) %>%
    dplyr::mutate(count=dplyr::n()) %>%
    dplyr::mutate(Name=dplyr::case_when(.data$count==1 ~ Name,
                                        TRUE ~ paste0(.data$Name," (",.data$region,")"))) |>
    dplyr::select(-.data$count) |>
    dplyr::ungroup()

  if (length(gs)>1) {
    r <- r |>
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
# muni_status = function(code) {
#   geography <- c("CD", "CD", "CD", "CD", "CD", "CD", "CD", "CD", "CD", "CD", "CD",
#                  "CD", "CSD", "CSD", "CSD", "CSD", "CSD", "CSD", "CSD", "CSD", "CSD",
#                  "CSD", "CSD", "CSD", "CSD", "CSD", "CSD", "CSD", "CSD", "CSD", "CSD",
#                  "CSD", "CSD", "CSD", "CSD", "CSD", "CSD", "CSD", "CSD", "CSD", "CSD",
#                  "CSD", "CSD", "CSD", "CSD", "CSD", "CSD", "CSD", "CSD", "CSD", "CSD",
#                  "CSD", "CSD", "CSD", "CSD", "CSD", "CSD", "CSD", "CSD", "CSD", "CSD",
#                  "CSD", "CSD", "CSD", "CSD")
#   status_code <- c("CDR", "CT", "CTY", "DIS", "DM", "MRC", "RD", "REG", "RM", "TÉ", "TER",
#             "UC", "C", "CC", "CG", "CN", "COM", "CT", "CU", "CV", "CY", "DM", "HAM",
#             "ID", "IGD", "IM", "IRI", "LGD", "LOT", "M", "MD", "MÉ", "MU", "NH", "NL",
#             "NO", "NV", "P", "PE", "RCR", "RDA", "RGM", "RM", "RV", "S-É", "SA", "SC",
#             "SÉ", "SET", "SG", "SM", "SNO", "SV", "T", "TC", "TI", "TK", "TL", "TP",
#             "TV", "V", "VC", "VK", "VL", "VN")
#   status <- c("Census division / Division de recensement", "County / Comté", "County",
#               "District", "District municipality", "Municipalité régionale de comté",
#               "Regional district", "Region", "Regional municipality", "Territoire équivalent",
#               "Territory / Territoire", "United counties", "City / Cité", "Chartered community",
#               "Community government", "Crown colony / Colonie de la couronne", "Community",
#               "Canton (municipalité de)", "Cantons unis (municipalité de)", "City / Ville",
#               "City", "District municipality", "Hamlet", "Improvement district",
#               "Indian government district", "Island municipality",
#               "Indian reserve / Réserve indienne", "Local government district",
#               "Township and royalty", "Municipality / Municipalité", "Municipal district",
#               "Municipalité", "Municipality", "Northern hamlet", "Nisga'a land",
#               "Unorganized / Non organisé", "Northern village",
#               "Parish / Paroisse (municipalité de)", "Paroisse (municipalité de)",
#               "Rural community / Communauté rurale", "Regional district electoral area",
#               "Regional municipality", "Rural municipality", "Resort village",
#               "Indian settlement / Établissement indien", "Special area",
#               "Subdivision of county municipality / Subdivision municipalité de comté",
#               "Settlement / Établissement", "Settlement",
#               "Self-government / Autonomie gouvernementale", "Specialized municipality",
#               "Subdivision of unorganized / Subdivision non organisée", "Summer village",
#               "Town", "Terres réservées aux Cris", "Terre inuite",
#               "Terres réservées aux Naskapis", "Teslin land", "Township", "Town / Ville",
#               "Ville", "Village cri", "Village naskapi", "Village", "Village nordique")
#   muni_types <- data.frame(geography = geography, status_code = status_code, status = status)
#   result <- muni_types[muni_types$status_code == code,]
#   return(result)
#   if(nrow(result)>0) result else {
#     stop(paste0('There is no Census division or subdivision or municipality status with code "',
#                 code,'"'),
#          call. = FALSE)
#   }
# }
