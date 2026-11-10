# recalled data functionality

recall_data_path <- function(){
  file.path(tempdir(),"recall.csv")
}

# Match recalled vector IDs against the JSON-encoded `vectors` metadata field.
# IDs are anchored on the surrounding JSON quotes so that e.g. a recall of
# "v_CA21_1" does not also match cached data for "v_CA21_10".
recalled_vector_pattern <- function(vectors){
  paste0('"(',paste0(regex_escape(vectors),collapse="|"),')"')
}


#' Get metadata for recalled data
#'
#' @description
#' Grabs recall data from server if needed and returns list of recalled data
#'
#' @keywords internal
#'
#' @param refresh Will force refresh of recalled database
#' @param warn_only_once Will only warn on first run, `FALSE` by default
#' @return tibble with rows describing recalled data
#'
#' @examples
#' \dontrun{
#' get_recalled_database()
#' }
get_recalled_database <- function(refresh=FALSE, warn_only_once=FALSE){
  problem <- FALSE
  path <- recall_data_path()
  # A failed download is not attempted again in this session unless asked for with `refresh`, this
  # gets called on every read from the cache and would otherwise warn and wait on the network each time.
  failed_key <- paste0("recall_database_unavailable_",path)
  if (!refresh && !file.exists(path) && isTRUE(session_cache_get(failed_key))) return(NULL)
  if (refresh || !file.exists(path)) {
    url <- paste0(cancensus_base_url(),"/api/v1/recall.csv")
    # Download to a temporary file and only replace the cached copy on success,
    # so that an error response never gets stored as the recall database.
    tmp <- tempfile(fileext = ".csv")
    on.exit(unlink(tmp), add = TRUE)
    response <- tryCatch(cm_GET(url, httr::write_disk(tmp, overwrite = TRUE),
                                httr::timeout(getOption("timeout", 60))),
                         error = function(e) e)
    reason <- if (inherits(response, "error")) {
      conditionMessage(response)
    } else if (httr::status_code(response) != 200) {
      cc_msg("hosted_http_status", status = httr::status_code(response))
    } else if (!file.copy(tmp, path, overwrite = TRUE)) {
      cc_msg("recall_database_not_saved", path = path)
    }
    session_cache_set(failed_key, !is.null(reason))
    if (!is.null(reason)) {
      cc_warn("recall_database_unavailable", reason = reason)
      problem <- TRUE
    }
  }
  data <- NULL
  if (!problem) {
    # parsed once per session and version of the file
    data_key <- paste0("recall_database_",path,"_",file.size(path),"_",as.numeric(file.mtime(path)))
    data <- session_cache_get(data_key)
    if (is.null(data)) {
      data <- session_cache_set(data_key, readr::read_csv(path,col_types = readr::cols(.default="c")))
    }
  }
  if (length(names(data))==0) {
    if (!warn_only_once && !problem) {
      data <- get_recalled_database(refresh=TRUE,warn_only_once = TRUE)
    } else {
      data <- NULL
    }
  }
  data
}

#' List recalled data stored in local cache
#'
#' @description
#' Checks the local cached database for recalled data and lists all recalled cached entries
#'
#' @param cached_data List of locally cached data to check for recall, default is `list_cancensus_cache()` in which case
#' it will get checked against all locally cached data
#' @param warn_only_once Will only warn on first run during each session, `FALSE` by default
#' @return tibble with rows describing locally cached recalled data
#'
#' @export
#'
#' @examples
#' \dontrun{
#' list_recalled_cached_data()
#' }
list_recalled_cached_data <- function(cached_data=list_cancensus_cache(),warn_only_once=FALSE){
  data <- NULL
  recall_database <- get_recalled_database(warn_only_once=warn_only_once)
  if (!is.null(recall_database)) {
    data <- dplyr::bind_rows(
      recalled_cache_entries(recall_database,cached_data,"d",match_vectors = TRUE),
      recalled_cache_entries(recall_database,cached_data,"g",match_vectors = FALSE))
  }
  data
}

# Versions look like "d.3" or "g.10", these get compared as numbers and not as text so that
# "d.10" is newer than "d.3". Versions that can't be parsed are NA and don't match any recall.
recall_version <- function(version){
  numeric_version(sub("^[a-z]+\\.","",version),strict = FALSE)
}

# Cached entries affected by recalls of one type, "d" for data and "g" for geometry. Data recalls
# only affect cached data that contains one of the recalled vectors.
recalled_cache_entries <- function(recall_database,cached_data,type,match_vectors){
  pattern <- paste0("^",type,"\\.")
  recalls <- dplyr::filter(recall_database,grepl(pattern,.data$api_version))
  cached <- dplyr::filter(cached_data,grepl(pattern,.data$version))
  # group_map evaluates its function on an empty group when there are no groups,
  # which fails for the comparisons below
  if (nrow(recalls)==0) return(cached[0,])
  recalls %>%
    dplyr::group_by(.data$api_version,.data$dataset,.data$level) %>%
    dplyr::group_map(~dplyr::filter(cached,
                                    is.na(.y$level) | .y$level==.data$level | .data$level=="Regions",
                                    recall_version(.data$version)<=recall_version(.y$api_version),
                                    .data$dataset==.y$dataset,
                                    if (match_vectors) grepl(recalled_vector_pattern(.x$vector),.data$vectors) else TRUE)) %>%
    dplyr::bind_rows()
}



#' Remove recalled data from local cache
#'
#' @description
#' Checks the local cached database for recalled data and removes cached data that has been recalled
#'
#' @return Storage size of removed locally cached data that got freed up in number of bytes.
#' @export
#'
#' @examples
#' \dontrun{
#' remove_recalled_cached_data()
#' }
remove_recalled_cached_data <- function(){
  recalled_data <- list_recalled_cached_data()
  size <- 0
  if (is.null(recalled_data)) {
  } else if (nrow(recalled_data)>0) {
    size<-remove_from_cancensus_cache(recalled_data$path)
    cc_inform("removing_recalled", size = format_bytes(size), .n = nrow(recalled_data))
  } else {
    cc_inform("no_recalled_data")
  }
  size
}




