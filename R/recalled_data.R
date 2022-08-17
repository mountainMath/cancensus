# recalled data functionality

recall_data_path <- function(){
  file.path(tempdir(),"recall.csv")
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
  if (refresh || !file.exists(path)) {
    url <- paste0(cancensus_base_url(),"/api/v1/recall.csv")
    tryCatch(
      utils::download.file(url,path,mode="wb",quiet=TRUE),
      error = function(e) {
        warning("Unable to download recall database at this point.")
        problem <- TRUE
      },
      warning = function(e) {
        warning("Unable to download recall database at this point.")
        problem <<- TRUE
      }
    )
  }
  data <- NULL
  if (!problem) {
    data <- readr::read_csv(path,col_types = readr::cols(.default="c"))
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
    #if (is.null(cached_data)) cached_data <- list_cancensus_cache()
    vector_recall <- recall_database %>%
      dplyr::filter(grepl("^d\\.",.data$api_version)) %>%
      dplyr::group_by(.data$api_version,.data$dataset,.data$level) %>%
      dplyr::group_map(~dplyr::filter(cached_data,
                                      is.na(.y$level) | .y$level==.data$level | .data$level=="Regions",
                                      .data$version<=.y$api_version,
                                      .data$dataset==.y$dataset,
                                      grepl(paste0(.x$vector,collapse = "|"),.data$vectors))) %>%
      dplyr::bind_rows() %>%
      dplyr::ungroup()

    geo_recall <- recall_database %>%
      dplyr::filter(grepl("^g\\.",.data$api_version)) %>%
      dplyr::group_by(.data$api_version,.data$dataset,.data$level) %>%
      dplyr::group_map(~dplyr::filter(cached_data,
                                      is.na(.y$level) | .y$level==.data$level | .data$level=="Regions",
                                      .data$version<=.y$api_version,
                                      .data$dataset==.y$dataset)) %>%
      dplyr::bind_rows() %>%
      dplyr::ungroup()

    data <- dplyr::bind_rows(vector_recall,geo_recall)
  }
  data
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
    message(paste0("Removing ",nrow(recalled_data)," datasets totalling ",size," bytes."))
  } else {
    message("No recalled data in cached data.")
  }
  size
}




