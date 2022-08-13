# recalled data functionality

recall_data_path <- function(){
  file.path(tempdir(),"recall.csv")
}


#' Get recalled data
#'
#' @description
#' Grabs recall data from server if needed and returns list of recalled data
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' check_recalled_data()
#' }
get_recalled_database <- function(){
  path <- recall_data_path()
  if (!file.exists(path)) {
      url <- paste0(cancensus_base_url(),"/api/v1/recall.csv")
      download.file(url,path,mode="wb",quiet=TRUE)
    }
    data <- readr::read_csv(path,col_types = readr::cols(.default="c"))
    data
}

#' Lists locally cached data that has been recalled
#'
#' @description
#' Checks the local cached database for recalled data and lists all recalled cached entries
#'
#' @export
#'
#' @examples
#' \dontrun{
#' list_recalled_cached_data()
#' }
list_recalled_cached_data <- function(){
  recall_database <- get_recalled_database()
  cached_data <- list_cancensus_cache()
  recall_database |>
    group_by(api_version,dataset,level) |>
    group_map(~filter(cached_data,
                      is.na(.y$level) | .y$level==.data$level,
                      .data$version==.y$api_version,
                      .data$dataset==.y$dataset,
                      grepl(paste0(.x$vector,collapse = "|"),.data$vectors))) |>
    bind_rows() |>
    ungroup()
}


#' Removes locally cached data that has been recalled
#'
#' @description
#' Checks the local cached database for recalled data and remvoves cached data that has been recalled
#'
#' @export
#'
#' @examples
#' \dontrun{
#' remove_recalled_chached_data()
#' }
remove_recalled_chached_data <- function(){
  recalled_data <- list_recalled_cached_data()
  if (nrow(recalled_data)>0) {
    size<-remove_from_cancensus_cache(recalled_data$path)
    message(paste0("Removing ",nrow(recalled_data)," datasets totalling ",size," bytes."))
  } else {
    message("No cached data has been recalled.")
  }
  d<-NULL
}




