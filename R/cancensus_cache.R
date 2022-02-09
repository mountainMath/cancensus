touch_metadata <- function(meta_file){
  if (file.exists(meta_file)) {
    metadata <- readRDS(meta_file) %>%
      dplyr::mutate(last_accessed=Sys.time())
    if ("access_count" %in% names(metadata)) {
      metadata <- metadata %>% dplyr::mutate(access_count=.data$access_count+1)
    } else {
      metadata <- metadata %>% dplyr::mutate(access_count=1)
    }
  } else {
    pattern <- basename(meta_file) %>% gsub("\\.meta$","",.)
    path <- dir(cache_path(),pattern = pattern)
    metadata <- dplyr::tibble(path=path, last_accessed=Sys.time(),access_count=1)
  }
  saveRDS(metadata,file = meta_file)
}


#' List cached files
#'
#' @description
#' Lists all cached data and metadata if available
#'
#' @return tibble with metadata on cached data
#'
#' @examples
#' # list add the cached census data
#' list_cancensus_cache()
#'
#' @export
list_cancensus_cache <- function(){
  cp <- cache_path()
  metas <- dir(cp,"\\.meta$") %>%
    lapply(function(m)readRDS(file.path(cp,m)) %>% dplyr::mutate(path=gsub("\\.meta$","",m))) %>%
    dplyr::bind_rows()

  all <- dplyr::tibble(path=dir(cp,"^CM_")) %>%
    dplyr::filter(!grepl("\\.meta$",.data$path))

  if (nrow(metas)==0){
    all <- all %>% dplyr::mutate(size=NA_real_)
  } else {
    all <- all %>% dplyr::left_join(metas,by="path")
    if (!("size") %in% names(all)) all <- all %>% dplyr::mutate(size=NA_real_)
  }

  missing_sizes <- all %>%
    dplyr::filter(is.na(.data$size)) %>%
    dplyr::pull(.data$path)

  if (length(missing_sizes)>0) {
    manual_sizes <- missing_sizes %>%
      lapply(function(p) dplyr::tibble(path=p,manual_size=file.info(file.path(cp,p))$size)) %>%
      dplyr::bind_rows()

    all <- all %>%
      dplyr::left_join(manual_sizes,by="path") %>%
      dplyr::mutate(size=dplyr::coalesce(.data$size,.data$manual_size)) %>%
      dplyr::select(-.data$manual_size)
  }
  all
}


#' Remove cached files
#'
#' @description
#' Remove cached data for paths given
#'
#' @param paths list of paths to remove, as returned by the path column in `list_cancensus_cache`
#' @return freed-up disk space
#'
#'@examples
#'\dontrun{
#' # remove the first cached dataset
#' cache_data <- list_cancensus_cache()
#'
#' remove_from_cancensus_cache(cache_data$path[1])
#' }
#' @export
remove_from_cancensus_cache <- function(paths){
  cp <- cache_path()

  cache_data <- list_cancensus_cache()

  extras <- setdiff(paths,cache_data$path)
  if (length(extras)>0) warning(paste0("Could not find cached data for ",
                                       paste0(extras, collapse = ", ")))


  to_delete <- cache_data %>%
    dplyr::filter(.data$path %in% paths)
  freed_up_size <- to_delete$size %>% sum(na.rm=TRUE)
  if (nrow(to_delete)>0) {
    to_delete$path %>%
      lapply(function(p){
        unlink(file.path(cp,p))
        meta_path <- file.path(cp,paste0(p,".meta"))
        if (file.exists(meta_path)) unlink(meta_path)
        })
  }

  freed_up_size
}
