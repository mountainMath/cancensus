metadata_columns <- c("path" , "dataset", "regions", "level", "vectors", "created_at",
                      "version", "size" ,"last_accessed", "access_count",  "resolution")
metadata_columns_c <- c("path" , "dataset", "regions", "level", "vectors", "version", "resolution")
metadata_columns_d <- c("created_at","last_accessed")
metadata_columns_n <- c("size" , "access_count")


generate_metadata <- function(meta_file,params){
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
    metadata <- dplyr::tibble(last_accessed=Sys.time(),access_count=1)
  }
  if (!is.null(params) & !("dataset" %in% names(metadata)) & "dataset" %in% names(params)) {
    metadata$dataset=params$dataset
  }
  if (!is.null(params) & !("level" %in% names(metadata)) & "level" %in% names(params)) {
    metadata$level=params$level
  }
  if (!is.null(params) & !("regions" %in% names(metadata)) & "regions" %in% names(params)) {
    metadata$regions=as.character(params$regions)
  }
  if (!is.null(params) & !("resolution" %in% names(metadata)) & "resolution" %in% names(params)) {
    metadata$resolution=as.character(params$resolution)
  }
  if (!is.null(params) & !("vectors" %in% names(metadata)) & "vectors" %in% names(params)) {
    metadata$vectors=jsonlite::toJSON(as.character(params$vectors))  %>% as.character()
  }
  if (!("version" %in% names(metadata))) {
    path <- basename(meta_file)
    if (grepl("^CM_geo_",path)) metadata$version="g.1"
    if (grepl("^CM_data_",path)) metadata$version="d.1"
  }

  for (n in setdiff(metadata_columns_c,names(metadata))) {
    metadata[,n]=NA_character_
  }
  # for (n in setdiff(metadata_columns_d,names(metadata))) {
  #   metadata[,n]=NA
  # }
  for (n in setdiff(metadata_columns_n,names(metadata))) {
    metadata[,n]=NA_real_
  }

  metadata
}

touch_metadata <- function(meta_file,params=NULL){
  metadata <- generate_metadata(meta_file,params)
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

  for (n in setdiff(metadata_columns_c,names(all))) {
    all[,n]=NA_character_
  }
  # for (n in setdiff(metadata_columns_d,names(metadata))) {
  #   metadata[,n]=NA
  # }
  for (n in setdiff(metadata_columns_n,names(all))) {
    all[,n]=NA_real_
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
