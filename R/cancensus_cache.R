metadata_columns_c <- c("path" , "dataset", "regions", "level", "vectors", "version", "resolution")
metadata_columns_n <- c("size" , "access_count")

# Add the metadata columns that are missing, older cache entries don't have all of them
fill_metadata_columns <- function(metadata){
  for (n in setdiff(metadata_columns_c,names(metadata))) {
    metadata[,n]=NA_character_
  }
  for (n in setdiff(metadata_columns_n,names(metadata))) {
    metadata[,n]=NA_real_
  }
  metadata
}

# Record what a cached file holds, for list_cancensus_cache() and the check for recalled data. The
# version of the data comes with the `response` of the API, further fields get passed by name.
write_cache_metadata <- function(meta_file,data_file,response,...){
  metadata <- dplyr::tibble(...,created_at=Sys.time(),
                            version=response$headers$`data-version`,
                            size=file.info(data_file)$size)
  saveRDS(metadata,file = meta_file)
}

# The metadata of a cached file with updated access information. Cache entries from before the
# metadata was introduced get it filled in from the `params` of the request where possible.
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
    metadata <- dplyr::tibble(last_accessed=Sys.time(),access_count=1)
  }
  for (n in setdiff(intersect(c("dataset","level","regions","resolution"),names(params)),names(metadata))) {
    metadata[[n]]=as.character(params[[n]])
  }
  if (!("vectors" %in% names(metadata)) && "vectors" %in% names(params)) {
    metadata$vectors=jsonlite::toJSON(as.character(params$vectors))  %>% as.character()
  }
  if (!("version" %in% names(metadata))) {
    path <- basename(meta_file)
    if (grepl("^CM_geo_",path)) metadata$version="g.1"
    if (grepl("^CM_data_",path)) metadata$version="d.1"
  }

  fill_metadata_columns(metadata)
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
    # file.info is vectorized, stat all files in one batch
    manual_sizes <- dplyr::tibble(path=missing_sizes,
                                  manual_size=file.info(file.path(cp,missing_sizes))$size)

    all <- all %>%
      dplyr::left_join(manual_sizes,by="path") %>%
      dplyr::mutate(size=dplyr::coalesce(.data$size,.data$manual_size)) %>%
      dplyr::select(-"manual_size")
  }

  fill_metadata_columns(all)
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
  if (length(extras)>0) cc_warn("cache_entries_not_found", paths = extras)


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
