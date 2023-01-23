#' Read the geosuite data
#'
#' @description
#' Reads the original unprocessed geographic boundary files from Statistics Canada
#'
#' @param census_year census year to get the data for, right now only 2021 is supported
#' @param level geographic level to return the data for, valid choices are
#' "PR","CD","CMACA","CSD","CT","ADA","DA","ER","FED","DPL","POPCNTR"
#' @param type type of geographic data, valid choices area "cartographic" or "digital"
#' @param cache_path optional path to cache the data. If the cancensus cache path is set the geographic data gets
#' cached in the "geographies" subdirectory of the cancensus cache path.
#' @param timeout optional timeout parameter, adjust as needed if the data download times out when using slow connections
#' @param refresh (logical) refresh the cache if true
#' @param quiet (logical) suppress messages if `TRUE`
#' @return a spatial dataframe with the geographic data
#'
#' @examples
#' # get the digital geographic boundaries for provinces and territories
#' \dontrun{
#' get_statcan_geographies(census_year="2021",level="PR",type="digital")
#' }
#' @export
get_statcan_geographies <- function(census_year,level,type="cartographic",
                                    cache_path = NULL,timeout=1000,
                                    refresh=FALSE,quiet=FALSE) {
  valid_census_years <- c("2021")
  valid_levels <- c("PR","CD","CMACA","CMA","CA","CSD","CT","ADA","DA","ER","FED","DPL","POPCNTR")
  valid_types <- c("cartographic","digital")
  if (!(census_year %in% valid_census_years)) {
    stop(paste0("Census year must be one of ",paste0(valid_census_years,collapse = ", "),"."))
  }
  if (!(type %in% valid_types)) {
    stop(paste0("Type must be one of ",paste0(valid_types,collapse = ", "),"."))
  }
  if (!(level %in% valid_levels)) {
    stop(paste0("Level must be one of ",paste0(valid_levels,collapse = ", "),"."))
  }
  level_map <-  c("CMACA"="CMA","CA"="CMA","POPCNTR","PC")
  if (level %in% names(level_map)) level <-level_map[[level]]
  geo_base_path <- cache_path("geographies")
  if (!dir.exists(geo_base_path)) dir.create(geo_base_path)
  geo_base_path <- file.path(geo_base_path,type)
  if (!dir.exists(geo_base_path)) dir.create(geo_base_path)
  exdir <- file.path(geo_base_path,level)
  if (refresh || !dir.exists(exdir) || length(dir(exdir,"\\.shp$"))==0) {
    old_timeout <- getOption("timeout")
    if (type=="cartographic") typeID <- "b" else typeID <- "a"
    if (nchar(level)==2) filler="_000"
    else if (nchar(level)==3) filler="000"
    else {
      stop(paste0("Problem, don't know how to get geographic data for level ",level,"."))
    }
    url <- paste0("https://www12.statcan.gc.ca/census-recensement/",census_year,"/geo/sip-pis/boundary-limites/files-fichiers/l",tolower(level),filler,typeID,"21a_e.zip")
    tmp <- tempfile()
    options(timeout = timeout)
    utils::download.file(url,tmp,mode="wb",quiet=quiet)
    options(timeout = old_timeout)
    utils::unzip(tmp,exdir = exdir)
  } else {
    if (!quiet) message("Reading geographic data from local cache.")
  }
  path <- dir(exdir,"\\.shp$",full.names = TRUE)

  geos <- sf::read_sf(path)
  geos
}
