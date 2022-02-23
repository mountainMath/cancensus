


#' Read the geosuite data
#'
#' @description
#' Reads the geosuite data for the given level and census year. Data gets cached after first download.
#'
#' @param level geographic level to return the data for, valid choices are
#' "DB", "DA", "ADA", "CT", "CSD", "CMA", "CD", "PR"
#' @param census_year census year to get the data for, right now only 2021 is supported
#' @param refresh (logical) refresh the cache if true
#' @return tibble with the geosuite data
#'
#' @examples
#' # list add the cached census data
#' get_geo_suite("DA","2021")
#'
#' @export
get_geo_suite <- function(level,census_year="2021",refresh=FALSE){
  valid_years <- c("2021")
  valid_levels <- c("DB", "DA", "CT", "ADA", "CSD", "CMA", "CD", "PR")
  if (!(as.character(census_year) %in% valid_years)) {
    stop(paste0("Only census years ",paste0(valid_years,collapse = ", "),
                " are supported for GeoSuite"))
  }
  if (level=="CMA" || level=="CA") {level="CMA_CA"}
  if (!(level %in% c(valid_levels,"CMA_CA"))) {
    stop(paste0("Only levels ",paste0(valid_levels,collapse = ", ")," are supported for GeoSuite"))
  }
  path <- cache_path(paste0("geo_suite_",census_year))
  file_path <- file.path(path,paste0(level,".csv"))
  if (refresh || !dir.exists(path) || !file.exists(file_path)) {
    tmp <- tempfile()
    url <- paste0("https://www12.statcan.gc.ca/census-recensement/",census_year,"/geo/aip-pia/geosuite/files-fichiers/",census_year,"_92-150-X_eng.zip")
    utils::download.file(url,destfile=tmp,
                  mode = "wb")
    if (!dir.exists(path)) dir.create(path)
    if (Sys.info()['sysname']=="Darwin") {
      system(paste0("ditto -V -x -k --sequesterRsrc --rsrc ",tmp," ",path))
    } else {
      utils::unzip(tmp,exdir = path)
    }
  }
  gd <- readr::read_csv(file_path,
                        locale = readr::locale(encoding ="Windows-1252"),
                        col_types = readr::cols(.default="c")) %>%
    dplyr::mutate(dplyr::across(dplyr::matches("pop_|tdwell_|urdwell_|area"),as.numeric))

}




#' Read the Dissemination Geographies Relationship File
#'
#' @description
#' Reads the Dissemination Geographies Relationship File for the given census year. The table contains
#' the information on how all the geographic levels are related for each area. A reference guide is available
#' at https://www150.statcan.gc.ca/n1/en/catalogue/982600032021001
#'
#' @param census_year census year to get the data for, right now only 2021 is supported
#' @param refresh (logical) refresh the cache if true
#' @return tibble with the relationship data
#'
#' @examples
#' # list add the cached census data
#' get_geography_relationship("2021")
#'
#' @export
get_geography_relationship <- function(census_year="2021", refresh=FALSE){
  valid_years <- c("2021")
  if (!(as.character(census_year) %in% valid_years)) {
    stop(paste0("Only census years ",paste0(valid_years,collapse = ", "),
                " are supported for the geographic relationship file."))
  }
  file_path <- cache_path(paste0("geography_relationship_",census_year,".zip"))
  if (refresh || !file.exists(file_path)) {
    url <- paste0("https://www12.statcan.gc.ca/census-recensement/",census_year,"/geo/sip-pis/dguid-idugd/files-fichiers/",census_year,"_98260004.zip")
    utils::download.file(url,  file_path)
  }
  readr::read_csv(file_path,col_types = readr::cols(.default="c"))
}

