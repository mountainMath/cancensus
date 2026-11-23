# Release dates of the 2026 boundary files that weren't available with the first release, as announced
# on https://www12.statcan.gc.ca/census-recensement/2026/geo/sip-pis/boundary-limites/index2026-eng.cfm
statcan_geography_releases <- list(
  "2026" = c(CMA = "2027-02-10", CT = "2027-02-10", PC = "2027-02-10", DPL = "2027-02-10",
             FSA = "2027-09-28")
)

# The levels with boundary files for each census year, after mapping alternative names
statcan_geography_levels <- list(
  "2021" = c("PR","CD","CMA","CSD","CT","ADA","DA","DB","ER","FED","DPL","PC","FSA","HR"),
  "2026" = c("PR","CD","CMA","CSD","CT","ADA","DA","DB","ER","FED","DPL","PC","FSA","HR","CAR","CCS")
)

# URL of the zipped shapefile of a boundary file. Generalized files are a cartographic product, the
# type letter "c" takes the place of "b" for cartographic and "a" for digital files.
statcan_geography_url <- function(census_year, level, type = "cartographic", precision = "detailed"){
  if (level == "HR") {
    folder <- if (type == "cartographic") "carto" else "digi"
    type_id <- if (type == "cartographic") "b" else "a"
    return(paste0("https://www150.statcan.gc.ca/n1/en/pub/82-402-x/2024001/hrbf-flrs/",folder,
                  "/ArcGIS/HR_000",type_id,"23a_e.zip"))
  }
  type_id <- if (precision == "generalized") "c" else if (type == "cartographic") "b" else "a"
  # all valid levels have two or three letters once mapped
  filler <- if (nchar(level) == 2) "_000" else "000"
  paste0("https://www12.statcan.gc.ca/census-recensement/",census_year,
         "/geo/sip-pis/boundary-limites/files-fichiers/l",tolower(level),filler,type_id,
         substr(census_year,3,4),"a_e.zip")
}

#' Read the geosuite data
#'
#' @description
#' Reads the original unprocessed geographic boundary files from Statistics Canada
#'
#' @param census_year census year to get the data for, "2021" or "2026". Not all 2026 boundary files are
#' available yet, Statistics Canada plans to release those for "CMACA", "CT", "POPCNTR" and "DPL" on
#' February 10, 2027, and for "FSA" on September 28, 2027.
#' @param level geographic level to return the data for, valid choices are
#' "PR","CD","CMACA","CSD","CT","ADA","DA","DB","ER","FED","DPL","POPCNTR", "FSA", "HR",
#' and for 2026 also "CAR" (census agricultural regions) and "CCS" (census consolidated subdivisions)
#' @param type type of geographic data, valid choices area "cartographic" or "digital"
#' @param cache_path optional path to cache the data. If the cancensus cache path is set the geographic data gets
#' cached in the "geographies" subdirectory of the cancensus cache path.
#' @param timeout optional timeout parameter, adjust as needed if the data download times out when using slow connections
#' @param refresh (logical) refresh the cache if true
#' @param quiet (logical) suppress messages if `TRUE`
#' @param precision precision of cartographic boundary files, "detailed" (the default) or "generalized".
#' Generalized boundary files are new with the 2026 census and are only available for
#' `type = "cartographic"`. They are simplified for mapping at smaller scales while keeping the shape of
#' and the spatial relationships between the geographic areas.
#' @return a spatial dataframe with the geographic data. Statistics Canada blocks plain downloads of
#' these files with a browser verification. If the safeconduct package is installed and finds Google
#' Chrome or another Chromium-based browser, the download goes through that browser, set
#' `options(cancensus.use_browser = FALSE)` to turn this off. Otherwise, or if that fails, this gives
#' a warning explaining how to download the file manually, and returns the cached data
#' when refreshing existing cached data and `NULL` otherwise
#'
#' @examples
#' # get the digital geographic boundaries for provinces and territories
#' \dontrun{
#' get_statcan_geographies(census_year="2021",level="PR",type="digital")
#'
#' # generalized cartographic boundaries of 2026 census divisions
#' get_statcan_geographies(census_year="2026",level="CD",precision="generalized")
#' }
#' @export
get_statcan_geographies <- function(census_year,level,type="cartographic",
                                    cache_path = NULL,timeout=1000,
                                    refresh=FALSE,quiet=FALSE,precision="detailed") {
  census_year <- as.character(census_year)[1]
  valid_census_years <- names(statcan_geography_levels)
  level_map <-  c("CMACA"="CMA","CA"="CMA","POPCNTR"="PC","POPCTR"="PC")
  valid_types <- c("cartographic","digital")
  valid_precisions <- c("detailed","generalized")
  if (!(census_year %in% valid_census_years)) {
    cc_abort("invalid_census_year", choices = valid_census_years)
  }
  if (!(type %in% valid_types)) {
    cc_abort("invalid_type", choices = valid_types)
  }
  if (!(length(precision) == 1 && precision %in% valid_precisions)) {
    cc_abort("invalid_precision", choices = valid_precisions)
  }
  year_levels <- statcan_geography_levels[[census_year]]
  valid_levels <- c(year_levels, names(level_map)[level_map %in% year_levels])
  if (!(level %in% valid_levels)) {
    cc_abort("invalid_level_choice", choices = valid_levels)
  }
  requested_level <- level
  if (level %in% names(level_map)) level <-level_map[[level]]
  if (precision == "generalized") {
    if (as.integer(census_year) < 2026) cc_abort("generalized_not_available", year = census_year)
    if (type != "cartographic") cc_abort("generalized_only_cartographic")
    if (level == "HR") cc_abort("generalized_not_available_hr")
  }
  release <- statcan_geography_releases[[census_year]][level]
  if (length(release) == 1 && !is.na(release) && Sys.Date() < as.Date(release)) {
    cc_abort("geography_not_released", level = requested_level, year = census_year,
             date = format(as.Date(release), "%Y-%m-%d"))
  }

  geo_base_path <- if (is.null(cache_path)) {
    # local argument shadows the cache_path() helper, R resolves the call to the function
    cache_path("geographies")
  } else {
    file.path(cache_path,"geographies")
  }
  # 2021 data keeps the location it had before other census years were added
  if (census_year != "2021") geo_base_path <- file.path(geo_base_path,census_year)
  geo_base_path <- file.path(geo_base_path,if (precision == "generalized") "cartographic_generalized" else type)
  if (!dir.exists(geo_base_path)) dir.create(geo_base_path,recursive = TRUE)
  exdir <- file.path(geo_base_path,level)
  has_cache <- length(dir(exdir,"\\.shp$"))>0
  if (refresh || !has_cache) {
    url <- statcan_geography_url(census_year, level, type, precision)
    tmp <- statcan_download_zip(url,base_path = cache_path,timeout = timeout,quiet = quiet,has_cache = has_cache)
    if (is.null(tmp) && !has_cache) return(NULL)
    if (!is.null(tmp)) {
      utils::unzip(tmp,exdir = exdir)
      unlink(tmp)
      fs <- dir(exdir,full.names = TRUE)
      if (length(fs)==1 && dir.exists(fs)) {
        tmp_dir <- tempfile(pattern = "cancensus_geo_", tmpdir = geo_base_path)
        file.rename(exdir,tmp_dir)
        fs <- dir(tmp_dir,full.names = TRUE)
        file.rename(fs,exdir)
        unlink(tmp_dir,recursive = TRUE)
      }
    }
  } else {
    if (!quiet) cc_inform("reading_geographies_cache")
  }
  path <- dir(exdir,"\\.shp$",full.names = TRUE)

  geos <- sf::read_sf(path)
  geos
}
