#' Read the geosuite data
#'
#' @description
#' Reads the geosuite data for the given level and census year. Data gets cached after first download if the
#' cancensus cache path has been set. For older
#' years `get_statcan_geographic_attributes()` can fill in most of the information
#'
#' @param level geographic level to return the data for, valid choices are
#' "DB", "DA", "ADA", "CT", "CSD", "CMA", "CD", "PR", "FED", "DPL", "ER", "PN", "POPCTR"
#' @param census_year census year to get the data for, right now only 2021 is supported
#' @param refresh (logical) refresh the cache if true
#' @return tibble with the geosuite data
#'
#'
#' @examples
#' # list add the cached census data
#' \dontrun{
#' get_statcan_geo_suite("DA","2021")
#' }
#' @export
get_statcan_geo_suite <- function(level,census_year="2021",refresh=FALSE){
  valid_years <- c("2021") #seq(2001,2021,5) %>% as.character()
  valid_levels <- c("DB", "DA", "CT", "ADA", "CSD", "CMA", "CD", "PR","FED","DPL","ER","PN","POPCTR")
  if (!(as.character(census_year) %in% valid_years)) {
    stop(paste0("Only census years ",paste0(valid_years,collapse = ", "),
                " are supported for GeoSuite"))
  }
  if (level=="CMA" || level=="CA") {level="CMA_CA"}
  if (!(level %in% c(valid_levels,"CMA_CA"))) {
    stop(paste0("Only levels ",paste0(valid_levels,collapse = ", ")," are supported for GeoSuite"))
  }
  path <- cache_path(paste0("geo_suite_",census_year))
  file_path <- file.path(path,"2021_92-150-X_eng",paste0(level,".csv"))
  if (refresh || !dir.exists(path) || !file.exists(file_path)) {
    tmp <- tempfile()
    urls <- c("2016"="https://www12.statcan.gc.ca/census-recensement/2016/geo/ref/geosuite/files-fichiers/GeoSuite_2016_92-150_XBB_eng.zip",
              "2021"="https://www12.statcan.gc.ca/census-recensement/2021/geo/aip-pia/geosuite/files-fichiers/2021_92-150-X_eng.zip",
              "2011"="https://www12.statcan.gc.ca/census-recensement/2011/geo/ref/files-fichiers/2011_92-150_XBB_eng.zip",
              "2006"="https://www12.statcan.gc.ca/census-recensement/2011/geo/ref/files-fichiers/2006_92-150_XBB_eng.zip",
              "2001"="https://www12.statcan.gc.ca/census-recensement/2011/geo/ref/files-fichiers/92F0150WCB2001000.zip")
    url <- urls[[census_year]]
    old_timeout <- getOption("timeout")
    options(timeout=10000)
    utils::download.file(url,destfile=tmp, mode = "wb")
    options(timeout=old_timeout)
    if (!dir.exists(path)) dir.create(path)
    if (Sys.info()[['sysname']]=="Darwin") {
      system(paste0("ditto -V -x -k --sequesterRsrc --rsrc '",tmp,"' '",path,"'"))
    } else {
      utils::unzip(tmp,exdir = path)
    }
  }
  gs <- NULL
  if (census_year=="2021") {
    gd <- readr::read_csv(file_path,
                          locale = readr::locale(encoding ="Windows-1252"),
                          col_types = readr::cols(.default="c")) %>%
      dplyr::mutate(dplyr::across(dplyr::matches("pop_|tdwell_|urdwell_|area"),as.numeric))
  } else if (census_year=="2021") {
    # if (level=="DB") level="CB"
    # gd<-Hmisc::mdb.get(file.path(path,"data/geosuite2001.mdb"),
    #                   tables=level) %>%
    #   as_tibble() %>%
    #   dplyr::mutate(dplyr::across(dplyr::matches("\\.POP$|\\.PODWELL$"),as.numeric),
    #                 dplyr::across(dplyr::matches("\\.ID$|.+CODE$|.+NAME$|IRRFLAG$"),as.character))
  }

  gd
}




#' Read the Dissemination Geographies Relationship File
#'
#' @description
#' Reads the Dissemination Geographies Relationship File for the given census year. The table contains
#' the information on how all the geographic levels are related for each area. Data gets cached after first download if the
#' cancensus cache path has been set. A reference guide is available
#' at https://www150.statcan.gc.ca/n1/en/catalogue/982600032021001
#'
#' @param census_year census year to get the data for, right now only 2021 is supported, for older
#' years `get_statcan_geographic_attributes()` can fill in most of the information
#' @param refresh (logical) refresh the cache if true
#' @return tibble with the relationship data
#'
#' @keywords internal
#'
#' @examples
#' # list add the cached census data
#' \dontrun{
#' get_statcan_geography_relationships("2021")
#' }
#' @export
get_statcan_geography_relationships <- function(census_year="2021", refresh=FALSE){
  valid_years <- c("2021")
  if (!(as.character(census_year) %in% valid_years)) {
    stop(paste0("Only census years ",paste0(valid_years,collapse = ", "),
                " are supported for the geographic relationship file."))
  }
  file_path <- cache_path(paste0("geography_relationship_",census_year,".zip"))
  if (refresh || !file.exists(file_path)) {
    url <- paste0("https://www12.statcan.gc.ca/census-recensement/",census_year,"/geo/sip-pis/dguid-idugd/files-fichiers/",census_year,"_98260004.zip")
    utils::download.file(url,  file_path,method="wb")
  }
  readr::read_csv(file_path,
                  locale = readr::locale(encoding ="Windows-1252"),
                  col_types = readr::cols(.default="c"))
}

#' Read the Geographic Attributes File
#'
#' @description
#' Reads the Geographies Attributes File for the given census year. The table contains
#' the information on how all the geographic levels are related for each area, and population, dwelling and household counts.
#' Data gets cached after first download if the
#' cancensus cache path has been set. A reference guide is available
#' at https://www150.statcan.gc.ca/n1/en/catalogue/92-151-G2021001
#'
#' @param census_year census year to get the data for, right now only 2006, 2011, 2016, 2021 are supported
#' @param refresh (logical) refresh the cache if true
#' @return tibble with the relationship data
#'
#' @examples
#' # list add the cached census data
#' \dontrun{
#' get_statcan_geographic_attributes("2021")
#' }
#' @export
get_statcan_geographic_attributes <- function(census_year="2021",refresh=FALSE){
  census_year <- as.character(census_year)[1]
  valid_years <- seq(2006,2021,5) %>% as.character
  if (!(as.character(census_year) %in% valid_years)) {
    stop(paste0("Only census years ",paste0(valid_years,collapse = ", "),
                " are supported for the geographic relationship file."))
  }
  urls <- c("2021"="https://www12.statcan.gc.ca/census-recensement/2021/geo/aip-pia/attribute-attribs/files-fichiers/2021_92-151_X.zip",
            "2016"="https://www12.statcan.gc.ca/census-recensement/2016/geo/ref/gaf/files-fichiers/2016_92-151_XBB_txt.zip",
            "2011"="https://www12.statcan.gc.ca/census-recensement/2011/geo/ref/files-fichiers/2011_92-151_XBB_txt.zip",
            "2006"="https://www12.statcan.gc.ca/census-recensement/2011/geo/ref/files-fichiers/2006_92-151_XBB_txt.zip")

  base_path <- cache_path("attribute_files")
  if (!dir.exists(base_path)) dir.create(base_path)
  base_path_year <- file.path(base_path,census_year)
  if (refresh || !dir.exists(base_path_year)) {
    if (dir.exists(base_path_year)) unlink(base_path_year,recursive = TRUE)
    dir.create(base_path_year)
    tmp<-tempfile(fileext = ".zip")
    status <- utils::download.file(urls[[census_year]],tmp,method="wb")
    utils::unzip(tmp,exdir = base_path_year)
  }
  if (census_year=="2021") file <- dir(base_path_year,pattern="\\.csv",full.names = TRUE)
  else file <- dir(base_path_year,pattern="\\.txt",full.names = TRUE)
  if (census_year %in% c("2016","2021")) {
    result <- readr::read_csv(file,col_types = readr::cols(.default="c"),
                              locale = readr::locale(encoding ="Windows-1252"))
  } else {
  headers <- c("DBuid",paste0(c("DBpop","DBtdwell","DBurdwell","DBarea","DB_ir"),census_year),"DAuid",
               "DAlamx","DAlamy","DAlat","DAlong",
               "PRuid","PRname", "PRename", "PRfname", "PReabbr", "PRfabbr", "FEDuid",
               "FEDname", "ERuid", "ERname", "CDuid", "CDname", "CDtype", "CSDuid", "CSDname",
               "CSDtype","SACtype","SACcode","CCSuid","CCSname","DPLuid","DPLname","DPLtype",
               "CMAPuid","CMAuid","CMAname","CMAtype","CTuid","CTcode","CTname","POPCTRRAPuid",
               "POPCTRRAuid","POPCTRRAname","POPCTRRAtype","POPCTRRAclass")

  positions <- c(1,11, 19, 27, 35, 48, 49, 57, 74,91,100,111,113,168,198,228,238,248,253, 338,
                 342, 427,431, 471, 474,481,536,539,540,543,550,605,611,696,699,704,704,807,
                 808,818,822,829,835, 839, 939,940)
  widths <- c(10,8,8,8,13,1,8,17,17,9,11,2,55,30,30,10,10,5,85,4,84,4,40,3,7,44,3,1,3,7,55,
              6,85,3,5,3,100,1,10,4,7,6,4,100,1,1)

  result <- readr::read_fwf(file,col_types = readr::cols(.default="c"),
                  locale = readr::locale(encoding ="Windows-1252"),
                  col_positions = readr::fwf_widths(widths)) %>%
    setNames(headers)
  }
  result %>%
    dplyr::mutate(dplyr::across(dplyr::matches("DBpop\\d{4}|DBtdwell\\d{4}|DBurdwell\\d{4}|DBarea"),as.numeric))
}





