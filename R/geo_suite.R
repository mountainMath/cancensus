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
#' @return tibble with the geosuite data. If Statistics Canada blocks the download this gives a
#' warning explaining how to download the file manually, and returns the cached data when
#' refreshing existing cached data and `NULL` otherwise
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
  has_cache <- file.exists(file_path)
  if (refresh || !has_cache) {
    urls <- c("2016"="https://www12.statcan.gc.ca/census-recensement/2016/geo/ref/geosuite/files-fichiers/GeoSuite_2016_92-150_XBB_eng.zip",
              "2021"="https://www12.statcan.gc.ca/census-recensement/2021/geo/aip-pia/geosuite/files-fichiers/2021_92-150-X_eng.zip",
              "2011"="https://www12.statcan.gc.ca/census-recensement/2011/geo/ref/files-fichiers/2011_92-150_XBB_eng.zip",
              "2006"="https://www12.statcan.gc.ca/census-recensement/2011/geo/ref/files-fichiers/2006_92-150_XBB_eng.zip",
              "2001"="https://www12.statcan.gc.ca/census-recensement/2011/geo/ref/files-fichiers/92F0150WCB2001000.zip")
    tmp <- statcan_download_zip(urls[[census_year]],has_cache = has_cache)
    if (is.null(tmp) && !has_cache) return(NULL)
    if (!is.null(tmp)) {
      if (!dir.exists(path)) dir.create(path)
      if (Sys.info()[['sysname']]=="Darwin") {
        system(paste("ditto -V -x -k --sequesterRsrc --rsrc",shQuote(tmp),shQuote(path)))
      } else {
        utils::unzip(tmp,exdir = path)
      }
      unlink(tmp)
    }
  }
  gs <- NULL
  if (census_year=="2021") {
    gd <- readr::read_csv(file_path,
                          locale = readr::locale(encoding ="Windows-1252"),
                          col_types = readr::cols(.default="c")) %>%
      dplyr::mutate(dplyr::across(dplyr::matches("pop_|tdwell_|urdwell_|area"),as.numeric))
  } else { #if (census_year=="2021") {
    stop("GeoSuite data is only available for the 2021 census year at this time.")
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
#' @return tibble with the relationship data. If Statistics Canada blocks the download this gives a
#' warning explaining how to download the file manually, and returns the cached data when
#' refreshing existing cached data and `NULL` otherwise
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
  has_cache <- statcan_is_zip(file_path)
  if (refresh || !has_cache) {
    url <- paste0("https://www12.statcan.gc.ca/census-recensement/",census_year,"/geo/sip-pis/dguid-idugd/files-fichiers/",census_year,"_98260004.zip")
    tmp <- statcan_download_zip(url,has_cache = has_cache)
    if (is.null(tmp) && !has_cache) return(NULL)
    if (!is.null(tmp)) {
      file.copy(tmp,file_path,overwrite = TRUE)
      unlink(tmp)
    }
  }
  readr::read_csv(file_path,
                  locale = readr::locale(encoding ="Windows-1252"),
                  col_types = readr::cols(.default="c"))
}

#' Read the Geographic Attributes File
#'
#' @description
#' Reads the geographic attributes for the given census year, with one row for each area of the lowest
#' census geography. The table contains the information on how all the geographic levels are related
#' for each area, along with population and dwelling counts and representative points.
#'
#' The data comes from parquet files hosted at \url{https://data.mountainmath.ca/census/geo_attributes/},
#' as Statistics Canada now blocks programmatic downloads of these files. The files are derived from
#' Statistics Canada data under the Statistics Canada Open Licence. They get downloaded on first use and
#' cached if the cancensus cache path has been set, and cached copies only get updated with
#' `refresh = TRUE`. Reading the files requires the \pkg{duckdb} and \pkg{DBI} packages.
#'
#' A reference guide for the 2021 file is available
#' at https://www150.statcan.gc.ca/n1/en/catalogue/92-151-G2021001
#'
#' @section Differences between census years:
#' Column names follow the 2021 file, but the available columns differ between census years. The main
#' change is between 1996 and 2001:
#'
#' * 2001 to 2021 have one row per dissemination block, identified by `DBUID`. There is no 2001 attribute
#'   file, the 2001 data was derived from the 2001 GeoSuite and has no total dwelling counts or block areas.
#' * 1981 to 1996 have one row per enumeration area, identified by `EAUID`. These have different
#'   columns, for example `CTCODE` instead of `CTUID` and no dissemination areas.
#'
#' Identifiers are only unique within a census year, the same `DBUID` or `EAUID` in another year
#' does not generally refer to the same area. In 2001 and 2006 the fields for census metropolitan areas and
#' agglomerations, census tracts and urban areas were filled outside of those areas to match the files from
#' 2011 on, so not all of these values are in the source. The `notes_en` entry of the metadata has details
#' on this and on any problems with the source of each file.
#'
#' @param census_year census year to get the data for, one of 1981, 1986, 1991, 1996, 2001, 2006, 2011,
#' 2016 or 2021
#' @param refresh (logical) download the file again if true, and report if it has changed
#' @param language language of the column labels, one of `"en"` or `"fr"`. Defaults to the
#' `cancensus.language` option, and to `"en"` if that is not set. This does not change the data.
#' @return tibble with the attribute data, with a description of each column in its `"label"` attribute.
#' The `"metadata"` attribute of the tibble holds the metadata of the file, including the attribution,
#' licence, source and notes. If the file can't be downloaded this gives a warning, and returns the cached
#' data when refreshing existing cached data and `NULL` otherwise.
#'
#' @examples
#' # read the attributes for the 2021 census
#' \dontrun{
#' attributes <- get_statcan_geographic_attributes("2021")
#' # where the data comes from
#' attr(attributes, "metadata")$attribution_en
#' }
#' @md
#' @export
get_statcan_geographic_attributes <- function(census_year="2021",refresh=FALSE,
                                              language=getOption("cancensus.language", "en")){
  census_year <- as.character(census_year)[1]
  valid_years <- as.character(seq(1981,2021,5))
  if (!(census_year %in% valid_years)) {
    stop(paste0("Only census years ",paste0(valid_years,collapse = ", "),
                " are supported for the geographic attributes."),call. = FALSE)
  }
  language <- validate_language(language)
  # check before downloading the file
  rlang::check_installed(c("duckdb","DBI"),version = c("1.5.6",NA),
                         reason = "to read the geographic attributes.")

  url <- paste0("https://data.mountainmath.ca/census/geo_attributes/geo_attributes_",census_year,".parquet")
  file_path <- cache_path(paste0("geo_attributes_",census_year,".parquet"))
  has_cache <- is_parquet_file(file_path)
  downloaded <- FALSE
  if (refresh || !has_cache) {
    downloaded <- download_parquet_file(url,file_path,has_cache = has_cache)
    if (!downloaded && !has_cache) return(NULL)
  }
  result <- read_parquet_file(file_path,language = language)
  if (downloaded) {
    metadata <- attr(result,"metadata")
    message("Geographic attributes hosted by MountainMath, source: ",metadata$attribution_en,
            ". Licence: ",metadata$licence_en,".")
  }
  result
}

# Parquet files start and end with the same signature, so this also catches truncated downloads
is_parquet_file <- function(path){
  if (!file.exists(path) || dir.exists(path)) return(FALSE)
  size <- file.size(path)
  if (is.na(size) || size < 8) return(FALSE)
  con <- file(path,"rb")
  on.exit(close(con))
  signature <- charToRaw("PAR1")
  start <- readBin(con,what = "raw",n = 4)
  seek(con,where = size-4,origin = "start")
  identical(start,signature) && identical(readBin(con,what = "raw",n = 4),signature)
}

fetch_parquet_file <- function(url,path,quiet = FALSE){
  httr::GET(url,cm_request_config(quiet),httr::write_disk(path,overwrite = TRUE),httr::timeout(600))
}

# Download a hosted parquet file to `path`, which only gets replaced by a valid file. Returns whether
# that worked, anything else is a warning so that this fails gracefully when the file is unavailable.
download_parquet_file <- function(url,path,has_cache = FALSE,quiet = FALSE){
  tmp <- tempfile(fileext = ".parquet",tmpdir = dirname(path))
  on.exit(unlink(tmp))
  response <- tryCatch(fetch_parquet_file(url,tmp,quiet = quiet),error = function(e) e)
  problem <- if (inherits(response,"error")) {
    conditionMessage(response)
  } else if (httr::http_error(response)) {
    paste0("the server responded with HTTP status ",httr::status_code(response))
  } else if (!is_parquet_file(tmp)) {
    "the server did not return a complete parquet file"
  }
  if (!is.null(problem)) {
    warning("Could not download ",url,": ",problem,". ",statcan_fallback_message(has_cache),call. = FALSE)
    return(FALSE)
  }
  if (has_cache) {
    changed <- digest::digest(tmp,file = TRUE) != digest::digest(path,file = TRUE)
    message(if (changed) "The hosted file has changed, updating the cached copy." else
      "The hosted file has not changed.")
  }
  file.rename(tmp,path)
  warn_if_cache_not_persistent(quiet = quiet)
  TRUE
}

# Read a parquet file along with its metadata. This returns the same "label" and "metadata" attributes as
# `r2parquet::collect_parquet()`, which can replace this and the two functions above once it is on CRAN.
# The file gets read locally since remote access needs the httpfs extension, a separate download.
read_parquet_file <- function(path,language = "en"){
  # keeps duckdb from storing extensions and secrets in the user's home directory
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  on.exit(DBI::dbDisconnect(con,shutdown = TRUE))
  source <- DBI::dbQuoteString(con,normalizePath(path,winslash = "/"))
  data <- DBI::dbGetQuery(con,paste0("SELECT * FROM read_parquet(",source,")")) %>%
    dplyr::as_tibble()
  entries <- DBI::dbGetQuery(con,paste0("SELECT decode(key) AS k, decode(value) AS v FROM parquet_kv_metadata(",
                                        source,")"))
  metadata <- lapply(stats::setNames(entries$v,entries$k),parse_parquet_metadata_value)
  for (column in names(data)) {
    entry <- metadata$columns[[column]]
    label <- entry[[paste0("label_",language)]]
    if (is.null(label)) label <- entry[["label_en"]]
    if (!is.null(label)) attr(data[[column]],"label") <- label
  }
  attr(data,"metadata") <- metadata
  data
}

# JSON objects and arrays get parsed, the rest stays a string
parse_parquet_metadata_value <- function(value){
  if (!grepl("^\\s*[\\[{]",value)) return(value)
  tryCatch(jsonlite::fromJSON(value),error = function(e) value)
}





