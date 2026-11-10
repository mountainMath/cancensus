#' Read GeoSuite tables
#'
#' @description
#' Reads a table of the Statistics Canada GeoSuite for the given census year. The GeoSuite has a table for
#' each geographic level, with the codes and names of the areas, how they relate to other levels, population
#' and dwelling counts, land areas and representative points, along with code tables like the census
#' subdivision types. Called without `level` this lists the tables available for the census year.
#'
#' The data comes from parquet files hosted at \url{https://data.mountainmath.ca/census/geo_suite/},
#' as Statistics Canada now blocks programmatic downloads of the GeoSuite. The files are derived from
#' Statistics Canada data under the Statistics Canada Open Licence. They get downloaded on first use and
#' cached if the cancensus cache path has been set, and cached copies only get updated with
#' `refresh = TRUE`. Reading the tables requires the \pkg{duckdb} and \pkg{DBI} packages.
#'
#' @section Tables and columns:
#' The GeoSuite is available for the censuses from 1996 to 2021. The tables differ between census years,
#' for example 1996 has enumeration areas (`EA`) instead of dissemination areas and blocks, 2001 to 2006 have
#' urban areas (`UA`) and 2011 on population centres (`POPCTR`). Tables ending in `_P` hold the parts of
#' areas that span provinces or other areas. `PN` is the place name table, called `LOC` in the source.
#'
#' Column names follow the attribute files from `get_statcan_geographic_attributes()`: the level, the
#' measure and the census year, e.g. `CSDUID`, `CSDPOP2021` or `DARPLAT`. The same columns have the same
#' names across census years, and the tables can be joined to the attribute files without renaming. A suffix
#' `A` marks counts of the previous census adjusted to the current boundaries, and `C` or `AC` corrected
#' counts from the count review. Columns referring to other levels hold the code within the parent
#' level as in the source, e.g. `CSDCODE` instead of `CSDUID`. Each column has a description in its
#' `"label"` attribute, and the Statistics Canada field name in the `source_field` entry of the `columns`
#' metadata.
#'
#' @param level the table to read, e.g. `"DA"`, `"CSD"` or `"CMA_CA"`. `"CMA"`, `"CA"` and `"CMACA"` read
#' `"CMA_CA"`, `"LOC"` reads `"PN"` and `"CB"` reads `"DB"`. If `NULL` this returns the list of tables
#' for the census year.
#' @param census_year census year to get the data for, one of 1996, 2001, 2006, 2011, 2016 or 2021. For
#' older censuses `get_statcan_geographic_attributes()` has much of the same information.
#' @param refresh (logical) download the table again if true, and report if it has changed
#' @param language language of the column labels and table titles, one of `"en"` or `"fr"`. Defaults to
#' the `cancensus.language` option, and to `"en"` if that is not set. This does not change the data.
#' @param quiet (logical) suppress messages and the progress bar if `TRUE`. Warnings about failed
#' downloads are still shown.
#' @return tibble with the GeoSuite table, with a description of each column in its `"label"` attribute.
#' The `"metadata"` attribute of the tibble holds the metadata of the table, including the attribution,
#' licence, source and notes. If `level` is `NULL`, a tibble with the `table` names, their `title` and
#' `primary_key` and the `size` of the file in bytes. If the data can't be downloaded this gives a warning,
#' and returns the cached data when refreshing existing cached data and `NULL` otherwise.
#'
#' @examples
#' \dontrun{
#' # list the tables of the 2021 GeoSuite
#' get_statcan_geo_suite(census_year = "2021")
#' # read the dissemination areas
#' get_statcan_geo_suite("DA", "2021")
#' }
#' @md
#' @export
get_statcan_geo_suite <- function(level=NULL,census_year="2021",refresh=FALSE,
                                  language=getOption("cancensus.language", "en"),
                                  quiet=FALSE){
  census_year <- as.character(census_year)[1]
  valid_years <- names(geo_suite_tables)
  if (!(census_year %in% valid_years)) {
    stop(paste0("The GeoSuite is only available for census years ",paste0(valid_years,collapse = ", "),
                ". For older censuses get_statcan_geographic_attributes() has much of the same information."),
         call. = FALSE)
  }
  language <- validate_language(language)
  if (!is.null(level)) {
    level <- geo_suite_table_name(level,census_year)
    rlang::check_installed(c("duckdb","DBI"),version = c("1.5.6",NA),
                           reason = "to read the GeoSuite.")
  }
  if (!quiet) message_unused_geo_suite_files()

  base_url <- paste0("https://data.mountainmath.ca/census/geo_suite/",census_year,"/")
  if (is.null(level)) {
    return(geo_suite_table_list(paste0(base_url,"_manifest.json"),
                                cache_path("geo_suite/",census_year,"/_manifest.json"),
                                refresh = refresh,language = language,quiet = quiet))
  }
  read_hosted_parquet(paste0(base_url,level,".parquet"),
                      cache_path("geo_suite/",census_year,"/",level,".parquet"),
                      description = paste0(census_year," GeoSuite table ",level),
                      refresh = refresh,language = language,quiet = quiet)
}

# Tables of the hosted GeoSuite files, so that names can be checked without a download
geo_suite_tables <- local({
  common <- c("CAN","PR","CD","CDTYPE","CCS","CSD","CSDTYPE","SACTYPE","ER","FED","CT","DA","DB",
              "DPL","DPLTYPE","PN","LOCATOR_SOURCE_TYPE","DISSOLVED_GA_TYPE",
              "CMA_CA","CMA_CA_P","CMA_CA_NCMA_CA","CMA_CA_NCMA_CA_P")
  ua <- c("UA","UA_P","UA_RA","UA_RA_P","UARATYPE")
  popctr <- c("POPCTR","POPCTR_P","POPCTR_RA","POPCTR_RA_P","POPCTRRATYPE")
  list(
    "1996" = c("CAN","PR","CD","CCS","CSD","CSDTYPE","ER","FED","CT","EA","DPL","DPLTYPE","PN",
               "CMA_CA","CMA_CA_P","CMATYPE","PCMA_PCA","PCMA_PCA_P","UA","UA_P","UATYPE"),
    "2001" = c(common,"DPL_P",ua),
    "2006" = c(common,"REFMAPS",ua),
    "2011" = c(common,"REFMAPS","CMATYPE",popctr),
    "2016" = c(common,"ADA","CMATYPE","CSD_POPCHALL_ADJ","DPL_POPCHALL_ADJ",popctr),
    "2021" = c(common,"ADA","CMATYPE",popctr,"POPCTRRACLASSE","SACCODE")
  )
})

geo_suite_level_aliases <- c(CMA = "CMA_CA",CA = "CMA_CA",CMACA = "CMA_CA",LOC = "PN",CB = "DB")

# The hosted table name for `level`, with an error listing the tables if there is no such table
geo_suite_table_name <- function(level,census_year){
  if (!is.character(level) || length(level)!=1 || is.na(level)) {
    stop("level must be a single table name, e.g. \"DA\".",call. = FALSE)
  }
  table <- toupper(level)
  if (table %in% names(geo_suite_level_aliases)) table <- geo_suite_level_aliases[[table]]
  tables <- geo_suite_tables[[census_year]]
  if (!(table %in% tables)) {
    hint <- if (census_year=="1996" && table %in% c("DA","DB","ADA")) {
      " The 1996 GeoSuite is based on enumeration areas, in the EA table."
    } else ""
    stop(paste0("There is no ",level," table in the ",census_year," GeoSuite.",hint," Available tables are ",
                paste0(sort(tables),collapse = ", "),", get_statcan_geo_suite(census_year = \"",census_year,
                "\") lists them with descriptions."),call. = FALSE)
  }
  table
}

# The tables listed in the manifest of a GeoSuite year
geo_suite_table_list <- function(url,file_path,refresh,language,quiet){
  has_cache <- is_manifest_file(file_path)
  if (refresh || !has_cache) {
    downloaded <- download_hosted_file(url,file_path,has_cache = has_cache,quiet = quiet,
                                       is_valid = is_manifest_file,kind = "list of tables")
    if (!downloaded && !has_cache) return(NULL)
  }
  files <- jsonlite::read_json(file_path)$files
  files <- files[grepl("\\.parquet$",vapply(files,function(f) f$path,character(1)))]
  field <- function(metadata,name){
    value <- metadata[[name]]
    if (is.character(value) && length(value)==1) value <- parse_parquet_metadata_value(value)
    paste0(unlist(value),collapse = ", ")
  }
  dplyr::tibble(
    table = sub("\\.parquet$","",vapply(files,function(f) f$path,character(1))),
    title = vapply(files,function(f) {
      title <- field(f$metadata,paste0("title_",language))
      if (title=="") field(f$metadata,"title_en") else title
    },character(1)),
    primary_key = vapply(files,function(f) field(f$metadata,"primary_key"),character(1)),
    size = vapply(files,function(f) as.numeric(f$size),numeric(1))
  )
}

is_manifest_file <- function(path){
  file.exists(path) && !dir.exists(path) &&
    tryCatch(is.list(jsonlite::read_json(path)$files),error = function(e) FALSE)
}

# GeoSuite downloads from before the move to hosted parquet files, see message_unused_files()
message_unused_geo_suite_files <- function(){
  zips <- c("2021_92-150-X_eng.zip","GeoSuite_2016_92-150_XBB_eng.zip","2011_92-150_XBB_eng.zip",
            "2006_92-150_XBB_eng.zip","92F0150WCB2001000.zip")
  message_unused_files("unused_geo_suite_files",
                       c(cache_path(paste0("geo_suite_",seq(2001,2021,5))),
                         file.path(statcan_downloads_path(),c(zips,sub("\\.zip$","",zips)))),
                       "GeoSuite files","get_statcan_geo_suite()")
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
#' @param quiet (logical) suppress messages and the progress bar if `TRUE`. Warnings about failed
#' downloads are still shown.
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
                                              language=getOption("cancensus.language", "en"),
                                              quiet=FALSE){
  census_year <- as.character(census_year)[1]
  valid_years <- as.character(seq(1981,2021,5))
  if (!(census_year %in% valid_years)) {
    stop(paste0("Only census years ",paste0(valid_years,collapse = ", "),
                " are supported for the geographic attributes."),call. = FALSE)
  }
  language <- validate_language(language)
    rlang::check_installed(c("duckdb","DBI"),version = c("1.5.6",NA),
                         reason = "to read the geographic attributes.")
  if (!quiet) message_unused_attribute_files()

  read_hosted_parquet(paste0("https://data.mountainmath.ca/census/geo_attributes/geo_attributes_",
                             census_year,".parquet"),
                      cache_path(paste0("geo_attributes_",census_year,".parquet")),
                      description = "Geographic attributes",
                      refresh = refresh,language = language,quiet = quiet)
}

# Read a hosted parquet file through the cache at `file_path`, downloading it if needed or asked for
# with `refresh`. Says where the data comes from after downloading, as described by `description`.
read_hosted_parquet <- function(url,file_path,description,refresh = FALSE,language = "en",quiet = FALSE){
  has_cache <- is_parquet_file(file_path)
  downloaded <- FALSE
  if (refresh || !has_cache) {
    downloaded <- download_hosted_file(url,file_path,has_cache = has_cache,quiet = quiet)
    if (!downloaded && !has_cache) return(NULL)
  }
  result <- read_parquet_file(file_path,language = language)
  if (downloaded && !quiet) {
    metadata <- attr(result,"metadata")
    message(description," hosted by MountainMath, source: ",metadata$attribution_en,
            ". Licence: ",metadata$licence_en,".")
  }
  result
}

# Attribute files from before the move to hosted parquet files, see message_unused_files()
message_unused_attribute_files <- function(){
  zips <- c("2021_92-151_X.zip","2016_92-151_XBB_txt.zip","2011_92-151_XBB_txt.zip","2006_92-151_XBB_txt.zip")
  message_unused_files("unused_attribute_files",
                       c(cache_path("attribute_files"),
                         file.path(statcan_downloads_path(),c(zips,sub("\\.zip$","",zips)))),
                       "attribute files","get_statcan_geographic_attributes()")
}

# Point out files from before the move to hosted parquet files that `function_name` no longer reads,
# with a command to remove them, once per session, `key` and cache path. Deleting them is left to the
# user, as Statistics Canada may not allow downloading them again. This can go after a release or two.
message_unused_files <- function(key,paths,what,function_name){
  key <- paste0(key,"_",cache_path())
  if (!is.null(session_cache_get(key))) return(invisible(NULL))
  session_cache_set(key,TRUE)

  paths <- normalizePath(paths[file.exists(paths)],winslash = "/")
  if (length(paths)==0) return(invisible(NULL))

  files <- c(paths[!dir.exists(paths)],
             list.files(paths[dir.exists(paths)],recursive = TRUE,all.files = TRUE,full.names = TRUE))
  size <- format(structure(sum(file.size(files)),class = "object_size"),units = "auto",standard = "SI")
  quoted <- paste0(encodeString(paths,quote = '"'),collapse = ", ")
  if (length(paths)>1) quoted <- paste0("c(",quoted,")")
  message("Previously downloaded Statistics Canada ",what," are no longer used by ",
          function_name," and take up ",size,". These are the original files, which ",
          "may not be possible to download again. To delete them, run:\n  unlink(",quoted,", recursive = TRUE)")
  invisible(NULL)
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

fetch_hosted_file <- function(url,path,quiet = FALSE){
  httr::GET(url,cm_request_config(quiet),httr::write_disk(path,overwrite = TRUE),httr::timeout(600))
}

# Download a hosted file to `path`, which only gets replaced by a file that passes `is_valid`. Returns
# whether that worked, anything else is a warning so that this fails gracefully when the file is unavailable.
download_hosted_file <- function(url,path,has_cache = FALSE,quiet = FALSE,
                                 is_valid = is_parquet_file,kind = "parquet file"){
  dir.create(dirname(path),recursive = TRUE,showWarnings = FALSE)
  tmp <- tempfile(fileext = paste0(".",tools::file_ext(path)),tmpdir = dirname(path))
  on.exit(unlink(tmp))
  response <- tryCatch(fetch_hosted_file(url,tmp,quiet = quiet),error = function(e) e)
  problem <- if (inherits(response,"error")) {
    conditionMessage(response)
  } else if (httr::http_error(response)) {
    paste0("the server responded with HTTP status ",httr::status_code(response))
  } else if (!is_valid(tmp)) {
    paste0("the server did not return a complete ",kind)
  }
  if (!is.null(problem)) {
    warning("Could not download ",url,": ",problem,". ",statcan_fallback_message(has_cache),call. = FALSE)
    return(FALSE)
  }
  if (has_cache && !quiet) {
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





