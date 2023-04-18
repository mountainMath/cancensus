#' Query the StatCan WDS for metadata
#'
#' @description
#' Get official metadata information from Statistics Canada for a given geographic level. Only available for the 2021 census.
#' Data is cached for the duration of the R session.
#'
#' @param census_year census year to get the data for, right now only 2021 is supported
#' @param level geographic level to return the data for, valid choices are
#' "PR","CD","CMACA","CSD","CT","ADA","DA","ER","FED","DPL","POPCNTR", "FSA"
#' @param refresh default is `FALSE` will refresh the temporary cache if `TRUE`
#' @return tibble with the metadata
#'
#' @examples
#' # get metadata for federal electoral districts
#' \dontrun{
#' get_statcan_wds_metadata(census_year="2021",level="FED")
#' }
#' @export
get_statcan_wds_metadata <- function(census_year,level,refresh=FALSE){
  valid_census_years <- c("2021")
  valid_levels <- c("PR","CD","CMACA","CSD","CT","ADA","DA","ER","FED","DPL","POPCNTR","FSA")
  if (!(census_year %in% valid_census_years)) {
    stop(paste0("Census year must be one of ",paste0(valid_census_years,collapse = ", "),"."))
  }
  if (!(level %in% valid_levels)) {
    stop(paste0("Level must be one of ",paste0(valid_levels,collapse = ", "),"."))
  }
  meta_url <- paste0("https://api.statcan.gc.ca/census-recensement/profile/sdmx/rest/dataflow/STC_CP/DF_",level,"?references=all")
  metadata_tempfile <- file.path(tempdir(),paste0("census_wds_metadata_",digest::digest(meta_url),".sdmx"))
  if (refresh || !file.exists(metadata_tempfile)) {
    utils::download.file(meta_url,metadata_tempfile)
  }
  d <- xml2::read_xml(metadata_tempfile)
  code_lists <- xml2::xml_find_all(d,"//structure:Codelist")

  meta_data <- lapply(code_lists, \(cl){
    codelist_id <- cl |> xml2::xml_attr("id")
    agencyID <- cl |> xml2::xml_attr("agencyID")
    codelist_en <- cl |> xml2::xml_find_all("common:Name[@xml:lang='en']") |> xml2::xml_text()
    codelist_fr <- cl |> xml2::xml_find_all("common:Name[@xml:lang='fr']") |> xml2::xml_text()
    description_en <- cl |> xml2::xml_find_all("common:Name[@xml:lang='en']") |> xml2::xml_text()
    description_fr <- cl |> xml2::xml_find_all("common:Name[@xml:lang='fr']") |> xml2::xml_text()
    codes <- cl |> xml2::xml_find_all("structure:Code")
    dplyr::tibble(`Agency ID`=agencyID,
           `Codelist ID`=codelist_id,
           `Codelist en`=codelist_en,
           `Codelist fr`=codelist_fr,
           ID=codes |> xml2::xml_attr("id"),
           en=codes |> xml2::xml_find_all("common:Name[@xml:lang='en']") |> xml2::xml_text(),
           fr=codes |> xml2::xml_find_all("common:Name[@xml:lang='fr']") |> xml2::xml_text(),
           `Parent ID`=codes |> xml2::xml_find_all("structure:Parent/Ref",flatten=FALSE) |>
             lapply(\(d)ifelse(is.null(d),NA,xml2::xml_attr(d,"id")))  |> unlist()
             )
  }) |>
    dplyr::bind_rows()
 meta_data
}

#' Query the StatCan WDS for data
#'
#' @description
#' Get official census data from Statistics Canada for a given set of DGUIDs. Only available for the 2021 census. The
#' downloaded data gets enriched by geographic and characteristic names based on metadata obtained via `get_statcan_wds_metadata()`.
#' Data is cached for the duration of the R session.
#'
#' @param DGUIDs census year to get the data for, right now only 2021 is supported. Valid DGUIDs for a given geographic
#' level can be queried via `get_statcan_wds_metadata()`.
#' @param members list of Member IOs to download data for. By default all characteristics are downloaded. Valid
#' Member IDs and their descriptions can be queried via the `get_statcan_wds_metadata()` call.
#' @param gender optionally query data for only one gender. By default this queries data for all genders, possible
#' values are "Total", "Male", "Female" to only query total data, or for males only or for females only.
#' @param language specify language for geography and characteristic names that get added, valid choices are "en" and "fr"
#' @param refresh default is `FALSE` will refresh the temporary cache if `TRUE`
#' @return tibble with the enriched census data
#'
#' @examples
#' # get data for federal electoral district 2013A000459021
#' \dontrun{
#' get_statcan_wds_data(DGUIDs="2013A000459021",level="FED")
#' }
#' @export
get_statcan_wds_data <- function(DGUIDs,
                           members = NULL,
                           gender="All",
                           language="en",
                           refresh=FALSE) {
  DGUIDs <- sort(DGUIDs)
  members <- sort(members)
  level <- geo_level_from_DGUID(DGUIDs[1])
  url <- paste0("https://api.statcan.gc.ca/census-recensement/profile/sdmx/rest/data/STC_CP")
  gender <- tolower(gender)
  gender <- paste0(toupper(substr(gender,1,1)),substr(gender,2,100))
  valid_genders <- c("All","Total","Male","Female")
  if (!(gender %in% valid_genders)) {
    stop(paste0("Gender must be one of ",paste0(valid_genders,collapse = ", "),"."))
  }
  language <- tolower(language)
  valid_languages <- c("en","fr")
  if (!(language %in% valid_languages)) {
    stop(paste0("Language must be one of ",paste0(valid_languages,collapse = ", "),"."))
  }
  gender <- c("All"="","Total"="1","Male"="2","Female"="3")[[gender]]
  dguid_string <- paste0(DGUIDs,collapse="+")
  member_string <- paste0(members,collapse = "+")
  add=paste0("DF_",level,"/A5.",dguid_string,".",gender,".",member_string,".1")
  wds_data_tempfile <- file.path(tempdir(),paste0("wds_data_",digest::digest(add),".csv"))
  if (!file.exists(wds_data_tempfile)) {
    response <- httr::GET(paste0(url,",",add),
                          httr::accept("text/csv"),
                          httr::add_headers("Accept-Encoding"="deflate, gzip, br"),
                          httr::write_disk(wds_data_tempfile,overwrite = TRUE))
  }
  if (!response$status_code=="200") {
    stop(paste0("Invalid request.\n",httr::content(response)))
  }
  census_year <- "2021"
  meta_data <- get_statcan_wds_metadata(census_year,level,refresh = refresh)

  levels <- meta_data |>
    dplyr::filter(.data$`Codelist ID`=="CL_GEO_LEVEL")

  meta_geos <- meta_data |>
    dplyr::filter(.data$`Codelist ID`==paste0("CL_GEO_",level))
  meta_characteristics <- meta_data |>
    dplyr::filter(.data$`Codelist ID`=="CL_CHARACTERISTIC")

  name_field <- language #paste0(language,"_description")

  data <- readr::read_csv(wds_data_tempfile,col_types = readr::cols(.default="c")) |>
    dplyr::mutate(dplyr::across(dplyr::matches("OBS_VALUE|TNR_CI_"),as.numeric)) |>
    dplyr::left_join(meta_geos |>
                       dplyr::select(GEO_DESC=.data$ID,GEO_NAME=!!as.name(name_field)),
                     by="GEO_DESC") |>
    dplyr::left_join(meta_characteristics |>
                       dplyr::select(CHARACTERISTIC=.data$ID,CHARACTERISTIC_NAME=!!as.name(name_field)),
              by="CHARACTERISTIC")

  data
}



geo_level_from_DGUID <- function(DGUID,simple=TRUE){
  schema <- substr(DGUID,"6","9")
  schema_to_level <- c("0000"="C","0001"="C",
                       "0002"="PR",
                       "0003"="CD",
                       "0004"="FED",
                       "0005"="CSD",
                       "0006"="DPL",
                       "0007"="HR",
                       "0008"="LHR",
                       "0011"="FSA",
                       "0500"="ER",
                       "0501"="CAR",
                       "0502"="CCSD",
                       "0503"="CMA",
                       "0504"="CA",
                       "0505"="CMAP",
                       "0507"="CT",
                       "0510"="PC",
                       "0511"="PCP",
                       "0512"="DA",
                       "0513"="DB",
                       "0516"="ADA")
  level <- schema_to_level[[schema]]
  if (simple){
    simple_translation <- c("LHR"="HR","CMA"="CMACA","CA"="CMACA","CMAP"="CMACA")
    if (level %in% names(simple_translation)) level <- simple_translation[[level]]
  }
  level
}


