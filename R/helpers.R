cancensus_base_url <- function(){
  url <- getOption("cancensus.base_url")
  if (is.null(url)) url <- "https://censusmapper.ca"
  url
}
