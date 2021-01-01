## code to prepare `COV_SKYTRAIN_STATIONS` dataset goes here


COV_SKYTRAIN_STATIONS <- VancouvR::get_cov_data("rapid-transit-stations",format="geojson") #%>%
  #st_transform(26910) %>%
  #st_buffer(800) %>%
  #st_transform(4326)

usethis::use_data(COV_SKYTRAIN_STATIONS, overwrite = TRUE)
