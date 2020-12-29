## code to prepare `COV_SKYTRAIN_STATIONS` dataset goes here


COV_SKYTRAIN_STATIONS <- VancouvR::get_cov_data("rapid-transit-stations",format="geojson")

usethis::use_data(COV_SKYTRAIN_STATIONS, overwrite = TRUE)
