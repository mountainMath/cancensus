## ----setup, echo=FALSE, message=FALSE, warning=FALSE--------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = nzchar(Sys.getenv("COMPILE_VIG"))
)
library(cancensus)
library(dplyr)
# options(cancensus.api_key = "your_api_key")

## ----load_package_cran, echo=TRUE, message=FALSE, warning=FALSE, eval = FALSE----
#  install.packages("cancensus")
#  
#  library(cancensus)
#  
#  options(cancensus.api_key = "your_api_key")
#  options(cancensus.cache_path = "custom cache path")

## ----load_package_git, echo=TRUE, message=FALSE, warning=FALSE, eval = FALSE----
#  # install.packages("devtools")
#  devtools::install_github("mountainmath/cancensus")
#  
#  library(cancensus)
#  
#  options(cancensus.api_key = "your_api_key")
#  options(cancensus.cache_path = "custom cache path")

## ----get_census example, echo=TRUE, warning=FALSE, message=FALSE, eval = FALSE----
#  # Returns a data frame with data only
#  census_data <- get_census(dataset='CA16', regions=list(CMA="59933"),
#                            vectors=c("v_CA16_408","v_CA16_409","v_CA16_410"),
#                            level='CSD', use_cache = FALSE, geo_format = NA)
#  
#  # Returns data and geography as an sf-class data frame
#  census_data <- get_census(dataset='CA16', regions=list(CMA="59933"),
#                            vectors=c("v_CA16_408","v_CA16_409","v_CA16_410"),
#                            level='CSD', use_cache = FALSE, geo_format = 'sf')
#  
#  # Returns a SpatialPolygonsDataFrame object with data and geography
#  census_data <- get_census(dataset='CA16', regions=list(CMA="59933"),
#                            vectors=c("v_CA16_408","v_CA16_409","v_CA16_410"),
#                            level='CSD', use_cache = FALSE, geo_format = 'sp')

## ----list datasets, message=FALSE, warning=FALSE------------------------------
list_census_datasets()

## ----list regions, message=FALSE, warning=FALSE-------------------------------
list_census_regions("CA16")

## ---- message=FALSE, warning=FALSE, eval=FALSE--------------------------------
#  # Retrieves Vancouver and Toronto
#  list_census_regions('CA16') %>%
#    filter(level == "CMA", name %in% c("Vancouver","Toronto"))
#  
#  census_data <- get_census(dataset='CA16', regions=list(CMA=c("59933","35535")),
#                            vectors=c("v_CA16_408","v_CA16_409","v_CA16_410"),
#                            level='CSD', use_cache = FALSE)

## ----list_vectors, message=FALSE, warning=FALSE-------------------------------
list_census_vectors("CA16")

## ----search_vectors1, message=FALSE, warning=FALSE----------------------------
# Find the variable indicating the number of people of Austrian ethnic origin
search_census_vectors("Austrian", dataset = 'CA16')

## ----search_vectors2, message=FALSE, warning=FALSE----------------------------
# Find the variable indicating the number of people of Austrian ethnic origin
search_census_vectors("Austraian", dataset = 'CA16')

## ----parent_vectors, message=FALSE, warning=FALSE-----------------------------
list_census_vectors("CA16") %>% 
  filter(vector == "v_CA16_4092") %>% 
  parent_census_vectors()

## ----child_vectors1, message=FALSE, warning=FALSE-----------------------------
# Find the variable indicating the Northern European aggregate
search_census_vectors("Northern European", dataset = 'CA16')

## ----child_vectors2, message=FALSE, warning=FALSE-----------------------------
# Show all child variable leaves
list_census_vectors("CA16") %>% 
  filter(vector == "v_CA16_4122") %>% child_census_vectors(leaves = TRUE)

