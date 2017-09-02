# cancensus
R wrapper for calling CensusMapper APIs

This package provides a wrapper function for CensusMapper API calls from R to query specific census data and geographies for use in R.

The CensusMapper API is still in beta, and the use of the CensusMapper API requires API keys. You can obtain a free API key by [signing up](https://censusmapper.ca/users/sign_up) for a CensusMapper account. CensusMapper API keys are free; however, API requests are limited in volume. For larger quotas, please get in touch with Jens [directly](mailto:jens@censusmapper.ca).  

**Cancensus is currently in early beta and parts of the code may be subject to change.** 

## Installing the package

```
devtools::install_github("mountainmath/cancensus")
library(cancensus)
```

## Get your API key

**cancensus** requires a valid CensusMapper API key to use. To check your API key, just go to "Edit Profile" (in the top-right of the CensusMapper menu bar). Once you have your key, you can store it in your system environment so it is automatically used in API calls. To do so just enter `options(cancensus.api_key = "your_api_key")`.

## Currently available datasets

**cancensus** can access Statistics Canada Census data for the 2006 Census, the 2011 Census and National Household Survey, as well as the latest available data from the 2016 Census. You can run `list_census_datasets` to check what datasets are currently available for access through the CensusMapper API. Additional data for the 2016 Census will be included in Censusmapper within a day or two after public release by Statistics Canada. Statistics Canada maintains a release schedule for the Census 2016 Program which can be viewed on their [website](http://www12.statcan.gc.ca/census-recensement/2016/ref/release-dates-diffusion-eng.cfm). The next release is income data, which is scheduled for September 13, 2017. 

## Picking regions and variables

Census data contains thousands of different geographic regions as well as thousands of unique variables. There are several useful functions within **cancensus** to simplify accessing Census metadata, locating regions, and identifying variables.

```
# To view available Census datasets
list_census_datasets()

# To view available named regions at different levels of Census hierarchy for the 2016 Census (for example)
list_census_regions("CA16")

# To view available Census variables for the 2016 Census
list_census_vectors("CA16")
```

As the number of Census regions and variables is significant, and Census naming patterns are not always the most intuitive, there are a few functions to help users narrow down the data to what they need. These functions will try to catch mispelled search terms and prompt the user with correctly-spelled alternatives.

```
# To search for regions containing a specific term (e.g. "Victoria") in them in the 2016 Census
search_census_regions("Victoria","CA16")

# To search for variables whose description or label contains a specific term (e.g. "Ojibwe")
search_census_vectors("Ojibwe","CA16")
```

There is also an interactive tool that is available at [CensusMapper API call generator](https://censusmapper.ca/api) to visually select regions and variables and generate code for the API call.

## Getting the data

**cancensus** can return census data with or without associated Census geographical information that can be used for mapping and GIS. By default, **cancensus** returns tidy tabular data only, but has options to return spatial data objects in either [**sf**](https://github.com/r-spatial/sf) or [**sp**](https://cran.r-project.org/web/packages/sp/sp.pdf) formats. 
```
# Return data only
census_data <- get_census(dataset='CA16', regions=list(CMA="59933"), vectors=c("v_CA16_408","v_CA16_409","v_CA16_410"), level='CSD')

# Return an sf-class data frame
census_data <- get_census(dataset='CA16', regions=list(CMA="59933"), vectors=c("v_CA16_408","v_CA16_409","v_CA16_410"), level='CSD', geo_format = "sf")

# Return a SpatialPolygonsDataFrame
census_data <- get_census(dataset='CA16', regions=list(CMA="59933"), vectors=c("v_CA16_408","v_CA16_409","v_CA16_410"), level='CSD', geo_format = "sp")

# Additional options can viewed in documentation by entering ?get_census() in your R console
```
**cancensus** attempts to minimize bandwidth usage and download time by caching downloads. When attempting to download data that has previously been downloaded,  **cancensus** will instead access the locally cached equivalent. 

## Analyze and Visualize

**cancensus** generates tidy data that is easy to work with for manipulation, analysis, and visualization. 

```
census_data$sd <- census_data$`v_CA16_409: Single-detached house` / census_data$`v_CA16_408: Occupied private dwellings by structural type of dwelling data` 

# or, equivalently using dplyr
census_data <- census_data %>% 
  mutate(sd = `v_CA16_409: Single-detached house`/`v_CA16_408: Occupied private dwellings by structural type of dwelling data`)

# Visualize:
# devtools::install_github("tidyverse/ggplot2") - development version of ggplot2 which includes geom_sf
library(ggplot2)
ggplot(census_data) +
  geom_sf(aes(fill = sd)) +
  scale_fill_viridis_c("%SD") +
  ggtitle("Proportion of Dwelling Units that are Single Detached") +
  theme_bw()
  
# There are many other ways of visualizing spatial data such as ggmap, tmap, and leaflet for interactive web maps. 
```
## To-do

* Expand vignettes and provide additional examples
* Submit to CRAN

## Examples

* [Example 1](http://htmlpreview.github.io/?https://raw.githubusercontent.com/mountainMath/cancensus/master/vignettes/cancensus-demo.nb.html)
* [Example 2](http://htmlpreview.github.io/?https://github.com/mountainMath/cancensus/blob/master/vignettes/Working-with-cancensus.html)
* [Example 3](http://htmlpreview.github.io/?https://github.com/mountainMath/cancensus/blob/master/vignettes/dot_density_example.nb.html)

## Version History

* 0.2 - Added vector and geography discovery capabilities, improved performance, error and API messaging, package dependencies
* 0.1 - First release of **cancensus** 
