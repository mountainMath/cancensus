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

**cancensus** can access Statistics Canada Census data for the 2006 Census, the 2011 Census and National Household Survey, as well as the latest available data from the 2016 Census. You can run `cancensus.list_datasets` to check what datasets are currently available for access through the CensusMapper API. Additional data for the 2016 Census will be included in Censusmapper within a day or two after public release by Statistics Canada. Statistics Canada maintains a release schedule for the Census 2016 Program which can be viewed on their [website](http://www12.statcan.gc.ca/census-recensement/2016/ref/release-dates-diffusion-eng.cfm). The next release is income data, which is scheduled for September 13, 2017. 

## Picking regions and variables

Census data contains thousands of different geographic regions as well as thousands of unique variables. An interactive tool is available at [CensusMapper API call generator](https://censusmapper.ca/api) to select regions and variables and generate code for the API call, for example:

```
census_data <- cancensus.load(dataset='CA16', regions=list(CMA="59933"), vectors=c("v_CA16_408","v_CA16_409","v_CA16_410"), level='CSD', geo_format = "sf")

# Additional options can viewed in documentation by entering ?cancensus.load() in your R console
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

* Provide more information about available datasets, Census variables, and geography directly through the **cancensus** package.
* Expand vignettes and provide additional examples
* Submit to CRAN

## Examples

* [Example 1](http://htmlpreview.github.io/?https://raw.githubusercontent.com/mountainMath/cancensus/master/vignettes/cancensus-demo.nb.html)
* [Example 2](http://htmlpreview.github.io/?https://github.com/mountainMath/cancensus/blob/master/vignettes/Working-with-cancensus.html)
* [Example 3](http://htmlpreview.github.io/?https://github.com/mountainMath/cancensus/blob/master/vignettes/dot_density_example.nb.html)

## Version History

* 0.2 - Added vector and geography discovery capabilities, improved performance, error and API messaging, package dependencies
* 0.1 - First release of **cancensus** 
