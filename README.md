# cancensus
R wrapper for calling CensusMapper APIs

This package provides a wrapper function for CensusMapper API calls from R to query specific census data and geographies for use in R.

The CensusMapper API is still in beta, the use of the CensusMapper API requires API keys, which can be obtained from [CensusMapper](https://CensusMapper.ca) by emailing Jens.

**Cancensus is currently in early beta and parts of the code may be subject to change.** 

## Installing the package

```
devtools::install_github("mountainmath/cancensus")
library(cancensus)
```

## Get your API key

**cancensus** requires a valid CensusMapper API key to use. You can obtain a free API key by [signing up](https://censusmapper.ca/users/sign_up) for a CensusMapper account. To check your API key, just go to "Edit Profile" (in the top-right of the CensusMapper menu bar). Once you have your key, you can store it in your system environment so it is automatically used in API calls. To do so just enter `options(cancensus.api_key = "your_api_key")`

## Currently available datasets

**cancensus** can access Statistics Canada Census data for the 2006 Census, the 2011 Census and National Household Survey, as well as the latest available data from the 2016 Cencus. You can run `cancensus::list_datasets` to check what datasets are currently available for access through the CensusMapper API. 

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

# Making maps: tmap
# install.packages("tmap")
library(tmap)
tm_shape(census_data) +
  tm_polygons("sd", style="quantile", title="Single Detached Homes")
  
# There are many other ways of visualizing spatial data such as geom_sf from development versions of ggplot2 as well as leaflet for interactive web maps. 
```
## To-do

* Provide more information about available datasets, Census variables, and geography directly through the **cancensus** package.
* Expand vignettes and provide additional examples
* Submit to CRAN

## Examples

* [Example 1](http://htmlpreview.github.io/?https://raw.githubusercontent.com/mountainMath/cancensus/master/vignettes/cancensus-demo.nb.html)
* [Example 2](http://htmlpreview.github.io/?https://github.com/mountainMath/cancensus/blob/master/vignettes/Working-with-cancensus.html)
* [Example 3](http://htmlpreview.github.io/?https://github.com/mountainMath/cancensus/blob/master/vignettes/dot_density_example.nb.html)

A more complete example of how to use this package can be viewed [on the CensusMapper](https://censusmapper.ca/r-censusmapper-demo.html).
