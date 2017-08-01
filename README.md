# cancensus
R wrapper for calling CensusMapper APIs

This package provides a wrapper function for CensusMapper API calls from R to query specific census data and geographies for use in R.

The CensusMapper API is still in beta, the use of the CensusMapper API requires API keys, which can be obtained from [CensusMapper](https://CensusMapper.ca) by emailing Jens.

To install the package
```
devtools::install_github("mountainmath/cancensus")
```

To use the functionality you have to first set the CensusMapper API key
```
library(cancensus)
cancensus.set_api_key('<your API key>')
```

Go to the [CensusMapper API call generator](https://censusmapper.ca/api) to select regions and variables and generate code for the API call, for example
```
census_data <- cancensus.load(dataset='CA16', regions='{"CMA":["59933"]}', vectors=c("v_CA16_408","v_CA16_409","v_CA16_414"), level='CT')
```

Then use the data for analysis or visualization, for example

```
census_data$sd <- census_data$`v_CA16_409: Single-detached house` / census_data$`v_CA16_408: Occupied private dwellings by structural type of dwelling data` 

# or, equivalently using dplyr
census_data2 <- census_data %>% 
  mutate(sd = `v_CA16_409: Single-detached house`/`v_CA16_408: Occupied private dwellings by structural type of dwelling data`)

# install.package("tmap")
library(tmap)
tm_shape(census_data) +
  tm_polygons("sd", style="quantile", title="Single Detached Homes")
```

A more complete example of how to use this package can be viewed [on the CensusMapper](https://censusmapper.ca/r-censusmapper-demo.html).
