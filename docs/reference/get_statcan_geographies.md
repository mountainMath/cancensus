# Read the geosuite data

Reads the original unprocessed geographic boundary files from Statistics
Canada

## Usage

``` r
get_statcan_geographies(
  census_year,
  level,
  type = "cartographic",
  cache_path = NULL,
  timeout = 1000,
  refresh = FALSE,
  quiet = FALSE
)
```

## Arguments

- census_year:

  census year to get the data for, right now only 2021 is supported

- level:

  geographic level to return the data for, valid choices are
  "PR","CD","CMACA","CSD","CT","ADA","DA","DB,"ER","FED","DPL","POPCNTR",
  "FSA", "HR"

- type:

  type of geographic data, valid choices area "cartographic" or
  "digital"

- cache_path:

  optional path to cache the data. If the cancensus cache path is set
  the geographic data gets cached in the "geographies" subdirectory of
  the cancensus cache path.

- timeout:

  optional timeout parameter, adjust as needed if the data download
  times out when using slow connections

- refresh:

  (logical) refresh the cache if true

- quiet:

  (logical) suppress messages if \`TRUE\`

## Value

a spatial dataframe with the geographic data

## Examples

``` r
# get the digital geographic boundaries for provinces and territories
if (FALSE) { # \dontrun{
get_statcan_geographies(census_year="2021",level="PR",type="digital")
} # }
```
