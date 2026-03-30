# Read the geosuite data

Reads the geosuite data for the given level and census year. Data gets
cached after first download if the cancensus cache path has been set.
For older years \`get_statcan_geographic_attributes()\` can fill in most
of the information

## Usage

``` r
get_statcan_geo_suite(level, census_year = "2021", refresh = FALSE)
```

## Arguments

- level:

  geographic level to return the data for, valid choices are "DB", "DA",
  "ADA", "CT", "CSD", "CMA", "CD", "PR", "FED", "DPL", "ER", "PN",
  "POPCTR"

- census_year:

  census year to get the data for, right now only 2021 is supported

- refresh:

  (logical) refresh the cache if true

## Value

tibble with the geosuite data

## Examples

``` r
# list add the cached census data
if (FALSE) { # \dontrun{
get_statcan_geo_suite("DA","2021")
} # }
```
