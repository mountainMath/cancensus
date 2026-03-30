# Read the Dissemination Geographies Relationship File

Reads the Dissemination Geographies Relationship File for the given
census year. The table contains the information on how all the
geographic levels are related for each area. Data gets cached after
first download if the cancensus cache path has been set. A reference
guide is available at
https://www150.statcan.gc.ca/n1/en/catalogue/982600032021001

## Usage

``` r
get_statcan_geography_relationships(census_year = "2021", refresh = FALSE)
```

## Arguments

- census_year:

  census year to get the data for, right now only 2021 is supported, for
  older years \`get_statcan_geographic_attributes()\` can fill in most
  of the information

- refresh:

  (logical) refresh the cache if true

## Value

tibble with the relationship data

## Examples

``` r
# list add the cached census data
if (FALSE) { # \dontrun{
get_statcan_geography_relationships("2021")
} # }
```
