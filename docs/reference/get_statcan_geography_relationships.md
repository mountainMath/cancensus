# Read the Dissemination Geographies Relationship File (deprecated)

This function is deprecated and will be removed in the next release, as
Statistics Canada blocks programmatic downloads of the file (#228). Use
`get_statcan_geographic_attributes("2021")` instead, which has the same
DGUIDs for each dissemination block apart from census agricultural
regions, along with the IDs, names and counts, and covers the censuses
back to 1981.

Unlike the relationship file, the attribute file fills `CMADGUID`
outside census metropolitan areas and census agglomerations with the
metropolitan influenced zone, and `CTDGUID` outside areas with census
tracts with a placeholder census tract. `CMATYPE %in% c("B", "D", "K")`
selects the blocks in census metropolitan areas and census
agglomerations. Census agricultural regions (`CARDGUID`) are only in the
relationship file, which can still be downloaded manually from
Statistics Canada.

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

  census year to get the data for, only 2021 is available

- refresh:

  (logical) refresh the cache if true

## Value

tibble with the relationship data. If Statistics Canada blocks the
download this gives a warning explaining how to download the file
manually, and returns the cached data when refreshing existing cached
data and `NULL` otherwise

## Examples

``` r
if (FALSE) { # \dontrun{
# use the attribute file instead
attributes <- get_statcan_geographic_attributes("2021")
} # }
```
