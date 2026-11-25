# Read the Dissemination Geographies Relationship File (deprecated)

This function is deprecated and will be removed in a future release, as
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

  census year to get the data for, "2021" or "2026". The 2026 file
  doesn't have the census metropolitan areas and census agglomerations,
  census tracts, population centres and designated places yet,
  Statistics Canada plans to release these geographies on February
  10, 2027. Use `refresh = TRUE` to get an updated file after that.

- refresh:

  (logical) refresh the cache if true

## Value

tibble with the relationship data. If Statistics Canada blocks the
download, it goes through a browser with the safeconduct package where
that is installed and finds Google Chrome or another Chromium-based
browser, as for
[`get_statcan_geographies()`](https://mountainmath.github.io/cancensus/reference/get_statcan_geographies.md).
Otherwise, or if that fails, this gives a warning explaining how to
download the file manually, and returns the cached data when refreshing
existing cached data and `NULL` otherwise

## Examples

``` r
if (FALSE) { # \dontrun{
# use the attribute file instead
attributes <- get_statcan_geographic_attributes("2021")
} # }
```
