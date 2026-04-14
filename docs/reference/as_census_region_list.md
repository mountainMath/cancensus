# Convert a (suitably filtered) data frame from [`list_census_regions`](https://mountainmath.github.io/cancensus/reference/list_census_regions.md) to a list suitable for passing to [`get_census`](https://mountainmath.github.io/cancensus/reference/get_census.md).

Convert a (suitably filtered) data frame from
[`list_census_regions`](https://mountainmath.github.io/cancensus/reference/list_census_regions.md)
to a list suitable for passing to
[`get_census`](https://mountainmath.github.io/cancensus/reference/get_census.md).

## Usage

``` r
as_census_region_list(tbl)
```

## Arguments

- tbl:

  A data frame, suitably filtered, as returned by
  [`list_census_regions`](https://mountainmath.github.io/cancensus/reference/list_census_regions.md).

## Examples

``` r
if (FALSE) { # \dontrun{
library(dplyr, warn.conflicts = FALSE)

# Query the CensusMapper API for the total occupied dwellings
# of 20 random Census Subdivisions, in Census 2016.
regions <- list_census_regions("CA16") %>%
  filter(level == "CSD") %>%
  sample_n(20) %>%
  as_census_region_list()

occupied <- get_census("CA16", regions = regions,
                            vectors = c("v_CA16_408"),
                            level = "Regions")
} # }
```
