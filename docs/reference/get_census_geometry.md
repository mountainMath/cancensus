# Deprecated, use \`get_census\` instead

This function will be removed in future versions.

## Usage

``` r
get_census_geometry(dataset, regions, level = NA, geo_format = "sf", ...)
```

## Source

Census data and boundary geographies are reproduced and distributed on
an "as is" basis with the permission of Statistics Canada (Statistics
Canada 2006; 2011; 2016).

## Arguments

- dataset:

  A CensusMapper dataset identifier.

- regions:

  A named list of census regions to retrieve. Names must be valid census
  aggregation levels.

- level:

  The census aggregation level to retrieve, defaults to `"Regions"`. One
  of `"Regions"`, `"PR"`, `"CMA"`, `"CD"`, `"CSD"`, `"CT"` or `"DA"`.

- geo_format:

  By default is set to `NA` and appends no geographic information. To
  include geographic information with census data, specify one of either
  `"sf"` to return an
  [`sf`](https://r-spatial.github.io/sf/reference/sf.html) object
  (requires the `sf` package) or `"sp"` to return a
  [`SpatialPolygonsDataFrame-class`](https://edzer.github.io/sp/reference/SpatialPolygonsDataFrame-class.html)
  object (requires the `rgdal` package).

- ...:

  Further arguments passed to `get_census`.

## Examples

``` r
# Query the API for data geographies in Vancouver, at the census subdivision
# level:
if (FALSE) { # \dontrun{
# Query the API for geographies in Vancouver, at the census subdivision
# level, and return the associated geography files in \code{sf} format:
census_data <- get_census(dataset='CA16', regions=list(CMA="59933"),
                          level='CSD', geo_format = "sf")
} # }
```
