# Access to Canadian census data through the CensusMapper API

This function allows convenient access to Canadian census data and
boundary files through the CensusMapper API. An API key is required to
retrieve data.

## Usage

``` r
get_census(
  dataset,
  regions,
  level = NA,
  vectors = c(),
  geo_format = NA,
  resolution = "simplified",
  labels = "detailed",
  use_cache = TRUE,
  quiet = FALSE,
  api_key = Sys.getenv("CM_API_KEY"),
  retry = 0
)
```

## Source

Census data and boundary geographies are reproduced and distributed on
an "as is" basis with the permission of Statistics Canada (Statistics
Canada 1996; 2001; 2006; 2011; 2016).

## Arguments

- dataset:

  A CensusMapper dataset identifier.

- regions:

  A named list of census regions to retrieve. Names must be valid census
  aggregation levels.

- level:

  The census aggregation level to retrieve, defaults to `"Regions"`. One
  of `"Regions"`, `"PR"`, `"CMA"`, `"CD"`, `"CSD"`, `"CT"`, `"DA"`,
  `"EA"` (for 1996), or `"DB"` (for 2001-2016).

- vectors:

  An R vector containing the CensusMapper variable names of the census
  variables to download. If no vectors are specified only geographic
  data will get downloaded.

- geo_format:

  By default is set to `NA` and appends no geographic information. To
  include geographic information with census data, specify one of either
  `"sf"` to return an
  [`sf`](https://r-spatial.github.io/sf/reference/sf.html) object
  (requires the `sf` package) or `"sp"` to return a
  [`SpatialPolygonsDataFrame-class`](https://edzer.github.io/sp/reference/SpatialPolygonsDataFrame-class.html)
  object (requires the `rgdal` package). If user requests geo-spatial
  data and neither package is available, a context menu will prompt to
  install the `sf` package.

- resolution:

  Resolution of the geographic data. By default simplified geometries
  will be download. For lower level geometries like DB or DA this will
  be very close to the high resolution data. Simplification generally
  increases as the geographic aggregation level increases. If high
  resolution geometries are required then this option can be set to
  'high'. By default this setting is set to `'simplified'`.

- labels:

  Set to "detailed" by default, but truncated Census variable names can
  be selected by setting labels = "short". Use `label_vectors(...)` to
  return variable label information in detail.

- use_cache:

  If set to TRUE (the default) data will be read from the local cache if
  available.

- quiet:

  When TRUE, suppress messages and warnings.

- api_key:

  An API key for the CensusMapper API. Defaults to
  [`options()`](https://rdrr.io/r/base/options.html) and then the
  `CM_API_KEY` environment variable.

- retry:

  Integer If greater than zero, automatically retry failed API requests
  with exponential backoff for specified maximum number of times.
  Defaults to 0.

## Details

For help selecting regions and vectors, see
[`list_census_regions`](https://mountainmath.github.io/cancensus/reference/list_census_regions.md)
and
[`list_census_vectors`](https://mountainmath.github.io/cancensus/reference/list_census_vectors.md),
or check out the interactive selection tool at
<https://censusmapper.ca/api> by calling
[`explore_census_vectors()`](https://mountainmath.github.io/cancensus/reference/explore_census_vectors.md)

## Examples

``` r
# Query the API for data on dwellings in Vancouver, at the census subdivision
# level:
if (FALSE) { # \dontrun{
census_data <- get_census(dataset='CA16', regions=list(CMA="59933"),
                          vectors=c("v_CA16_408","v_CA16_409","v_CA16_410"),
                          level='CSD')

# Query the API for data on dwellings in Vancouver, at the census subdivision
# level, and return the associated geography files in \code{sf} format:
census_data <- get_census(dataset='CA16', regions=list(CMA="59933"),
                          vectors=c("v_CA16_408","v_CA16_409","v_CA16_410"),
                          level='CSD', geo_format = "sf")

# Make the same query, but this time drop descriptive vector names:
census_data <- get_census(dataset='CA16', regions=list(CMA="59933"),
                          vectors=c("v_CA16_408","v_CA16_409","v_CA16_410"),
                          level='CSD', geo_format = "sf", labels="short")

# Get details for truncated vectors:
label_vectors(census_data)
} # }
```
