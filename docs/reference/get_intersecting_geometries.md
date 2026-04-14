# Get identifiers for census regions intersecting a geometry

This function returns a list of regions that intersect a given geometry
input. This list of regions can be used directly to query census when
one is interested in census data for a particular geographic region that
does not coincide with defined census geometries. The returned value can
be used as the `regions` parameter in
[get_census](https://mountainmath.github.io/cancensus/reference/get_census.md)
to get corresponding census geographies and variables that cover the
give geometry. Input spatial objects can be any `sf` or `sfc` class
objects such as `POINT`, `MULTIPOINT` or `POLYGON`.

This function comes with CensusMapper API limits

## Usage

``` r
get_intersecting_geometries(
  dataset,
  level,
  geometry,
  simplified = FALSE,
  use_cache = TRUE,
  quiet = FALSE,
  api_key = Sys.getenv("CM_API_KEY")
)
```

## Source

Census data and boundary geographies are reproduced and distributed on
an "as is" basis with the permission of Statistics Canada (Statistics
Canada 1996; 2001; 2006; 2011; 2016).

## Arguments

- dataset:

  A CensusMapper dataset identifier.

- level:

  The census aggregation level to retrieve. One of `"Regions"`, `"PR"`,
  `"CMA"`, `"CD"`, `"CSD"`, `"CT"`, `"DA"`, `"EA"` (for 1996 census), or
  `"DB"` (for 2001-2016 censuses)..

- geometry:

  A valid `sf` or `sfc` class object. As long as the geometry is valid,
  any projection is accepted. Objects will be reprojected as server-side
  intersections use lat/lon projection.

- simplified:

  If `TRUE` will return a region list compatible with
  [get_census](https://mountainmath.github.io/cancensus/reference/get_census.md),
  otherwise will return a character vector of matching region ids.

- use_cache:

  If set to `TRUE` (the default) data will be read from the local cache
  if available.

- quiet:

  When TRUE, suppress messages and warnings.

- api_key:

  An API key for the CensusMapper API. Defaults to
  [`options()`](https://rdrr.io/r/base/options.html) and then the
  `CM_API_KEY` environment variable.

## Examples

``` r

if (FALSE) { # \dontrun{
# Example using a POINT-class object from a pair of lat/lon coordinates
point_geo <- sf::st_sfc(sf::st_point(c(-123.25149, 49.27026)), crs=4326)
regions <- get_intersecting_geometries(dataset = 'CA16', level = 'CT', geometry = point_geo)
census_data <- get_census(dataset='CA16', regions=regions,
                          vectors=c("v_CA16_408","v_CA16_409","v_CA16_410"),
                          level='CT')

# Example using a POLYGON-class object from a bounding box with four coordinates
poly_geo <- sf::st_as_sfc(sf::st_bbox(c(ymin = 49.25, ymax = 49.30,
                          xmin = -123.25, xmax = -123.30)), crs = 4326)
regions <- get_intersecting_geometries(dataset = 'CA16', level = 'CT', geometry = poly_geo)
census_data <- get_census(dataset='CA16', regions=regions,
                         vectors=c("v_CA16_408","v_CA16_409","v_CA16_410"), level='CT')

} # }
```
