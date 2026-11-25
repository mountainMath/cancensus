# Show the area of a bounding box on a ggplot map

A
[`ggplot2::coord_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html)
that limits a map to a bounding box, e.g. the area of a CensusMapper
map's view, while the data can extend beyond it. The box can be in any
CRS: it is turned into the map's CRS. With `bbox = NULL` the map shows
all its data, as with
[`coord_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html), so
that cropping is switched off by setting the box to `NULL`.
[`geom_cm_context()`](https://mountainmath.github.io/cancensus/reference/geom_cm_context.md)
covers the area shown.

## Usage

``` r
coord_cm_bbox(bbox = NULL, crs = NULL, datum = NA, ...)
```

## Arguments

- bbox:

  The area to show: a bounding box
  ([`sf::st_bbox()`](https://r-spatial.github.io/sf/reference/st_bbox.html)),
  or sf data or an sfc whose bounding box to show, or a numeric vector
  `c(xmin, ymin, xmax, ymax)`. A box without a CRS, and a numeric
  vector, are taken to be in the CRS of the map's data (of the plot, or
  of its first sf layer). `NULL` to show all data.

- crs:

  The CRS of the map. By default that of the map's data.

- datum:

  The CRS of the graticule; by default none.

- ...:

  Other arguments passed on to
  [`ggplot2::coord_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html),
  e.g. `expand`.

## Value

An object to add to a ggplot with `+`.

## Examples

``` r
if (FALSE) { # \dontrun{
library(ggplot2)
census_data <- get_census("CA21", regions = list(CMA = "59933"),
                          vectors = c(population = "v_CA21_1"),
                          level = "CT", geo_format = "sf")
view_bbox <- sf::st_bbox(c(xmin = -123.19, ymin = 49.24, xmax = -123.05, ymax = 49.30),
                         crs = 4326)
ggplot(census_data) +
  geom_sf(aes(fill = population / `Shape Area`), colour = NA) +
  geom_cm_context() +
  coord_cm_bbox(view_bbox)
} # }
```
