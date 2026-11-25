# CensusMapper's base map for a ggplot map

Adds CensusMapper's base map on top of a map of census data: landuse
(semi-transparent, so the data shows through), water, buildings (at high
zoom only), roads and place and road labels, drawn from CensusMapper's
vector tiles and styled like the maps on censusmapper.ca. As on
CensusMapper, add it after the
[`geom_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html) with
the census data, which it draws over.

## Usage

``` r
geom_cm_context(
  layers = c("landuse", "water", "buildings", "roads", "labels"),
  zoom = NULL,
  pixels = 800,
  scale = 1,
  language = getOption("cancensus.language", "en"),
  family = NULL,
  tile_server = "https://censusmapper.ca"
)
```

## Arguments

- layers:

  Which parts of the base map to draw, any of `"landuse"`, `"water"`,
  `"buildings"`, `"roads"` and `"labels"`.

- zoom:

  Zoom level (as on CensusMapper) to draw the base map at. By default
  the zoom level at which the area shown spans `pixels` pixels.

- pixels:

  The width, in pixels, of the CensusMapper map the plot mimics. Larger
  values show more detail, with relatively smaller lines and labels.

- scale:

  Factor to make lines and labels larger or smaller.

- language:

  Language of the labels, `"en"` or `"fr"`. Defaults to the
  `cancensus.language` option, and to `"en"` if that is not set.

- family:

  Font family of the labels. By default "Noto Sans", as on CensusMapper,
  if the systemfonts package finds it, otherwise "sans".

- tile_server:

  Where to get the tiles from.

## Value

An object to add to a ggplot with `+`.

## Details

The map looks like CensusMapper showing the data in a window `pixels`
wide (or at `zoom`), scaled to the size of the plot's panel when it is
drawn: that zoom level decides which features, roads and labels are
shown, and lines and labels keep their size relative to the map whatever
size the plot is drawn at. Labels are thinned out so that they don't
overlap.

The base map covers the area the plot shows: by default all its sf data,
or the limits set with
[`coord_cm_bbox()`](https://mountainmath.github.io/cancensus/reference/coord_cm_bbox.md)
or
[`ggplot2::coord_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html).
It is drawn in the plot's CRS, whatever CRS the data are in.

The tiles are downloaded when the plot is drawn and cached for the
session. They need a CensusMapper API key, the same as for the census
data, though tiles count against a separate tile quota. The key is taken
from the `CM_API_KEY` environment variable, set with
[`set_cancensus_api_key()`](https://mountainmath.github.io/cancensus/reference/set_cancensus_api_key.md).
Without an API key the plot is drawn without the base map, with a
warning.

Needs the ggplot2 package, and GDAL with its MVT driver for sf to read
the tiles. To style the base map differently, or to use it with other
mapping packages, get its features with
[`cm_context()`](https://mountainmath.github.io/cancensus/reference/cm_context.md).

## See also

[`cm_context()`](https://mountainmath.github.io/cancensus/reference/cm_context.md)
for the features of the base map as sf data.

## Examples

``` r
if (FALSE) { # \dontrun{
library(ggplot2)
census_data <- get_census("CA21", regions = list(CSD = "5915022"),
                          vectors = c(population = "v_CA21_1"),
                          level = "DA", geo_format = "sf")
ggplot(census_data) +
  geom_sf(aes(fill = population / `Shape Area`), colour = NA) +
  scale_fill_viridis_c(trans = "log") +
  geom_cm_context() +
  coord_sf(datum = NA)
} # }
```
