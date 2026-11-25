# The features of CensusMapper's base map as sf data

Gets the features of CensusMapper's base map for the extent of some sf
data or a bounding box from CensusMapper's vector tiles: water,
landcover, landuse, airports, buildings, roads, and places and road
labels. These are the features
[`geom_cm_context()`](https://mountainmath.github.io/cancensus/reference/geom_cm_context.md)
draws, unstyled, for drawing them in other ways or with other mapping
packages.

## Usage

``` r
cm_context(
  data,
  layers = c("water", "landcover", "landuse", "aeroway", "buildings", "roads", "places",
    "road_labels"),
  zoom = NULL,
  pixels = 800,
  language = getOption("cancensus.language", "en"),
  tile_server = "https://censusmapper.ca",
  api_key = Sys.getenv("CM_API_KEY")
)
```

## Arguments

- data:

  sf data, an sfc or a bounding box whose extent to get the base map
  for.

- layers:

  Which layers to get, any of `"water"`, `"landcover"`, `"landuse"`,
  `"aeroway"`, `"buildings"`, `"roads"`, `"places"` and `"road_labels"`.

- zoom:

  Zoom level (as on CensusMapper) whose tiles to use. By default the
  zoom level at which the data spans `pixels` pixels.

- pixels:

  The width, in pixels, of the CensusMapper map whose zoom level to use.

- language:

  Language of the names, `"en"` or `"fr"`. Defaults to the
  `cancensus.language` option, and to `"en"` if that is not set.

- tile_server:

  Where to get the tiles from.

- api_key:

  An API key for the CensusMapper API. Defaults to the `CM_API_KEY`
  environment variable, as set with
  [`set_cancensus_api_key()`](https://mountainmath.github.io/cancensus/reference/set_cancensus_api_key.md).

## Value

A named list with an sf data frame for each layer, in the CRS of `data`
and cropped to its bounding box. A layer without features is an sf data
frame without rows. Features have their `class`, and places and road
labels their `name` in the language `language` (or the local name). Road
labels are points along named roads, with the `angle` (in degrees) to
turn labels by and the `length` of the road (in metres). The other
columns come from the tiles and may change. The zoom level is in the
`"zoom"` attribute.

## Details

The tiles, and with them the detail of the features, are those
CensusMapper uses at the zoom level at which the data spans `pixels`
pixels, or at `zoom`. Which classes of features exist at a zoom level is
decided by the tiles;
[`geom_cm_context()`](https://mountainmath.github.io/cancensus/reference/geom_cm_context.md)
additionally shows some classes (minor roads, buildings, road labels)
only at higher zoom levels.

Tiles are downloaded from the tile server and cached for the session.
They need a CensusMapper API key, the same as for the census data,
though tiles count against a separate tile quota. Needs GDAL with its
MVT driver for sf to read the tiles.

## See also

[`geom_cm_context()`](https://mountainmath.github.io/cancensus/reference/geom_cm_context.md)
to add the base map to a ggplot map.

## Examples

``` r
if (FALSE) { # \dontrun{
library(ggplot2)
census_data <- get_census("CA21", regions = list(CSD = "5915022"),
                          vectors = c(population = "v_CA21_1"),
                          level = "DA", geo_format = "sf")
context <- cm_context(census_data, layers = c("water", "roads", "places"))
major <- context$roads[context$roads$class %in% c("motorway", "trunk", "primary"), ]
ggplot(census_data) +
  geom_sf(aes(fill = population / `Shape Area`), colour = NA) +
  geom_sf(data = context$water, fill = "lightblue", colour = NA) +
  geom_sf(data = major, colour = "grey30") +
  geom_sf_text(data = context$places[context$places$class == "suburb", ],
               aes(label = name), size = 3) +
  coord_sf(datum = NA)
} # }
```
