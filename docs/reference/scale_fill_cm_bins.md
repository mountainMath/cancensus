# Binned colour scale for choropleth maps, as on CensusMapper

A discrete fill (or colour) scale for numeric values that bins them at
`breaks` and colours the bins from a palette, with legend labels for the
bins formatted for the kind of values shown. Values below the first
break go in the first bin and values at or above the last break in the
last, as in CensusMapper's maps. It extends
[`ggplot2::scale_fill_manual()`](https://ggplot2.tidyverse.org/reference/scale_manual.html):
`values` sets the colours directly and `labels` the legend labels.

## Usage

``` r
scale_fill_cm_bins(
  palette = "Blues",
  breaks = NULL,
  n = 5,
  direction = 1,
  units = c("number", "percentage", "percentage_100", "ratio", "currency",
    "currency_1000"),
  language = getOption("cancensus.language", "en"),
  values = NULL,
  labels = NULL,
  na.value = "grey60",
  ...,
  aesthetics = "fill"
)

scale_colour_cm_bins(..., aesthetics = "colour")
```

## Arguments

- palette:

  The palette the colours come from: the name of a ColorBrewer palette
  (e.g. `"Blues"` or `"YlOrRd"`, via RColorBrewer, as on CensusMapper),
  of a viridis palette (`"viridis"`, `"magma"`, `"inferno"`, `"plasma"`,
  `"cividis"`, `"rocket"`, `"mako"` or `"turbo"`, via viridisLite), or
  of a palette of
  [`grDevices::hcl.colors()`](https://rdrr.io/r/grDevices/palettes.html).
  Or a function that returns n colours.

- breaks:

  The cutoffs between the bins, in increasing order. By default `n` bins
  with about as many regions in each, at rounded cutoffs.

- n:

  The number of bins when there are no `breaks`.

- direction:

  1 for the palette's colours in their order, -1 for the reverse.

- units:

  What the values are, for formatting the labels: `"number"` (1.2k,
  3.4M), `"percentage"` (shares, 0.12 is 12%), `"percentage_100"`
  (percentages, 12 is 12%), `"ratio"` (1,230, 0.153), `"currency"`
  (\$1.2k) or `"currency_1000"` (currency in thousands). Labels show up
  to three significant digits, more only where needed to tell the breaks
  apart.

- language:

  Language for formatting the labels, `"en"` or `"fr"`. Defaults to the
  `cancensus.language` option, and to `"en"` if that is not set.

- values:

  Colours for the bins, from the lowest to the highest, instead of the
  palette's.

- labels:

  Labels for the bins instead of the ones made from `breaks`: a
  character vector, or a function that gets the breaks and returns the
  labels for the bins.

- na.value:

  The colour for missing values.

- ...:

  Other arguments passed on to
  [`ggplot2::discrete_scale()`](https://ggplot2.tidyverse.org/reference/discrete_scale.html),
  e.g. `name` or `guide`.

- aesthetics:

  The aesthetics the scale works with, e.g. `c("fill", "colour")`.

## Value

A ggplot2 scale.

## Details

The legend shows every bin in its colour, including bins without data.
If you pass your own `guide`, add `override.aes = list(.draw = TRUE)` to
it, or `show.legend = TRUE` to the layer, to keep this with ggplot2 3.5
or later.

## Examples

``` r
if (FALSE) { # \dontrun{
library(ggplot2)
census_data <- get_census("CA21", regions = list(CMA = "59933"),
                          vectors = c(lim_at = "v_CA21_1085"),
                          level = "CT", geo_format = "sf")
ggplot(census_data) +
  geom_sf(aes(fill = lim_at / 100), colour = NA) +
  scale_fill_cm_bins(palette = "YlOrRd", breaks = c(0.05, 0.1, 0.15, 0.2),
                     units = "percentage") +
  coord_sf(datum = NA)
} # }
```
