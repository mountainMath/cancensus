# A ggplot map of three-part compositions, as on CensusMapper

Maps compositions of three parts, e.g. the shares of children, working
age population and seniors, in the colours of CensusMapper's ternary
("rgb") maps (see
[`cm_tricolore()`](https://mountainmath.github.io/cancensus/reference/cm_tricolore.md)),
with a triangular colour key: a
[`ggplot2::geom_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html)
layer with the colours as fill, and the key as a legend. The key shows
the colours of compositions, the first part at the top, the second
bottom left and the third bottom right, and the regions as dots.

## Usage

``` r
geom_cm_tricolore(
  mapping = NULL,
  data = NULL,
  ...,
  labels = NULL,
  name = NULL,
  center = rep(1/3, 3),
  breaks = 3,
  hue = 60,
  chroma = 200,
  lightness = 80,
  contrast = 0.4,
  spread = 1,
  na.value = "grey60",
  show_data = TRUE,
  key_size = 3,
  show.legend = TRUE
)
```

## Arguments

- mapping:

  Aesthetics made with
  [`ggplot2::aes()`](https://ggplot2.tidyverse.org/reference/aes.html):
  `p1`, `p2` and `p3`, the three parts, as counts or shares, are
  required. Others, e.g. `colour`, are passed on to
  [`geom_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html).

- data:

  The data for the layer, sf data; by default the plot's data.

- ...:

  Other arguments passed on to
  [`ggplot2::geom_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html),
  e.g. `colour = NA`.

- labels:

  Labels for the three parts on the key. By default the expressions for
  `p1`, `p2` and `p3`. Long labels are wrapped.

- name:

  The title of the key, by default none.

- center:

  The composition that gets the neutral (grey) colour, e.g. the
  composition of the whole region. By default the centre of the
  triangle.

- breaks:

  The number of breaks along each side of the triangle for discrete
  colours, `Inf` for continuous colours.

- hue:

  The hue of the first part, in degrees (0 to 360); the other parts are
  120 and 240 degrees further.

- chroma:

  The maximum chroma (CIE-LUV, 0 to 200). Above about 75 some colours
  are outside the sRGB gamut and are clipped.

- lightness:

  The lightness (0 to 100).

- contrast:

  How much less light and chroma compositions near the centre get (0 to
  1).

- spread:

  How spread out the colours are around the centre (\> 0).

- na.value:

  The colour for regions without a composition.

- show_data:

  Whether to show the regions as dots on the key.

- key_size:

  The width of the key, in cm.

- show.legend:

  Whether to show the key.

## Value

An object to add to a ggplot with `+`.

## Details

The fill colours are set directly, not through a fill scale, so the plot
can still have a fill scale for other layers. Regions without a
composition (missing or negative parts, or a total of zero) are
`na.value`. The compositions are computed from the layer's data, or the
plot's data if the layer has none, when the layer is added to the plot.

## See also

[`cm_tricolore()`](https://mountainmath.github.io/cancensus/reference/cm_tricolore.md)
for just the colours.

## Examples

``` r
if (FALSE) { # \dontrun{
library(ggplot2)
census_data <- get_census("CA21", regions = list(CMA = "59933"),
                          vectors = c(children = "v_CA21_11",
                                      working_age = "v_CA21_68",
                                      seniors = "v_CA21_251"),
                          level = "CT", geo_format = "sf")
ggplot(census_data) +
  geom_cm_tricolore(aes(p1 = children, p2 = working_age, p3 = seniors),
                    labels = c("0 to 14", "15 to 64", "65 and over"),
                    colour = NA) +
  geom_cm_context() +
  coord_sf(datum = NA)
} # }
```
