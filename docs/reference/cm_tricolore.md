# Colours for three-part compositions, as on CensusMapper

Colours for compositions of three parts, e.g. the shares of children,
working age population and seniors, with the balance scheme of the
tricolore package, as used by CensusMapper's ternary ("rgb") maps. The
colours are the same as
[`tricolore::Tricolore()`](https://rdrr.io/pkg/tricolore/man/Tricolore.html)'s,
with the parameters in CensusMapper's units, the same as in a
CensusMapper map's `tricolore_options`.

## Usage

``` r
cm_tricolore(
  p1,
  p2,
  p3,
  center = rep(1/3, 3),
  breaks = 3,
  hue = 60,
  chroma = 200,
  lightness = 80,
  contrast = 0.4,
  spread = 1
)
```

## Arguments

- p1, p2, p3:

  The three parts, as counts or shares; they are divided by their total.
  Compositions with missing or negative parts, or a total of zero, get
  `NA`.

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

## Value

A character vector of colours (`"#RRGGBB"`), one for each composition.

## Details

With discrete colours (finite `breaks`) the triangle of compositions is
split into `breaks^2` triangles, and each composition gets the colour of
the centroid of its triangle. The `center` changes the colours of these
triangles, but not their shape.

## References

Schöley, J. (2021). The centered ternary balance scheme: A technique to
visualize surfaces of unbalanced three-part compositions. Demographic
Research, 44(19), 443–458.
[doi:10.4054/DemRes.2021.44.19](https://doi.org/10.4054/DemRes.2021.44.19)

## See also

[`geom_cm_tricolore()`](https://mountainmath.github.io/cancensus/reference/geom_cm_tricolore.md)
for a ggplot map with these colours and a colour key.

## Examples

``` r
cm_tricolore(c(0.2, 0.5, 1), c(0.6, 0.3, 1), c(0.2, 0.2, 1))
#> [1] "#009F92" "#A8894C" "#C37787"
```
