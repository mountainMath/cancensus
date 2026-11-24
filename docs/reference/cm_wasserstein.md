# Wasserstein distance per region, as on CensusMapper

CensusMapper's `wasserstein()`: the p-Wasserstein distance between two
samples of the same size for each region, using
[`transport::wasserstein1d()`](https://rdrr.io/pkg/transport/man/wasserstein1d.html).
Needs the transport package.

## Usage

``` r
cm_wasserstein(a, b, p = 1)
```

## Arguments

- a, b:

  The samples: matrices with one row per region, e.g.
  `cbind(v1, v2, v3)` inside
  [`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html),
  or vectors that are the same for every region.

- p:

  The order of the distance, the same for every region or one per
  region.

## Value

A numeric vector with one value per region, NA where any input is
missing.

## Examples

``` r
if (requireNamespace("transport", quietly = TRUE)) {
  cm_wasserstein(cbind(c(1, 2), c(3, 4)), c(2, 2))
}
#> [1] 1 1
```
