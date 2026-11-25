# Quantile from brackets, as on CensusMapper

CensusMapper's `binned_quantile()` (and
[`quantile()`](https://rdrr.io/r/stats/quantile.html)): the p-quantile,
e.g. the median income, from counts per bracket and the brackets' lower
cutoffs, interpolating linearly within the bracket. The lowest bracket
starts at 0 and the top bracket ends at twice its lower cutoff.

## Usage

``` r
cm_binned_quantile(counts, lower_cutoffs, p)
```

## Arguments

- counts:

  Counts per bracket: a matrix with one row per region and one column
  per bracket, e.g. `cbind(v1, v2, v3)` inside
  [`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html).

- lower_cutoffs:

  The brackets' lower cutoffs, a vector that is the same for every
  region, or a matrix like `counts`.

- p:

  The quantile, between 0 and 1, the same for every region or one per
  region.

## Value

A numeric vector with one value per region, NA where any input is
missing and NaN where there are no counts.

## Examples

``` r
cm_binned_quantile(cbind(c(10, 5), c(20, 10), c(5, 20)), c(0, 20000, 50000), 0.5)
#> [1] 31250 56250
```
