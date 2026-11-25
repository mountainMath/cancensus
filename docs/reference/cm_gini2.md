# Gini coefficient from income brackets, CensusMapper's gini2()

Like
[`cm_gini`](https://mountainmath.github.io/cancensus/reference/cm_gini.md),
but the top bracket's income total is the larger of its estimate from
the bracket width and what the total income leaves after the other
brackets.

## Usage

``` r
cm_gini2(counts, lower_cutoffs, total = 0)
```

## Arguments

- counts:

  Counts per bracket: a matrix with one row per region and one column
  per bracket, e.g. `cbind(v1, v2, v3)` inside
  [`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html).

- lower_cutoffs:

  The brackets' lower cutoffs, a vector that is the same for every
  region, or a matrix like `counts`.

- total:

  Total income per region, or 0 (the default) for none.

## Value

A numeric vector with one value per region, NA where any input is
missing.

## Examples

``` r
cm_gini2(cbind(c(10, 5), c(20, 10), c(5, 20)), c(0, 20000, 50000), c(1e6, 2e6))
#> [1] 0.3095238 0.2464286
```
