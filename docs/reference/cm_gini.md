# Gini coefficient from income brackets, as on CensusMapper

CensusMapper's `gini()`: the Gini coefficient from counts per income
bracket and the brackets' lower cutoffs, one value per region. Incomes
are taken at the bracket midpoints. The top bracket's income is twice
its lower cutoff less the previous cutoff, or is estimated from the
total income when that is given.

## Usage

``` r
cm_gini(counts, lower_cutoffs, total = 0)
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
cm_gini(cbind(c(10, 5), c(20, 10), c(5, 20)), c(0, 20000, 50000))
#> [1] 0.3095238 0.2464286
```
