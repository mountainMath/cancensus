# Get attribution for datasets

Get attribution for datasets

## Usage

``` r
dataset_attribution(datasets, language = getOption("cancensus.language", "en"))
```

## Arguments

- datasets:

  Vector of dataset identifiers

- language:

  Language of the attribution, one of `"en"` or `"fr"`. Defaults to the
  `cancensus.language` option, and to `"en"` if that is not set.

## Value

Returns a string to be used as attribution for the given datasets.

## Examples

``` r

if (FALSE) { # \dontrun{
# Attribution string for the 2006 and 2016 census datasets
dataset_attribution(c('CA06','CA16'))
} # }
```
