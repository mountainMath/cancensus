# Query the CensusMapper API for vectors with descriptions matching a search term or phrase (deprecated)

Query the CensusMapper API for vectors with descriptions matching a
search term or phrase (deprecated)

## Usage

``` r
search_census_vectors(searchterm, dataset, type = NA, ...)
```

## Arguments

- searchterm:

  The term or phrase to search for e.g. `"Ojibway"`. Search terms are
  case insensitive. If unable to find a given string, this function will
  suggest similarly named objects.

- dataset:

  The dataset to query for available vectors, e.g. `"CA16"`.

- type:

  One of `NA`, `'Total'`, `'Male'` or `'Female'`. If specified, only
  return variables of specified \`type\`.

- ...:

  Further arguments passed on to
  [`list_census_vectors`](https://mountainmath.github.io/cancensus/reference/list_census_vectors.md).

## Examples

``` r
if (FALSE) { # \dontrun{
search_census_vectors('Ojibway', 'CA16')

# This will return a warning that no match was found, but will suggest similar terms.
search_census_vectors('Ojibwe', 'CA16', 'Total')
} # }
```
