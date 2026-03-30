# Query the CensusMapper API for regions with names matching a searchterm.

Runs a query against the CensusMapper API to retrieve region data with
names matching specific queries. Users can optionally specify the target
geography level (e.g. `level = 'CMA'`, `level = 'CSD'`, etc.).
Alternatively, calling
[`explore_census_vectors()`](https://mountainmath.github.io/cancensus/reference/explore_census_vectors.md)
will launch the interactive region selection tool on the CensusMapper
site in a new web page or tab.

## Usage

``` r
search_census_regions(searchterm, dataset, level = NA, ...)
```

## Arguments

- searchterm:

  The term to search for e.g. `"Victoria"`. Search terms are case
  insensitive. If unable to find a given search term, this function will
  suggest the correct spelling to use when possible.

- dataset:

  The dataset to query for available regions, e.g. `"CA16"`.

- level:

  One of `NA`, `'C'`, `'PR'`, `'CMA'`, `'CD'`, or `'CSD'`. If specified,
  only return variables of specified \`level\`.

- ...:

  Further arguments passed on to
  [`list_census_regions`](https://mountainmath.github.io/cancensus/reference/list_census_regions.md).

## Value

A census region list of the same format as \`list_census_regions()\`
containing the matches.

## Examples

``` r
if (FALSE) { # \dontrun{
# This will return a warning that no match was found, but will suggest similar named regions.
search_census_regions('Victorea', 'CA16')

# This will limit region results to only include CMA level regions
search_census_regions('Victoria', 'CA16', level = "CMA")
} # }
```
