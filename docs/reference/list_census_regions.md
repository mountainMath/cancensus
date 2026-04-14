# Query the CensusMapper API for available regions in a given dataset.

Query the CensusMapper API for available regions in a given dataset.

## Usage

``` r
list_census_regions(dataset, use_cache = TRUE, quiet = FALSE)
```

## Arguments

- dataset:

  The dataset to query for available regions, e.g. `"CA16"`.

- use_cache:

  If set to TRUE (the default), data will be read from a local cache
  that is maintained for the duration of the R session, if available. If
  set to FALSE, query the API for the data, and refresh the local cache
  with the result.

- quiet:

  When TRUE, suppress messages and warnings.

## Value

Returns a data frame with the following columns:

- `region`:

  The region identifier.

- `name`:

  The name of that region.

- `level`:

  The census aggregation level of that region.

- `pop`:

  The population of that region.

- `municipal_status`:

  Additional identifiers to distinguish the municipal status of census
  subdivisions.

- `CMA_UID`:

  The identifier for the Census Metropolitan Area the region is in (if
  any).

- `CD_UID`:

  The identifier for the Census District the region is in (if any).

- `PR_UID`:

  The identifier for the Province the region is in (if unique).

## Examples

``` r
if (FALSE) { # \dontrun{
list_census_regions('CA16')
} # }
```
