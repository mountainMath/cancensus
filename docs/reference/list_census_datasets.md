# Query the CensusMapper API for available datasets.

Query the CensusMapper API for available datasets.

## Usage

``` r
list_census_datasets(use_cache = TRUE, quiet = FALSE)
```

## Arguments

- use_cache:

  If set to TRUE (the default), data will be read from a temporary local
  cache for the duration of the R session, if available. If set to
  FALSE, query the API for the data, and refresh the temporary cache
  with the result.

- quiet:

  When TRUE, suppress messages and warnings.

## Value

Returns a data frame with a column `dataset` containing the code for the
dataset, a column `description` describing it, a `geo_dataset` column
identifying the geography dataset the data is based on, a `attribution`
column with an attribution string, a `reference` column with a reference
identifier, and a `reference_url` column with a link to reference
materials.

## Examples

``` r

# List available datasets in CensusMapper
if (FALSE) { # \dontrun{
list_census_datasets()
} # }
```
