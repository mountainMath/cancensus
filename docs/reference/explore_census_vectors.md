# Interactively browse Census variables and regions on Censusmapper.ca in a new browser window

Finding the right Census variables or regions can be complicated.
`explore_census_vectors(dataset)` and `explore_census_regions(dataset)`
will open a new browser page or tab to an interactive Census variable
and region exploration and selection tool on the [Censusmapper.ca
website](https://censusmapper.ca/api). Interactive tools available for
the CA16, CA11, CA06, and CA01 Census datasets and geographies.

## Usage

``` r
explore_census_vectors(dataset = "CA16")
```

## Arguments

- dataset:

  The dataset to query for available vectors, e.g. `'CA16'`. Interactive
  tools available for the CA16, CA11, CA06, and CA01 Census datasets and
  geographies.

## Examples

``` r
if (FALSE) { # \dontrun{

explore_census_vectors(dataset = "CA16")

explore_census_regions(dataset = "CA11")

} # }
```
