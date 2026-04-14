# Return Census variable names and labels as a tidy data frame (Deprecated)

Return Census variable names and labels as a tidy data frame
(Deprecated)

## Usage

``` r
census_vectors(x)
```

## Arguments

- x:

  A data frame, `sp` or `sf` object returned from `get_census` or
  similar.

## Value

A data frame with a column `variable` containing the truncated variable
name, and a column `label` describing it.

## Examples

``` r
if (FALSE) { # \dontrun{
# Query census data with truncated labels:
census_data <- get_census(dataset='CA16', regions=list(CMA="59933"),
                          vectors=c("v_CA16_408","v_CA16_409","v_CA16_410"),
                          level='CSD', geo_format = "sf", labels="short")

# Get details for truncated vectors:
census_vectors(census_data)
} # }
```
