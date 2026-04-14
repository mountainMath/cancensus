# List all parent variables from vector hierarchies given either a list of Census variables returned by `list_census_vectors`, `search_census_vectors`, `find_census_vectors`, or a direct string reference to the vector code.

List all parent variables from vector hierarchies given either a list of
Census variables returned by `list_census_vectors`,
`search_census_vectors`, `find_census_vectors`, or a direct string
reference to the vector code.

## Usage

``` r
parent_census_vectors(vector_list)
```

## Arguments

- vector_list:

  The list of vectors to be used, either a character vector or a
  filtered tibble as returned from `list_census_vectors`.

## Examples

``` r
# Query parent vectors directly using vector identifier
parent_census_vectors("v_CA16_2519")
#> # A tibble: 1 × 7
#>   vector      type  label                units parent_vector aggregation details
#>   <chr>       <fct> <chr>                <fct> <chr>         <chr>       <chr>  
#> 1 v_CA16_2510 Total Total - Low-income … Numb… NA            Additive    CA 201…
if (FALSE) { # \dontrun{
# Example using multiple vectors coerced into a list
parent_census_vectors(c("v_CA16_2519","v_CA16_2520","v_CA16_2521"))

# or, equivalently
selected_vectors <- c("v_CA16_2519","v_CA16_2520","v_CA16_2521")
parent_census_vectors(selected_vectors)

# Example using dplyr and piped arguments
library(dplyr, warn.conflicts = FALSE)

list_census_vectors("CA16") %>%
  filter(vector == "v_CA16_2519") %>%
  parent_census_vectors()
} # }
```
