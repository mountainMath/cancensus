# List all child variables from vector hierarchies given either a list of Census variables returned by `list_census_vectors`, `search_census_vectors`, `find_census_vectors`, or a direct string reference to the vector code.

List all child variables from vector hierarchies given either a list of
Census variables returned by `list_census_vectors`,
`search_census_vectors`, `find_census_vectors`, or a direct string
reference to the vector code.

## Usage

``` r
child_census_vectors(
  vector_list,
  leaves_only = FALSE,
  max_level = NA,
  keep_parent = FALSE
)
```

## Arguments

- vector_list:

  the list of vectors to be used, either a character vector or a
  filtered tibble as returned from `list_census_vectors`.

- leaves_only:

  boolean flag to indicate if only final leaf vectors should be
  returned, i.e. terminal vectors that themselves do not have children.

- max_level:

  optional, maximum depth to look for child vectors. Default is `NA` and
  will return all child census vectors.

- keep_parent:

  optional, also return parent vector in list of results. Default is set
  to `FALSE`.

## Examples

``` r
# Query parent vectors directly using vector identifier
child_census_vectors("v_CA16_2510")
#> # A tibble: 4 × 7
#>   vector      type  label             units  parent_vector aggregation details  
#>   <chr>       <fct> <chr>             <fct>  <chr>         <chr>       <chr>    
#> 1 v_CA16_2513 Total 0 to 17 years     Number v_CA16_2510   Additive    CA 2016 …
#> 2 v_CA16_2519 Total 18 to 64 years    Number v_CA16_2510   Additive    CA 2016 …
#> 3 v_CA16_2522 Total 65 years and over Number v_CA16_2510   Additive    CA 2016 …
#> 4 v_CA16_2516 Total 0 to 5 years      Number v_CA16_2513   Additive    CA 2016 …

if (FALSE) { # \dontrun{

# Example using multiple vectors coerced into a list
child_census_vectors(c("v_CA16_2510","v_CA16_2511","v_CA16_2512"))

# or, equivalently
selected_vectors <- c("v_CA16_2510","v_CA16_2511","v_CA16_2512")
child_census_vectors(selected_vectors)

# Example using dplyr and piped arguments
library(dplyr, warn.conflicts = FALSE)

list_census_vectors("CA16") %>%
  filter(vector == "v_CA16_2510") %>%
  child_census_vectors(TRUE)

# this will return the equivalent of c("v_CA16_2510", child_census_vectors("v_CA16_2510"))
list_census_vectors("CA16") %>%
  filter(vector == "v_CA16_2510") %>%
  child_census_vectors(TRUE, keep_parent = TRUE)
} # }
```
