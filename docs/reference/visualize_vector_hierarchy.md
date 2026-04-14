# Visualize Census vector hierarchies as ASCII tree

Displays an ASCII tree representation of the hierarchical structure of
Census vectors. This helps users understand the relationship between
parent and child vectors when selecting variables for their analysis.

## Usage

``` r
visualize_vector_hierarchy(
  vector,
  dataset = NULL,
  max_depth = NA,
  show_type = FALSE,
  quiet = FALSE
)
```

## Arguments

- vector:

  A Census vector code (e.g., "v_CA16_2510") or a filtered tibble as
  returned from `list_census_vectors`.

- dataset:

  The dataset to query for vector information, e.g. `"CA16"`. Only
  required if `vector` is a character string.

- max_depth:

  Maximum depth of the tree to display. Default is `NA` which shows the
  entire hierarchy.

- show_type:

  Logical. If `TRUE`, shows the type (Total/Male/Female) next to each
  vector. Default is `FALSE`.

- quiet:

  When `TRUE`, suppress messages. Default is `FALSE`.

## Value

Invisibly returns a tibble of the vectors displayed in the tree. The
tree is printed to the console as a side effect.

## Examples

``` r
if (FALSE) { # \dontrun{
# Visualize the age hierarchy for 2016 Census
visualize_vector_hierarchy("v_CA16_2510", dataset = "CA16")

# Show only first two levels with type information
visualize_vector_hierarchy("v_CA16_2510", dataset = "CA16",
                           max_depth = 2, show_type = TRUE)

# Using a vector tibble from list_census_vectors
library(dplyr)
list_census_vectors("CA16") %>%
  filter(vector == "v_CA16_2510") %>%
  visualize_vector_hierarchy()
} # }
```
