# Convenience function for creating unique names from region list

Names of municipalities are not always unique, especially at the CSD
level. This function takes as input a subset of a regions list as
generated from \`list_census_regions()\` and de-duplicates names as
needed by adding the municipal status in parenthesis. If this does not
de-duplicate the name then the geographic identifier will be further
added in parenthesis behind that.

## Usage

``` r
add_unique_names_to_region_list(region_list)
```

## Arguments

- region_list:

  a subset of a regions list as gotten from \`list_census_regions()\`

## Value

The same list of regions with an extra column \`Name\` with
de-duplicated names.

## Examples

``` r
if (FALSE) { # \dontrun{
# This will return a warning that no match was found, but will suggest similar named regions.
library(dplyr)
list_census_regions("CA21") %>%
  filter(level=="CSD", CMA_UID=="59933") %>%
  add_unique_names_to_region_list()
} # }
```
