# Get metadata for recalled data

Grabs recall data from server if needed and returns list of recalled
data

## Usage

``` r
get_recalled_database(refresh = FALSE, warn_only_once = FALSE)
```

## Arguments

- refresh:

  Will force refresh of recalled database

- warn_only_once:

  Will only warn on first run, \`FALSE\` by default

## Value

tibble with rows describing recalled data

## Examples

``` r
if (FALSE) { # \dontrun{
get_recalled_database()
} # }
```
