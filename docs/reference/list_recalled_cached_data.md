# List recalled data stored in local cache

Checks the local cached database for recalled data and lists all
recalled cached entries

## Usage

``` r
list_recalled_cached_data(
  cached_data = list_cancensus_cache(),
  warn_only_once = FALSE
)
```

## Arguments

- cached_data:

  List of locally cached data to check for recall, default is
  \`list_cancensus_cache()\` in which case it will get checked against
  all locally cached data

- warn_only_once:

  Will only warn on first run during each session, \`FALSE\` by default

## Value

tibble with rows describing locally cached recalled data

## Examples

``` r
if (FALSE) { # \dontrun{
list_recalled_cached_data()
} # }
```
