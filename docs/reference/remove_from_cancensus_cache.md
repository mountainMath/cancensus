# Remove cached files

Remove cached data for paths given

## Usage

``` r
remove_from_cancensus_cache(paths)
```

## Arguments

- paths:

  list of paths to remove, as returned by the path column in
  \`list_cancensus_cache\`

## Value

freed-up disk space

## Examples

``` r
if (FALSE) { # \dontrun{
# remove the first cached dataset
cache_data <- list_cancensus_cache()

remove_from_cancensus_cache(cache_data$path[1])
} # }
```
