# Set persistent cancensus cache location

Cancensus provides session caching for retrieved data to increase speeds
and reduce API usage. This function will create a persistent cache
across sessions.

## Usage

``` r
set_cancensus_cache_path(cache_path, overwrite = FALSE, install = FALSE)
```

## Arguments

- cache_path:

  a local directory to use for saving cached data

- overwrite:

  Option to overwrite any existing cache path already stored locally.

- install:

  Option to install permanently for use across sessions.

## Examples

``` r
if (FALSE) { # \dontrun{
set_cancensus_cache_path("~/cancensus_cache")

# This will set the cache path permanently until overwritten again
set_cancensus_cache_path("~/cancensus_cache", install = TRUE)
} # }
```
