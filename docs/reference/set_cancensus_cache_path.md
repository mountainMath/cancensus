# Set cancensus cache location

Cancensus caches retrieved data to increase speeds and reduce API usage.
By default the cache only lasts for the R session. This function sets a
cache location that persists across sessions, either for all projects or
for a specific project.

## Usage

``` r
set_cancensus_cache_path(cache_path, overwrite = FALSE, install = FALSE)
```

## Arguments

- cache_path:

  a local directory to use for saving cached data

- overwrite:

  Option to overwrite any existing cache path already stored in
  `.Renviron`, only relevant with `install = TRUE`.

- install:

  Option to install permanently for use across sessions and projects.

## Details

With `install = TRUE` the cache path is added to your `.Renviron` file
and is used across sessions and projects.

With `install = FALSE` the cache path applies to the current R session,
taking precedence over one stored in `.Renviron`. Calling this at the
top of a script gives a project its own cache, for example to back up
the data together with the project for reproducibility. All cache
functions, like
[`list_cancensus_cache()`](https://mountainmath.github.io/cancensus/reference/list_cancensus_cache.md)
and
[`remove_recalled_cached_data()`](https://mountainmath.github.io/cancensus/reference/remove_recalled_cached_data.md),
then work on this project cache.

- Use an absolute path, for example via
  [`here::here()`](https://here.r-lib.org/reference/here.html). Relative
  paths get resolved against the working directory at the time of use,
  which can differ e.g. when knitting a document.

- Set the path before the first call to
  [`get_census()`](https://mountainmath.github.io/cancensus/reference/get_census.md).
  The check of the whole cache for recalled data only runs once per
  session, data read from a cache set later only gets checked as it is
  read.

- Don't set the cache path in a project-level `.Renviron` file unless it
  also has your API key. R reads a project-level `.Renviron` file
  instead of the one in your home directory, not in addition to it.

The cache path is stored in the `CM_CACHE_PATH` environment variable.
The older `cancensus.cache_path` option is still supported, but is only
used if the environment variable is not set.

## Examples

``` r
if (FALSE) { # \dontrun{
# This will set the cache path for the current session, for example for a project-specific cache
set_cancensus_cache_path("~/my_project/cancensus_cache")

# This will set the cache path permanently until overwritten again
set_cancensus_cache_path("~/cancensus_cache", install = TRUE)
} # }
```
