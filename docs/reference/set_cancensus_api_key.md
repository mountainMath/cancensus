# Set Censusmapper API key

Cancensus requires a free Censusmapper API key to retrieve data. This
function helps set the key for either the duration of the session
(default) or permanently for use across sessions.

## Usage

``` r
set_cancensus_api_key(key, overwrite = FALSE, install = FALSE)
```

## Arguments

- key:

  a Censusmapper API key. For more information on keys see the [API key
  section](https://mountainmath.github.io/cancensus/index.html#api-key)

- overwrite:

  Option to overwrite any existing Censusmapper keys already stored
  locally.

- install:

  Option to install permanently for use across sessions.

## Examples

``` r
if (FALSE) { # \dontrun{
set_cancensus_api_key("YOUR_CM_API_KEY")

# This will set the key permanently until overwritten again
set_cancensus_api_key("YOUR_CM_API_KEY", install = TRUE)
} # }
```
