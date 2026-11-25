# Read geographic boundary data from StatCan

Reads the original unprocessed geographic boundary files from Statistics
Canada

## Usage

``` r
get_statcan_geographies(
  census_year,
  level,
  type = "cartographic",
  cache_path = NULL,
  timeout = 1000,
  refresh = FALSE,
  quiet = FALSE,
  precision = "detailed"
)
```

## Arguments

- census_year:

  census year to get the data for, "2021" or "2026". Not all 2026
  boundary files are available yet, Statistics Canada plans to release
  those for "CMACA", "CT", "POPCNTR" and "DPL" on February 10, 2027, and
  for "FSA" on September 28, 2027.

- level:

  geographic level to return the data for, valid choices are
  "PR","CD","CMACA","CSD","CT","ADA","DA","DB","ER","FED","DPL","POPCNTR",
  "FSA", "HR", and for 2026 also "CAR" (census agricultural regions) and
  "CCS" (census consolidated subdivisions)

- type:

  type of geographic data, valid choices area "cartographic" or
  "digital"

- cache_path:

  optional path to cache the data. If the cancensus cache path is set
  the geographic data gets cached in the "geographies" subdirectory of
  the cancensus cache path.

- timeout:

  optional timeout parameter, adjust as needed if the data download
  times out when using slow connections

- refresh:

  (logical) refresh the cache if true

- quiet:

  (logical) suppress messages if \`TRUE\`

- precision:

  precision of cartographic boundary files, "detailed" (the default) or
  "generalized". Generalized boundary files are new with the 2026 census
  and are only available for \`type = "cartographic"\`. They are
  simplified for mapping at smaller scales while keeping the shape of
  and the spatial relationships between the geographic areas.

## Value

a spatial dataframe with the geographic data. Statistics Canada blocks
plain downloads of these files with a browser verification. If the
safeconduct package is installed and finds Google Chrome or another
Chromium-based browser, the download goes through that browser, set
\`options(cancensus.use_browser = FALSE)\` to turn this off. Otherwise,
or if that fails, this gives a warning explaining how to download the
file manually, and returns the cached data when refreshing existing
cached data and \`NULL\` otherwise

## Examples

``` r
# get the digital geographic boundaries for provinces and territories
if (FALSE) { # \dontrun{
get_statcan_geographies(census_year="2021",level="PR",type="digital")

# generalized cartographic boundaries of 2026 census divisions
get_statcan_geographies(census_year="2026",level="CD",precision="generalized")
} # }
```
