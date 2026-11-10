# Read GeoSuite tables

Reads a table of the Statistics Canada GeoSuite for the given census
year. The GeoSuite has a table for each geographic level, with the codes
and names of the areas, how they relate to other levels, population and
dwelling counts, land areas and representative points, along with code
tables like the census subdivision types. Called without `level` this
lists the tables available for the census year.

The data comes from parquet files hosted at
<https://data.mountainmath.ca/census/geo_suite/>, as Statistics Canada
now blocks programmatic downloads of the GeoSuite. The files are derived
from Statistics Canada data under the Statistics Canada Open Licence.
They get downloaded on first use and cached if the cancensus cache path
has been set, and cached copies only get updated with `refresh = TRUE`.
Reading the tables requires the duckdb and DBI packages.

## Usage

``` r
get_statcan_geo_suite(
  level = NULL,
  census_year = "2021",
  refresh = FALSE,
  language = getOption("cancensus.language", "en"),
  quiet = FALSE
)
```

## Arguments

- level:

  the table to read, e.g. `"DA"`, `"CSD"` or `"CMA_CA"`. `"CMA"`, `"CA"`
  and `"CMACA"` read `"CMA_CA"`, `"LOC"` reads `"PN"` and `"CB"` reads
  `"DB"`. If `NULL` this returns the list of tables for the census year.

- census_year:

  census year to get the data for, one of 1996, 2001, 2006, 2011, 2016
  or 2021. For older censuses
  [`get_statcan_geographic_attributes()`](https://mountainmath.github.io/cancensus/reference/get_statcan_geographic_attributes.md)
  has much of the same information.

- refresh:

  (logical) download the table again if true, and report if it has
  changed

- language:

  language of the column labels and table titles, one of `"en"` or
  `"fr"`. Defaults to the `cancensus.language` option, and to `"en"` if
  that is not set. This does not change the data.

- quiet:

  (logical) suppress messages and the progress bar if `TRUE`. Warnings
  about failed downloads are still shown.

## Value

tibble with the GeoSuite table, with a description of each column in its
`"label"` attribute. The `"metadata"` attribute of the tibble holds the
metadata of the table, including the attribution, licence, source and
notes. If `level` is `NULL`, a tibble with the `table` names, their
`title` and `primary_key` and the `size` of the file in bytes. If the
data can't be downloaded this gives a warning, and returns the cached
data when refreshing existing cached data and `NULL` otherwise.

## Tables and columns

The GeoSuite is available for the censuses from 1996 to 2021. The tables
differ between census years, for example 1996 has enumeration areas
(`EA`) instead of dissemination areas and blocks, 2001 to 2006 have
urban areas (`UA`) and 2011 on population centres (`POPCTR`). Tables
ending in `_P` hold the parts of areas that span provinces or other
areas. `PN` is the place name table, called `LOC` in the source.

Column names follow the attribute files from
[`get_statcan_geographic_attributes()`](https://mountainmath.github.io/cancensus/reference/get_statcan_geographic_attributes.md):
the level, the measure and the census year, e.g. `CSDUID`, `CSDPOP2021`
or `DARPLAT`. The same columns have the same names across census years,
and the tables can be joined to the attribute files without renaming. A
suffix `A` marks counts of the previous census adjusted to the current
boundaries, and `C` or `AC` corrected counts from the count review.
Columns referring to other levels hold the code within the parent level
as in the source, e.g. `CSDCODE` instead of `CSDUID`. Each column has a
description in its `"label"` attribute, and the Statistics Canada field
name in the `source_field` entry of the `columns` metadata.

## Examples

``` r
if (FALSE) { # \dontrun{
# list the tables of the 2021 GeoSuite
get_statcan_geo_suite(census_year = "2021")
# read the dissemination areas
get_statcan_geo_suite("DA", "2021")
} # }
```
