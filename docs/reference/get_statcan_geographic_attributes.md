# Read the Geographic Attributes File

Reads the geographic attributes for the given census year, with one row
for each area of the lowest census geography. The table contains the
information on how all the geographic levels are related for each area,
along with population and dwelling counts and representative points.

The data comes from parquet files hosted at
<https://data.mountainmath.ca/census/geo_attributes/>, as Statistics
Canada now blocks programmatic downloads of these files. The files are
derived from Statistics Canada data under the Statistics Canada Open
Licence. They get downloaded on first use and cached if the cancensus
cache path has been set, and cached copies only get updated with
`refresh = TRUE`. Reading the files requires the duckdb and DBI
packages.

A reference guide for the 2021 file is available at
https://www150.statcan.gc.ca/n1/en/catalogue/92-151-G2021001

## Usage

``` r
get_statcan_geographic_attributes(
  census_year = "2021",
  refresh = FALSE,
  language = getOption("cancensus.language", "en"),
  quiet = FALSE
)
```

## Arguments

- census_year:

  census year to get the data for, one of 1981, 1986, 1991, 1996, 2001,
  2006, 2011, 2016 or 2021

- refresh:

  (logical) download the file again if true, and report if it has
  changed

- language:

  language of the column labels, one of `"en"` or `"fr"`. Defaults to
  the `cancensus.language` option, and to `"en"` if that is not set.
  This does not change the data.

- quiet:

  (logical) suppress messages and the progress bar if `TRUE`. Warnings
  about failed downloads are still shown.

## Value

tibble with the attribute data, with a description of each column in its
`"label"` attribute. The `"metadata"` attribute of the tibble holds the
metadata of the file, including the attribution, licence, source and
notes. If the file can't be downloaded this gives a warning, and returns
the cached data when refreshing existing cached data and `NULL`
otherwise.

## Differences between census years

Column names follow the 2021 file, but the available columns differ
between census years. The main change is between 1996 and 2001:

- 2001 to 2021 have one row per dissemination block, identified by
  `DBUID`. There is no 2001 attribute file, the 2001 data was derived
  from the 2001 GeoSuite and has no total dwelling counts or block
  areas.

- 1981 to 1996 have one row per enumeration area, identified by `EAUID`.
  These have different columns, for example `CTCODE` instead of `CTUID`
  and no dissemination areas.

Identifiers are only unique within a census year, the same `DBUID` or
`EAUID` in another year does not generally refer to the same area. In
2001 and 2006 the fields for census metropolitan areas and
agglomerations, census tracts and urban areas were filled outside of
those areas to match the files from 2011 on, so not all of these values
are in the source. The `notes_en` entry of the metadata has details on
this and on any problems with the source of each file.

## Examples

``` r
# read the attributes for the 2021 census
if (FALSE) { # \dontrun{
attributes <- get_statcan_geographic_attributes("2021")
# where the data comes from
attr(attributes, "metadata")$attribution_en
} # }
```
