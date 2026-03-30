# Query the StatCan WDS for metadata

Get official metadata information from Statistics Canada for a given
geographic level. Only available for the 2021 census. Data is cached for
the duration of the R session.

## Usage

``` r
get_statcan_wds_metadata(census_year, level, version = NULL, refresh = FALSE)
```

## Arguments

- census_year:

  census year to get the data for, right now only 2021 is supported

- level:

  geographic level to return the data for, valid choices are
  "PR","CD","CMACA","CSD","CT","ADA","DA","ER","FED","DPL","POPCNTR",
  "FSA", "HR"

- version:

  optionally specify a version of the data to download. For example, for
  FED level data, version 1.3 will access the 2013 represenation order,
  whereas version 2.0 will access the 2023 representation order. By
  default the latest available version is accessed.

- refresh:

  default is \`FALSE\` will refresh the temporary cache if \`TRUE\`

## Value

tibble with the metadata

## Examples

``` r
# get metadata for federal electoral districts
if (FALSE) { # \dontrun{
get_statcan_wds_metadata(census_year="2021",level="FED")
} # }
```
