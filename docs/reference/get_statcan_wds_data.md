# Query the StatCan WDS for data

Get official census data from Statistics Canada for a given set of
DGUIDs. Only available for the 2021 census. The downloaded data gets
enriched by geographic and characteristic names based on metadata
obtained via \`get_statcan_wds_metadata()\`. Data is cached for the
duration of the R session.

## Usage

``` r
get_statcan_wds_data(
  DGUIDs,
  members = NULL,
  version = NULL,
  gender = "All",
  language = "en",
  refresh = FALSE
)
```

## Arguments

- DGUIDs:

  census year to get the data for, right now only 2021 is supported.
  Valid DGUIDs for a given geographic level can be queried via
  \`get_statcan_wds_metadata()\`.

- members:

  list of Member IOs to download data for. By default all
  characteristics are downloaded. Valid Member IDs and their
  descriptions can be queried via the \`get_statcan_wds_metadata()\`
  call.

- version:

  optionally specify a version of the data to download. For example, for
  FED level data, version 1.3 will access the 2013 represenation order,
  whereas version 2.0 will access the 2023 representation order. By
  default the latest available version is accessed.

- gender:

  optionally query data for only one gender. By default this queries
  data for all genders, possible values are "Total", "Male", "Female" to
  only query total data, or for males only or for females only.

- language:

  specify language for geography and characteristic names that get
  added, valid choices are "en" and "fr"

- refresh:

  default is \`FALSE\` will refresh the temporary cache if \`TRUE\`

## Value

tibble with the enriched census data

## Examples

``` r
# get data for federal electoral district 2013A000459021
if (FALSE) { # \dontrun{
get_statcan_wds_data(DGUIDs="2013A000459021",level="FED")
} # }
```
