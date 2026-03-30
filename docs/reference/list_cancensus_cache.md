# List cached files

Lists all cached data and metadata if available

## Usage

``` r
list_cancensus_cache()
```

## Value

tibble with metadata on cached data

## Examples

``` r
# list add the cached census data
list_cancensus_cache()
#> # A tibble: 3,794 × 11
#>    path          dataset regions level vectors created_at          version  size
#>    <chr>         <chr>   <chr>   <chr> <chr>   <dttm>              <chr>   <dbl>
#>  1 CM_data_0008… CA16    "{\"CS… Regi… "[\"v_… 2023-06-14 15:39:12 d.4       874
#>  2 CM_data_0017… CA16    "{\"CS… Regi… "[\"v_… 2023-05-24 18:34:54 d.4       516
#>  3 CM_data_0023… CA11    "{\"CS… Regi… "[\"v_… 2023-05-16 21:46:25 d.4       485
#>  4 CM_data_0039… CA21    "{\"CS… Regi… "[\"v_… 2023-05-24 19:32:51 d.4       533
#>  5 CM_data_003f… CA16    "{\"C\… Regi… "[\"v_… 2024-08-19 21:17:18 d.4      1902
#>  6 CM_data_0054… CA21    "{\"CS… DA    "[\"v_… 2023-08-28 15:19:19 d.4       620
#>  7 CM_data_0056… CA16    "{\"CS… Regi… "[\"v_… 2023-05-24 18:32:15 d.4       525
#>  8 CM_data_006c… CA21    "{\"CS… Regi… "[\"v_… 2023-05-24 18:29:31 d.4       529
#>  9 CM_data_007f… CA21    "{\"CM… Regi… "[]"    2023-01-26 07:18:01 d.4      1882
#> 10 CM_data_0084… CA16    "{\"CS… Regi… "[\"v_… 2023-06-14 15:32:37 d.4       707
#> # ℹ 3,784 more rows
#> # ℹ 3 more variables: last_accessed <dttm>, access_count <dbl>,
#> #   resolution <chr>
```
