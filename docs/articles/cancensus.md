# cancensus

## Cancensus and CensusMapper

The **cancensus** package was developed to provide users with a way to
access Canadian Census in a programmatic way following good [tidy
data](http://vita.had.co.nz/papers/tidy-data.pdf) practices. While the
structure and data in **cancensus** is unique to Canadian Census data,
this package is inspired in part by
[tidycensus](https://github.com/walkerke/tidycensus), a package to
interface with the US Census Bureau data APIs.

As Statistics Canada does not provide direct API access to Census data,
**cancensus** retrieves Census data indirectly through the
[CensusMapper](https://censusmapper.ca) API. CensusMapper is a project
by [Jens von Bergmann](https://github.com/mountainMath), one of the
authors of **cancensus**, to provide interactive geographic
visualizations of Canadian Census data. CensusMapper databases store all
publicly available data from Statistics Canada for the 2006, 2011, and
2016 Censuses. CensusMapper data can be accessed via an API and
**cancensus** is built to interface directly with it.

### API Key

**cancensus** requires a valid CensusMapper API key to use. You can
obtain a free API key by [signing
up](https://censusmapper.ca/users/sign_up) for a CensusMapper account.
CensusMapper API keys are free and public API quotas are generous;
however, due to incremental costs of serving large quantities of data,
there limits to API usage in place. For most use cases, these API limits
should not be an issue. Production uses with large extracts of fine
grained geographies may run into API quota limits. For larger quotas,
please get in touch with Jens [directly](mailto:jens@censusmapper.ca).

To check your API key, just go to “Edit Profile” (in the top-right of
the CensusMapper menu bar). Once you have your key, you can store it in
your system environment so it is automatically used in API calls. To do
so just enter `set_cancensus_api_key(<your_api_key>, install = TRUE)`

### Installing cancensus

The stable version of **cancensus** can be easily installed from CRAN.

``` r

install.packages("cancensus")

library(cancensus)
```

Alternatively, the latest development version can be installed from
Github using \`remotes\`\`.

``` r

# install.packages("devtools")
remotes::install_github("mountainmath/cancensus")

library(cancensus)
```

If you have not already done so, you can install the API keys and the
data cache path. You can get your free API key when you sign up for a
[CensusMapper account](https://censusmapper.ca/) and check your profile.
Additionally we recommend you set a permanent data cache path so that
census data you query is stored persistently across sessions. **Make
sure to replace the filler text `<your_api_key>` and
`<local cache path>` with your actual API key and local cache path.**

``` r

# Only need to install api key can cache path once
set_cancensus_api_key('<your_api_key>', install = TRUE)
set_cancensus_cache_path('<local cache path>', install = TRUE)
```

Data in the persistent cache can be managed via the functions
`list_cancensus_cache` and `remove_from_cancensus_cache` if needed.

## Accessing Census Data

**cancensus** provides three different functions for retrieving Census
data: \* `get_census` to retrieve Census data and geography as a spatial
dataset \* `get_census_data` to retrieve Census data only as a flat data
frame \* `get_census_geometry` to retrieve Census geography only as a
collection of spatial polygons.

`get_census` takes as inputs a dataset parameter, a list of specified
regions, a vector of Census variables, and a Census geography level. You
can specify one of three options for spatial formats: `NA` to return
data only, `sf` to return an sf-class data frame, or `sp` to return a
SpatialPolygonsDataFrame object.

``` r

# Returns a data frame with data only
census_data <- get_census(dataset='CA21', regions=list(CMA="59933"),
                          vectors=c("v_CA21_434","v_CA21_435","v_CA21_440"),
                          level='CSD', use_cache = FALSE, geo_format = NA, quiet = TRUE)

# Returns data and geography as an sf-class data frame
census_data <- get_census(dataset='CA21', regions=list(CMA="59933"),
                          vectors=c("v_CA21_434","v_CA21_435","v_CA21_440"),
                          level='CSD', use_cache = FALSE, geo_format = 'sf', quiet = TRUE)

# Returns a SpatialPolygonsDataFrame object with data and geography
census_data <- get_census(dataset='CA21', regions=list(CMA="59933"),
                          vectors=c("v_CA21_434","v_CA21_435","v_CA21_440"),
                          level='CSD', use_cache = FALSE, geo_format = 'sp', quiet = TRUE)
```

**cancensus** utilizes caching to increase speed, minimize API token
usage, and to make data available offline. Downloaded data is hashed and
stored locally so if a call is made to access the same data,
**cancensus** will read the local version instead. To force
**cancensus** to refresh the data, specify `use_cache = FALSE` as a
parameter for `get_census`.

Additional parameters for advanced options can be viewed by running
[`?get_census`](https://mountainmath.github.io/cancensus/reference/get_census.md).

### Census Datasets

**cancensus** can access Statistics Canada Census data for Census years
1996, 2001, 2006, 2011, 2016, and 2021. You can run
`list_census_datasets` to check what datasets are currently available
for access through the CensusMapper API.

Thanks to contributions by the Canada Mortgage and Housing Corporation
(CMHC), **cancensus** now includes additional Census-linked datasets as
open-data releases. These include annual taxfiler data at the census
tract level for tax years 2000 through 2017, which includes data on
incomes and demographics, as well as specialized crosstabs for
Structural type of dwelling by Document type, which details occupancy
status for residences. These crosstabs are available for the 2001, 2006,
2011, and 2016 Census years at all levels starting with census tract.

The function
[`list_census_datasets()`](https://mountainmath.github.io/cancensus/reference/list_census_datasets.md)
will show all available datasets alongside their metadata.

``` r

list_census_datasets()
#> # A tibble: 29 × 6
#>    dataset description           geo_dataset attribution reference reference_url
#>    <chr>   <chr>                 <chr>       <chr>       <chr>     <chr>        
#>  1 CA1996  1996 Canada Census    CA1996      StatCan 19… 92-351-U  https://www1…
#>  2 CA01    2001 Canada Census    CA01        StatCan 20… 92-378-X  https://www1…
#>  3 CA06    2006 Canada Census    CA06        StatCan 20… 92-566-X  https://www1…
#>  4 CA11    2011 Canada Census a… CA11        StatCan 20… 98-301-X… https://www1…
#>  5 CA16    2016 Canada Census    CA16        StatCan 20… 98-301-X  https://www1…
#>  6 CA21    2021 Canada Census    CA21        StatCan 20… 98-301-X  https://www1…
#>  7 CA01xSD 2001 Canada Census x… CA01        StatCan 20… 92-378-X  https://www1…
#>  8 CA06xSD 2006 Canada Census x… CA06        StatCan 20… 92-566-X  https://www1…
#>  9 CA11xSD 2011 Canada Census x… CA11        StatCan 20… 98-301-X  https://www1…
#> 10 CA16xSD 2016 Canada Census x… CA16        StatCan 20… 98-301-X  https://www1…
#> # ℹ 19 more rows
```

As other Census datasets become available via the CensusMapper API, they
will be listed as output when calling
[`list_census_datasets()`](https://mountainmath.github.io/cancensus/reference/list_census_datasets.md).

### Census Regions

Census data is aggregated at multiple [geographic
levels](#id_##%20Census%20Geographic%20Levels). Census geographies at
the national (C), provincial (PR), census metropolitan area (CMA),
census agglomeration (CA), census division (CD), and census subdivision
(CSD) are defined as named census regions.

Canadian Census geography can change in between Census periods.
**cancensus** provides a function, `list_census_regions(dataset)`, to
display all named census regions and their corresponding id for a given
census dataset.

``` r

list_census_regions("CA21")
#> # A tibble: 5,518 × 8
#>    region name               level    pop municipal_status CMA_UID CD_UID PR_UID
#>    <chr>  <chr>              <chr>  <int> <chr>            <chr>   <chr>  <chr> 
#>  1 01     Canada             C     3.70e7 NA               NA      NA     NA    
#>  2 35     Ontario            PR    1.42e7 Ont.             NA      NA     NA    
#>  3 24     Quebec             PR    8.50e6 Que.             NA      NA     NA    
#>  4 59     British Columbia   PR    5.00e6 B.C.             NA      NA     NA    
#>  5 48     Alberta            PR    4.26e6 Alta.            NA      NA     NA    
#>  6 46     Manitoba           PR    1.34e6 Man.             NA      NA     NA    
#>  7 47     Saskatchewan       PR    1.13e6 Sask.            NA      NA     NA    
#>  8 12     Nova Scotia        PR    9.69e5 N.S.             NA      NA     NA    
#>  9 13     New Brunswick      PR    7.76e5 N.B.             NA      NA     NA    
#> 10 10     Newfoundland and … PR    5.11e5 N.L.             NA      NA     NA    
#> # ℹ 5,508 more rows
```

The `regions` parameter in `get_census` requires as input a list of
region id strings that correspond to that regions geoid. You can combine
different regions together into region lists.

``` r

# Retrieves Vancouver and Toronto
list_census_regions('CA21') %>% 
  filter(level == "CMA", name %in% c("Vancouver","Toronto"))
#> # A tibble: 2 × 8
#>   region name      level     pop municipal_status CMA_UID CD_UID PR_UID
#>   <chr>  <chr>     <chr>   <int> <chr>            <chr>   <chr>  <chr> 
#> 1 35535  Toronto   CMA   6202225 B                NA      NA     35    
#> 2 59933  Vancouver CMA   2642825 B                NA      NA     59

census_data <- get_census(dataset='CA21', regions=list(CMA=c("59933","35535")),
                          vectors=c("v_CA21_434","v_CA21_435","v_CA21_440"),
                          level='CSD', use_cache = FALSE, quiet = TRUE)
```

### Census Geographic Levels

Census data accessible through **cancensus** comes is available in a
number of different aggregation levels including:

| Code    | Description                     | Count in Census 2016 |
|---------|---------------------------------|:--------------------:|
| C       | Canada (total)                  |          1           |
| PR      | Provinces/Territories           |          13          |
| CMA     | Census Metropolitan Area        |          35          |
| CA      | Census Agglomeration            |          14          |
| CD      | Census Division                 |         287          |
| CSD     | Census Subdivision              |         713          |
| CT      | Census Tracts                   |         5621         |
| DA      | Dissemination Area              |        56589         |
| EA      | Enumeration Area (1996 only)    |          \-          |
| DB      | Dissemination Block (2001-2016) |        489676        |
| Regions | Named Census Region             |                      |

Selecting `regions = "59933"` and `level = "CT"` will return data for
all 478 census tracts in the Vancouver Census Metropolitan Area.
Selecting `level = "DA"` will return data for all 3450 dissemination
areas and selecting `level = "DB"` will retrieve data for 15,197
dissemination block. Working with CT, DA, EA, and especially DB level
data significantly increases the size of data downloads and API usage.
To help minimize additional overhead, **cancensus** supports local data
caching to reduce usage and load times.

Setting `level = "Regions"` will produce data strictly for the selected
region without any tiling of data at lower census aggregation levels
levels.

## Working with Census Variables

Census data contains thousands of different geographic regions as well
as thousands of unique variables. In addition to enabling programmatic
and reproducible access to Census data, **cancensus** has a number of
tools to help users find the data they are looking for.

### Displaying available Census variables

Run `list_census_vectors(dataset)` to view all available Census
variables for a given dataset.

``` r

list_census_vectors("CA21")
#> # A tibble: 7,709 × 7
#>    vector    type   label                units parent_vector aggregation details
#>    <chr>     <fct>  <chr>                <fct> <chr>         <chr>       <chr>  
#>  1 v_CA21_1  Total  Population, 2021     Numb… NA            Additive    CA 202…
#>  2 v_CA21_2  Total  Population, 2016     Numb… NA            Additive    CA 202…
#>  3 v_CA21_3  Total  Population percenta… Numb… NA            Average of… CA 202…
#>  4 v_CA21_4  Total  Total private dwell… Numb… NA            Additive    CA 202…
#>  5 v_CA21_5  Total  Private dwellings o… Numb… v_CA21_4      Additive    CA 202…
#>  6 v_CA21_6  Total  Population density … Ratio NA            Average of… CA 202…
#>  7 v_CA21_7  Total  Land area in square… Numb… NA            Additive    CA 202…
#>  8 v_CA21_8  Total  Total - Age          Numb… NA            Additive    CA 202…
#>  9 v_CA21_9  Male   Total - Age          Numb… NA            Additive    CA 202…
#> 10 v_CA21_10 Female Total - Age          Numb… NA            Additive    CA 202…
#> # ℹ 7,699 more rows
```

### Variable characteristics

For each variable (vector) in that Census dataset, this shows:

- Vector: short variable code
- Type: variables are provided as aggregates of female responses, male
  responses, or total (male+female) responses
- Label: detailed variable name
- Units: provides information about whether the variable represents a
  count integer, a ratio, a percentage, or a currency figure
- Parent_vector: shows the immediate hierarchical parent category for
  that variable, where appropriate
- Aggregation: indicates how the variable should be aggregated with
  others, whether it is additive or if it is an average of another
  variable
- Description: a rough description of a variable based on its
  hierarchical structure. This is constructed by **cancensus** by
  recursively traversing the labels for every variable’s hierarchy, and
  facilitates searching for specific variables using key terms.

### Variable search

Each Census dataset features numerous variables making it a bit of a
challenge to find the exact variable you are looking for. There is a
function,
[`find_census_vectors()`](https://mountainmath.github.io/cancensus/reference/find_census_vectors.md),
for searching through Census variable metadata in a few different ways.
There are three types of searches possible using this function: exact
search, which simply looks for exact string matches for a given query
against the vector dataset; keyword search, which breaks vector metadata
into unigram tokens and then tries to find the vectors with the greatest
number of unique matches; and, semantic search which works better with
search phrases and has tolerance for inexact searches. Switching between
search modes is done using the `query_type` argument when calling
`find_census_variables()` function.

``` r

# Find the variable indicating the number of people of Austrian ethnic origin
find_census_vectors("Australia", dataset = "CA16", type = "total", query_type = "exact")
#> # A tibble: 2 × 4
#>   vector      type  label      details                                          
#>   <chr>       <fct> <chr>      <chr>                                            
#> 1 v_CA16_3813 Total Australia  25% Data; Citizenship and Immigration; Total - S…
#> 2 v_CA16_4809 Total Australian 25% Data; Minority / Origin; Total - Ethnic orig…

find_census_vectors("Australia origin", dataset = "CA16", type = "total", query_type = "semantic")
#> # A tibble: 1 × 4
#>   vector      type  label      details                                          
#>   <chr>       <fct> <chr>      <chr>                                            
#> 1 v_CA16_4809 Total Australian 25% Data; Minority / Origin; Total - Ethnic orig…

find_census_vectors("Australian ethnic", dataset = "CA16", type = "total", query_type = "keyword", interactive = FALSE)
#> # A tibble: 1 × 4
#>   vector      type  label      details                                          
#>   <chr>       <fct> <chr>      <chr>                                            
#> 1 v_CA16_4809 Total Australian 25% Data; Minority / Origin; Total - Ethnic orig…
```

### Managing variable hierarchy

Census variables are frequently hierarchical. As an example, consider
the variable for the number of people of Austrian ethnic background. We
can select that vector and quickly look up its entire hierarchy using
`parent_census_vectors` on a vector list.

``` r

list_census_vectors("CA16") %>% 
  filter(vector == "v_CA16_4092") %>% 
  parent_census_vectors()
#> # A tibble: 3 × 7
#>   vector      type  label                units parent_vector aggregation details
#>   <chr>       <fct> <chr>                <fct> <chr>         <chr>       <chr>  
#> 1 v_CA16_4089 Total Western European or… Numb… v_CA16_4044   Additive    CA 201…
#> 2 v_CA16_4044 Total European origins     Numb… v_CA16_3999   Additive    CA 201…
#> 3 v_CA16_3999 Total Total - Ethnic orig… Numb… NA            Additive    CA 201…
```

Sometimes we want to traverse the hierarchy in the opposite direction.
This is frequently required when looking to compare different variable
stems that share the same aggregate variable. As an example, if we want
to look the total count of Northern European ethnic origin respondents
disaggregated by individual countries, it is pretty easy to do so.

``` r

# Find the variable indicating the Northern European aggregate
find_census_vectors("Northern European", dataset = "CA16", type = "Total")
#> # A tibble: 7 × 4
#>   vector      type  label                                                details
#>   <chr>       <fct> <chr>                                                <chr>  
#> 1 v_CA16_4122 Total Northern European origins (except British Isles ori… 25% Da…
#> 2 v_CA16_4125 Total Danish                                               25% Da…
#> 3 v_CA16_4128 Total Finnish                                              25% Da…
#> 4 v_CA16_4131 Total Icelandic                                            25% Da…
#> 5 v_CA16_4134 Total Norwegian                                            25% Da…
#> 6 v_CA16_4137 Total Swedish                                              25% Da…
#> 7 v_CA16_4140 Total Northern European origins, n.i.e.                    25% Da…
```

The search result shows that the vector **v_CA16_4092** represents the
aggregate for all Northern European origins. The `child_census_vectors`
function can return a list of its constituent underlying variables.

``` r

# Show all child variable leaves
list_census_vectors("CA16") %>% 
  filter(vector == "v_CA16_4122") %>% child_census_vectors(leaves = TRUE)
#> # A tibble: 6 × 7
#>   vector      type  label                units parent_vector aggregation details
#>   <chr>       <fct> <chr>                <fct> <chr>         <chr>       <chr>  
#> 1 v_CA16_4125 Total Danish               Numb… v_CA16_4122   Additive    CA 201…
#> 2 v_CA16_4128 Total Finnish              Numb… v_CA16_4122   Additive    CA 201…
#> 3 v_CA16_4131 Total Icelandic            Numb… v_CA16_4122   Additive    CA 201…
#> 4 v_CA16_4134 Total Norwegian            Numb… v_CA16_4122   Additive    CA 201…
#> 5 v_CA16_4137 Total Swedish              Numb… v_CA16_4122   Additive    CA 201…
#> 6 v_CA16_4140 Total Northern European o… Numb… v_CA16_4122   Additive    CA 201…
```

The `leaves = TRUE` parameter specifies whether intermediate aggregates
are included or not. If `TRUE` then only the lowest level variables are
returns - the “leaves” of the hierarchical tree.
