# cancensus 0.6.2

## Breaking changes

- `get_statcan_geographic_attributes()` now reads parquet files hosted at <https://data.mountainmath.ca/census/geo_attributes/> instead of downloading from Statistics Canada, which blocks programmatic downloads of these files (#228, #233). This requires the **duckdb** and **DBI** packages. Column names are now simpler and follow one convention across census years, so code using the previous names needs updating, e.g. `CTCODE_SRCODE` is now `CTCODE` for 2021, and `DBuid` and `DAlat` are now `DBUID` and `DARPLAT` for 2006 and 2011. To make up for the simpler names, columns now have labels describing them in the language of choice, set by the new `language` parameter. Counts are now integers and coordinates are numeric. Data gets downloaded again once. The previous cache in the `attribute_files` folder of the cache directory and any copies of these files in the `statcan_downloads` folder are no longer used, and the function shows a command to delete them once per session

## Improvements

- `get_statcan_geographic_attributes()` now covers the 1981, 1986, 1991, 1996 and 2001 censuses (#233). 1981 to 1996 are at the enumeration area level, 2001 was derived from the 2001 GeoSuite. Columns have labels in the `"label"` attribute, in the language given by the new `language` parameter, and the `"metadata"` attribute of the result has the attribution, licence, source and notes on each file. With `refresh = TRUE` it reports whether the hosted file has changed
- French labels (#229): `get_census()`, `list_census_vectors()`, `parent_census_vectors()`, `child_census_vectors()`, `find_census_vectors()`, `visualize_vector_hierarchy()` and `dataset_attribution()` gain a `language` parameter, one of `"en"` or `"fr"`. It defaults to the new `cancensus.language` option and otherwise to `"en"`, so `options(cancensus.language = "fr")` switches the language for a session. Column layouts are the same in both languages, and in `get_census()` changing the language does not download the data again or use up API quota. Vectors without French labels, currently the tax and cross-tabulation datasets, fall back to English with a warning once per session and dataset. Region names and the `type`, `units` and `aggregation` columns of `list_census_vectors()` stay as they are
- `get_census()` gains `labels = "labelled"` (#230), which names columns by vector like `labels = "short"` and stores the label of each variable in the `"label"` attribute of its column. The other columns get short names, like `Name`, `Area`, `Quality` and `Population_Previous`, that are the same across datasets and for data and geometry requests, along with the same types and column order. Columns that data and geometry requests both return, like the region name and area, only appear once. These columns also get labels in the requested language, including the census year where that applies, e.g. `Population` is labelled `Population 2021` for CA21. Tax and cross-tabulation datasets use the year of the census they take these columns from, e.g. `Population 2016` for TX2018. Columns named by naming `vectors` get the label of their variable, and if that name is already used by another column, that column gets a trailing underscore, e.g. `Population_` for `vectors = c(Population = "v_CA21_1")`. `labels = "detailed"` and `labels = "short"` do not change
- The default of the `labels` parameter of `get_census()` is planned to change to `"labelled"` in a future version. It now defaults to the new `cancensus.labels` option and otherwise still to `"detailed"`, and `get_census()` shows a message once per session if neither `labels` nor the option are set. Setting either one, e.g. with `options(cancensus.labels = "detailed")`, ensures code keeps working when the default changes, and `quiet = TRUE` suppresses the message
- `get_census()` now stops with an error for invalid `labels` values, previously these were treated like `labels = "detailed"`
- All requests to the CensusMapper API now identify the package and version through the user agent, in the form `cancensus/<version> httr/<version> R/<version>`. This lets the API know which tools are making requests and retain backward compatibility with older package versions as the API evolves
- Packages and tools building on **cancensus** can identify themselves by setting `options(cancensus.user_agent_prefix = "mypackage/1.2.3")`, which is prepended to the user agent
- When no persistent cache path is set, **cancensus** now warns once per session when data gets downloaded, instead of emitting a message on package load and on every internal cache lookup, including when nothing was downloaded (#227). `get_census()`, `get_intersecting_geometries()` and the StatCan download functions no longer emit this notice otherwise, and `quiet = TRUE` suppresses it where available. Since this is a warning, it will show up in rendered documents and turns into an error under `options(warn = 2)`, setting a cache path avoids it
- Documented how to use `set_cancensus_cache_path()` without `install = TRUE` to set up a project-specific cache
- `list_census_datasets()` and `list_census_regions()` now respect the `cancensus.base_url` option like all other CensusMapper API calls

## Bug fixes

- `get_census()` now accepts `"CA"` and `"CMACA"` in addition to `"CMA"`, both for the `level` parameter and as names of `regions`. All three refer to the same level, which contains both census metropolitan areas and census agglomerations. This also fixes the error when passing census agglomerations from `list_census_regions()` through `as_census_region_list()` to `get_census()` (#208)
- The examples of `show_cancensus_api_key()` and `show_cancensus_cache_path()` no longer run when building documentation, which printed the values set on the build machine into the generated pages
- `label_vectors()` now works for `sf` and `sp` results of `get_census()`. Renaming columns dropped the labels for most requests with geographic data
- `find_census_vectors(query_type = "keyword")` no longer errors in non-interactive sessions when there are additional matches with less precision. It returns the top matches with a message about the number of additional matches, since the prompt to show them can't be answered
- `label_vectors()` now returns the full label for vectors with colons in their label, e.g. `Size of census family: 2 persons` in CA06 and CA11. Previously the `Detail` column was cut off at the second colon, and the result had a third unnamed column
- `find_census_vectors()` keyword and semantic search no longer drop non-ASCII letters from the query
- `get_census()` no longer errors when only some of the `vectors` are named, the unnamed ones keep their vector code as column name
- Checking the local cache for recalled data no longer errors when the recall database lists only data recalls or only geometry recalls. Previously this would have made `get_census()` fail for users with a non-empty cache
- `get_census()` and `list_census_regions()` no longer trigger a deprecation warning with readr 2.2.0 or newer when parsing downloaded data, **cancensus** now requires readr 2.0.0 or newer
- Column selections no longer use `.data`, which is deprecated in tidyselect 1.2.0 and newer
- `show_cancensus_cache_path()` now also reports a cache path set via the `cancensus.cache_path` option, previously it reported that no cache path was set
- The error for a corrupt cache path no longer ends in a stray `FALSE`
- `get_statcan_geographic_attributes()` no longer returns misaligned columns for 2006 and 2011, the fixed-width layout it used did not match these files. For 2006 the columns from `DAlamy` on were wrong, for 2011 the columns from `CDuid` on, so results based on these columns from earlier versions should be checked
- `get_statcan_geographies()`, `get_statcan_geo_suite()` and `get_statcan_geography_relationships()` now fail gracefully when Statistics Canada blocks the download with a Cloudflare browser verification (#228). They return `NULL` with a warning that gives the link for downloading the file in a browser and the location in the cache directory to move it to, and pick up the file from there on the next run. If the download was blocked while refreshing existing cached data with `refresh = TRUE`, they return the cached data with the same warning instead of `NULL`. The file has to stay zipped, so on macOS the warning points out that Safari unzips downloads automatically, and it flags folders that look like an unzipped copy of the file

# cancensus 0.6.1

## Bug fixes

- `set_cancensus_api_key()` and `set_cancensus_cache_path()` with `overwrite = TRUE, install = TRUE` no longer truncate the user's `.Renviron` when it does not already contain the corresponding key
- `get_statcan_wds_data()` no longer caches error response bodies as data, and `refresh = TRUE` now bypasses the data cache as documented
- `remove_recalled_cached_data()` no longer over-matches recalled vector IDs (e.g. a recall of `v_CA21_1` no longer also flags cached data for `v_CA21_10`)
- `get_census()` now warns about recalled data when reading tabular data from the local cache, matching the geometry path
- `parent_census_vectors()` and `child_census_vectors()` now stop with "Unable to determine dataset" for mixed-dataset input instead of silently dropping vectors
- `find_census_vectors()` exact search now matches queries containing regex metacharacters (e.g. `"income ($)"`) literally instead of erroring or silently failing
- `find_census_vectors()` semantic search now considers the best match for every query word (previously only the full phrase), and restores suffix n-grams dropped in 0.6.0
- `find_census_vectors()` keyword search no longer matches every vector when the query starts with a digit or punctuation
- `visualize_vector_hierarchy()` accepts multi-vector character input as documented, and no longer labels nodes truncated by `max_depth` as leaves (now marked with `...`)
- `add_unique_names_to_region_list()` now correctly preserves grouping of grouped input
- `list_census_datasets()` and `list_census_regions()` staleness warnings now respect `quiet = TRUE`
- API retry logic now retries HTTP 429 (rate limit) and 408 (timeout) responses, honoring `Retry-After` headers
- The cache directory is now created recursively with a clear error on failure

## Performance improvements

- New in-memory session cache for `list_census_vectors()` and `list_census_regions()`: repeated calls within a session no longer re-read and deserialize the cached file on every access (cache hits are ~1000x faster)
- `parent_census_vectors()` and `child_census_vectors()` use a hash-based BFS traversal, 4-8x faster on deep hierarchies on top of the 0.6.0 improvements, with identical output
- `find_census_vectors()` semantic search prunes fuzzy-match candidates by length bound before computing Levenshtein distances
- `get_census()` no longer scans the package library (`installed.packages()`) on spatial calls
- `get_statcan_wds_metadata()` parent ID extraction is vectorized (~5-10x faster metadata parsing for large levels such as DA)

## Minor changes
- update links to refreshed CensusMapper

# cancensus 0.6.0

## Performance Improvements

- **Significant performance improvements** to census vector hierarchy traversal functions
  - `parent_census_vectors()` is now **1.9x faster** (92% speedup) by caching the full vector list once instead of repeated lookups
  - `child_census_vectors()` is now **1.2x faster** (23% speedup) with same optimization
  - Replaced O(n²) rbind operations in loops with efficient list accumulation
  - Deep hierarchies (8+ levels) see the most improvement

- **Faster semantic search** in `find_census_vectors()`
  - N-gram generation is now **1.4x faster** (30-40% speedup)
  - Optimized text processing with pre-allocated vectors
  - Better performance for all query types and lengths

## Testing & Quality

- Added comprehensive test suite with **43 unit tests**
- New `tests/testthat/` directory structure
- All optimizations maintain 100% backward compatibility
- No breaking changes to any function signatures or behavior
- Added `microbenchmark` and `testthat` to Suggests

## New features

- Convenience function to visualize vector hierarchy (`visualize_vector_hierarchy()`)

## Various small improvements

- Improvements in warnings and messages
- Removal of stale code
- Download progress reporting and retry logic for robustness against network issues

# cancensus 0.5.10

- ensure that cancensus won't trigger CRAN check errors if StatCan servers are down
- replace shorthand function with regular function definition to address CRAN note

# cancensus 0.5.8

- clarify installing API key and cache path in "getting started" vignette
- fix caching problem with get_statcan_wds_data
- support health region level census data for 2021 via WDS
- support download of dissemination block geographies from StatCan

# cancensus 0.5.7

- fix issue with path names not quoted properly when downoading and unpacking geosuite data
- support versions in WDS data, in paricular for federal electoral districts

# cancensus 0.5.6

- fix issue when using named vectors to query data for non-existent geographies, return NULL in this case instead of throwing error
- fix problem with population centre geographic data download
- support newly released Forward Sortation Area geography for statcan geography and WDS functionality
- remove instances of new native R pipe |> with dplyr pipe %>% to preserve compatibility with older R versions

# cancensus 0.5.5

- add functionality for direct access to StatCan census WDS for 2021
- add functionality to download original StatCan geographies for 2021
- update CODES_TABLE for 2021 census

# cancensus 0.5.4

- added ability to query census datasets by year
- added a convenience function for creating unique names within given selection of regions from `list_census_regions()`
- added a check and context menu to install `sf` package when user requests spatial data but does not have the required package installed as opposed to erroring out.
- improved checking that correct spatial formats are requested
- preparing for 'sp' spatial format usage deprecation in future versions

# cancensus 0.5.3

- Added a check and context menu to install `sf` package when user requests spatial data but does not have the required package installed as opposed to stopping with an error. 
- fixes a bug in the local data recall check

# cancensus 0.5.2

- resolved broken and relocated links picked up in CRAN pretesting

# cancensus 0.5.1

- Added functionality for users to detect and remove locally cached data that has been recalled by Statistics Canada
- added a check on initial `get_census` call that produces a warning if locally cached data has been recalled by Statistics Canada
- added option to retrieve higher-resolution geometries in `get_census`

## Minor changes
- renamed get/set cache/api_key functions to avoid name conflicts with related packages

# cancensus 0.5.0

## Minor changes
- Added cache metadata
- overview information on cached data
- allow selective removing of cached data

# cancensus 0.4.3

## Minor changes
- Fixed a problem with `sf` compatibility and `agr` attribute
- Fixed a problem with querying geographic data for multiple geographic levels

# cancensus 0.4.2

## Minor changes
- Fixed a minor problem where cache path wasn't always picked up
- Added optional argument `keep_parent` when calling `child_census_vectors()` that retains the input parent variable in the list of result. We found that in many cases user would follow up a call to `child_census_vectors()` with a `bind_rows(...)` to do this, so this should save a step. 

# cancensus 0.4.1

## Minor changes
- Fixed minor problem where API key wasn't always picked up if not set correctly as environment variable.
- Fixed warning when `t` column not present in downloaded data.

# cancensus 0.4.0

## Major changes
- Added `get_intersecting_geometry` function to take advantage of new Censusmapper API endpoint. Check out the new [vignette](https://mountainmath.github.io/cancensus/articles/intersecting_geometries.html) detailing how to use it. 

## Minor changes
- Slightly reworked how to configure and store user API keys and cache locations. New functions `set_api_key`, `set_cache_path`, `show_api_key`, and `show_cache_path` added.
- Fixes various warnings due to recent changes in tibble and dplyr.

# cancensus 0.3.2

## Major changes
- Support for 1996 census
- Public availability of dissemination block area level data

## Minor changes
- Fixes [bug](https://github.com/mountainMath/cancensus/issues/150) in `find_census_vectors()`

# cancensus 0.3.1

## Minor changes
- CRAN check issues

# cancensus 0.3.0

## Major changes
- Fully redesigned variable search using `find_census_vectors()` and deprecation of `search_census_vectors(). See the [Data discovery: resources for finding available and relevant data vignette](https://mountainmath.github.io/cancensus/articles/data_discovery.html) for additional information.
- Census Agglomerations with defined geographies and Census tracts are separated from CMAs when calling `list_census_regions()`
- Additional metadata for catalogue information and attribution is returned when calling `list_census_datasets()`
- New functions `explore_census_regions` and `explore_census_vectors` which open a browser page towards the interactive discovery and selection tools on the [Censusmapper website](https://censusmapper.ca/api)
- New function `attribution_for_dataset` which provides accurate attribution information for citation and visualizations for a given dataset.
- Additional datasets: T1FF taxfiler data and dwelling type crosstabs, made available by CMHC. For more info, see the new vignettes for these datasets: [Additional datasets: Structural type of dwelling by document type](https://mountainmath.github.io/cancensus/articles/Dwellings_by_document_type_cross_tabulation.html), [Additional datasets: Annual T1FF taxfiler data](https://mountainmath.github.io/cancensus/articles/Taxfiler_Data.html). 
- `get_census_geometry()` is now hard-deprecated and will stop the program flow. Use `get_census()` instead.

## Minor changes
- Updated internal usage of dependent packages to avoid deprecated functions and warnings
- Additional minor fixes to efficiency improvements

# cancensus 0.2.2

## Minor changes
- More efficient conversion between `sp` and `sf` spatial objects
- Fixes issue occasionally reported when reading large GeoJSON objects as noted in https://github.com/mountainMath/cancensus/issues/138
- Adds `geojsonsf` dependency
- Replaces soft deprecated dplyr functions to fix https://github.com/mountainMath/cancensus/issues/137

# cancensus 0.2.1

## Minor changes
- Minor documentation improvements
- Allow for search by vector identifier
- Allow child and parent vector retrieval by vector identifier
- Add functionality for future tax data releases

# cancensus 0.2.0

## Major changes
- Added a `max_leaves_option` for the `search_census_vectors` function. Adds functionality to set maximum depth for child census vectors, i.e. `max_level=NA` as an additional parameter. Then e.g. `max_level=1` would only get direct children and no grandchildren.
- Resolved issues with mislabeled CD UID for CSD level data
- Improve reliability of character/numeric alignments for geographic UIDS
- `get_census_geography` is now soft deprecated and rolled into the standard `get_census` with parameters

## Minor changes
- Minor documentation improvements
- Soften dependency on `readr` package
- Allow for search of internal CensusMapper variables 

# cancensus 0.1.8

## Major changes
- Remove dependency on development versions of `sf` and `ggplot2` packages
- Add feature allowing for named vector retrieval

## Minor Changes
- Vignette updates
- Calls that retrieve only geography will now return geographic hierarchy information as well
- Small tweaks that make it easier to do 2011-2016 intra-census comparisons

## Bug fixes
- Fix labels when geo format is `sf`
- Remove some unnecessary code for geographic hierarchies

# cancensus 0.1.7

## Major changes 
- Set default caching to `tempdir()`
- Encourage users to set up local persistent caching using `options(cancensus.cache_path = 'XXX')`

# cancensus 0.1.6

## Major changes
- First released on CRAN

## Minor changes
- Ensured that data frames were `sf`-class data frames if option `sf` was selected for spatial data. 

## Bug fixes
- Fixed a bug where `read.csv` was incorrectly loading text data if `readr::read_csv` was not available 

# cancensus 0.1.5

## Major changes
- Added a `NEWS.md` file to track changes to the package.
- Added pkgdown documentation
- New vignettes - _cancensus_ and _Making maps with cancensus_

## Minor changes
- Default behaviour for `list_census_vectors()` changed to have `quiet = TRUE`

# cancensus 0.1.0

## Major changes
- Added vector and geography search and discovery capabilities
- Improved performance
- Error and API issue messaging

## Minor changes
- Reduced package dependencies

# cancensus 0.0.1

## Major changes
- Initial release
