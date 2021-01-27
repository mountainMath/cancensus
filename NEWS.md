# cancensus 0.4.1

## Minor changes
- Fix minor problem where API key wasn't always picked up if not set correctly as environment variable.
- Fix warning when `t` column not present in downloaded data.

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
- Resolved issues with mislabelled CD UID for CSD level data
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
