# Package index

## Retrieving Census data

- [`get_census()`](https://mountainmath.github.io/cancensus/reference/get_census.md)
  : Access to Canadian census data through the CensusMapper API

## Data discovery

- [`list_census_datasets()`](https://mountainmath.github.io/cancensus/reference/list_census_datasets.md)
  : Query the CensusMapper API for available datasets.

- [`list_census_regions()`](https://mountainmath.github.io/cancensus/reference/list_census_regions.md)
  : Query the CensusMapper API for available regions in a given dataset.

- [`as_census_region_list()`](https://mountainmath.github.io/cancensus/reference/as_census_region_list.md)
  :

  Convert a (suitably filtered) data frame from `list_census_regions` to
  a list suitable for passing to `get_census`.

- [`list_census_vectors()`](https://mountainmath.github.io/cancensus/reference/list_census_vectors.md)
  : Query the CensusMapper API for available vectors for a given
  dataset.

- [`find_census_vectors()`](https://mountainmath.github.io/cancensus/reference/find_census_vectors.md)
  : Query the CensusMapper API for vectors using exact, semantic, or
  keyword search

- [`search_census_regions()`](https://mountainmath.github.io/cancensus/reference/search_census_regions.md)
  : Query the CensusMapper API for regions with names matching a
  searchterm.

- [`explore_census_vectors()`](https://mountainmath.github.io/cancensus/reference/explore_census_vectors.md)
  : Interactively browse Census variables and regions on Censusmapper.ca
  in a new browser window

- [`explore_census_regions()`](https://mountainmath.github.io/cancensus/reference/explore_census_regions.md)
  : Interactively browse Census variables and regions on Censusmapper.ca
  in a new browser window

- [`search_census_vectors()`](https://mountainmath.github.io/cancensus/reference/search_census_vectors.md)
  : Query the CensusMapper API for vectors with descriptions matching a
  search term or phrase (deprecated)

- [`get_intersecting_geometries()`](https://mountainmath.github.io/cancensus/reference/get_intersecting_geometries.md)
  : Get identifiers for census regions intersecting a geometry

## Working with Census data

- [`label_vectors()`](https://mountainmath.github.io/cancensus/reference/label_vectors.md)
  : Return Census variable names and labels as a tidy data frame

- [`add_unique_names_to_region_list()`](https://mountainmath.github.io/cancensus/reference/add_unique_names_to_region_list.md)
  : Convenience function for creating unique names from region list

- [`child_census_vectors()`](https://mountainmath.github.io/cancensus/reference/child_census_vectors.md)
  :

  List all child variables from vector hierarchies given either a list
  of Census variables returned by `list_census_vectors`,
  `search_census_vectors`, `find_census_vectors`, or a direct string
  reference to the vector code.

- [`parent_census_vectors()`](https://mountainmath.github.io/cancensus/reference/parent_census_vectors.md)
  :

  List all parent variables from vector hierarchies given either a list
  of Census variables returned by `list_census_vectors`,
  `search_census_vectors`, `find_census_vectors`, or a direct string
  reference to the vector code.

- [`visualize_vector_hierarchy()`](https://mountainmath.github.io/cancensus/reference/visualize_vector_hierarchy.md)
  : Visualize Census vector hierarchies as ASCII tree

- [`dataset_attribution()`](https://mountainmath.github.io/cancensus/reference/dataset_attribution.md)
  : Get attribution for datasets

## Managing cached data

- [`list_cancensus_cache()`](https://mountainmath.github.io/cancensus/reference/list_cancensus_cache.md)
  : List cached files
- [`remove_from_cancensus_cache()`](https://mountainmath.github.io/cancensus/reference/remove_from_cancensus_cache.md)
  : Remove cached files

## Recalled data

- [`list_recalled_cached_data()`](https://mountainmath.github.io/cancensus/reference/list_recalled_cached_data.md)
  : List recalled data stored in local cache
- [`remove_recalled_cached_data()`](https://mountainmath.github.io/cancensus/reference/remove_recalled_cached_data.md)
  : Remove recalled data from local cache

## User settings

- [`set_cancensus_api_key()`](https://mountainmath.github.io/cancensus/reference/set_cancensus_api_key.md)
  : Set Censusmapper API key
- [`set_cancensus_cache_path()`](https://mountainmath.github.io/cancensus/reference/set_cancensus_cache_path.md)
  : Set persistent cancensus cache location
- [`show_cancensus_api_key()`](https://mountainmath.github.io/cancensus/reference/show_cancensus_api_key.md)
  : View saved Censusmapper API key
- [`show_cancensus_cache_path()`](https://mountainmath.github.io/cancensus/reference/show_cancensus_cache_path.md)
  : View saved cache directory path

## Getting data directly from StatCan

- [`get_statcan_wds_data()`](https://mountainmath.github.io/cancensus/reference/get_statcan_wds_data.md)
  : Query the StatCan WDS for data
- [`get_statcan_wds_metadata()`](https://mountainmath.github.io/cancensus/reference/get_statcan_wds_metadata.md)
  : Query the StatCan WDS for metadata
- [`get_statcan_geographies()`](https://mountainmath.github.io/cancensus/reference/get_statcan_geographies.md)
  : Read the geosuite data
- [`get_statcan_geographic_attributes()`](https://mountainmath.github.io/cancensus/reference/get_statcan_geographic_attributes.md)
  : Read the Geographic Attributes File
- [`get_statcan_geography_relationships()`](https://mountainmath.github.io/cancensus/reference/get_statcan_geography_relationships.md)
  : Read the Dissemination Geographies Relationship File
- [`get_statcan_geo_suite()`](https://mountainmath.github.io/cancensus/reference/get_statcan_geo_suite.md)
  : Read the geosuite data

## Data

- [`CODES_TABLE`](https://mountainmath.github.io/cancensus/reference/CODES_TABLE.md)
  : A dataset with code table summaries for census data
- [`COV_SKYTRAIN_STATIONS`](https://mountainmath.github.io/cancensus/reference/COV_SKYTRAIN_STATIONS.md)
  : A dataset City of Vancouver skytrain station locations
