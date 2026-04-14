# Query the CensusMapper API for vectors using exact, semantic, or keyword search

Query the available list of Census vectors based on their label and
return details including vector code. Default search behaviour expects
an exact match, but keyword or semantic searches can be used instead by
setting `query_type='keyword'` or `query_type = 'semantic'` instead.
Keyword search is useful when looking to explore Census vectors based on
broad themes like "income" or "language". Keyword search separates the
query into unigrams and returns Census vectors with matching words,
ranked by incidence of matches. Semantic search is designed for more
precise searches while allowing room for error for spelling or phrasing,
as well as for finding closely related vector matches. Semantic search
separates the query into n-grams and relies on string distance
measurement using a generalized Levenshtein distance approach.

Some census vectors return population counts segmented by `Female` and
`Male` populations, in addition to a total aggregate. By default, query
matches will return matches for the `Total` aggregation, but can
optionally return only the `Female` or `Male` aggregations by adding
`type = 'female'` or `type = 'male'` as a parameter.

## Usage

``` r
find_census_vectors(query, dataset, type = "all", query_type = "exact", ...)
```

## Arguments

- query:

  The term or phrase to search for e.g. `'Oji-cree'`. Search queries are
  case insensitive.

- dataset:

  The dataset to query for available vectors, e.g. `'CA16'`. To see a
  list of available datasets:
  [`list_census_datasets()`](https://mountainmath.github.io/cancensus/reference/list_census_datasets.md)

- type:

  One of `'all'`, `'total'`, `'male'` or `'female'`. If specified, only
  return aggregations of specified \`type\`. By default, only the
  `'total'` aggregation will be returned.

- query_type:

  One of `exact`, `'semantic'` or `'keyword'`. By default, assumes exact
  string matching, but the alternatives may be better options in some
  cases. See description section for more details on query types.

- ...:

  Other arguments passed to internal functions.

## Examples

``` r
if (FALSE) { # \dontrun{
find_census_vectors('Oji-cree', dataset = 'CA16', type = 'total', query_type = 'exact')

find_census_vectors('commuting duration', dataset = 'CA11', type = 'female', query_type = 'keyword')

find_census_vectors('after tax income', dataset = 'CA16', type = 'total', query_type = 'semantic')

# This incorrect spelling will return a warning that no match was found,
# but will suggest trying semantic or keyword search.
find_census_vectors('Ojibwey', dataset = 'CA16', type = 'total')

# This will find near matches as well
find_census_vectors('Ojibwey', dataset = 'CA16', type = 'total', query_type = "semantic")

find_census_vectors('commute duration', dataset = 'CA16', type = 'female', query_type = 'keyword')

find_census_vectors('commute duration', dataset = 'CA11', type = 'all', query_type = 'keyword')

find_census_vectors('ukrainian origin', dataset = 'CA16', type = 'total', query_type = 'keyword')

} # }
```
