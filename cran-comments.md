# Update - 0.5.3
- Added a check and context menu to install `sf` package when user requests spatial data but does not have the required package installed as opposed to erroring out. 
- fixes a bug in the local data recall check

# Update - v0.5.2
- resolved broken and relocated links picked up in CRAN pretesting

# Update - v0.5.1
- Added functionality for users to detect and remove locally cached data that has been recalled by Statistics Canada
- added a check on initial `get_census` call that produces a warning if locally cached data has been recalled by Statistics Canada
- added option to retrieve higher-resolution geometries in `get_census`
- resolved lingering CRAN NOTE
- tested with RHUB default flavours

# Update - v0.5.0

- Added cache metadata
- overview information on cached data
- allow selective removing of cached data

# Update - v0.4.3

- Fixed a problem with `sf` compatibility and `agr` attribute
- Fixed a problem with querying geographic data for multiple geographic levels
- Addressed sporadic external site timeout issues flagged in CRAN checks

# Update - v0.4.2

- Fixed a minor problem where cache path wasn't always picked up on setting user profile settings.
- Small functionality tweak to some existing functions

# Update - v0.4.1

- CRAN check NOTES regarding marked UTF-8 strings are understood but use of non-ASCII characters is intentional due to bilingual EN/FR source data and metadata from national statistics agency. 
- Fixed minor problem where API key wasn't always picked up if not set correctly as environment variable.
- Fixed warning when `t` column not present in downloaded data.

# Update - v.0.4.0

- Added `get_intersecting_geometry` function for new CensusMapper endpoint
- Slightly reworked how to configure and store user API keys and cache locations to make it easier for users to set up. New functions `set_api_key`, `set_cache_path`, `show_api_key`, and `show_cache_path` added.
- Fixes various warnings due to recent changes in tibble and dplyr.

## Update - v.0.3.2

- Add functionality for 1996 census and more refined geographies.
- Expanded vignettes.
- Fix minor bugs flagged by users.

## Update - v.0.3.1

Addressing warning and note in CRAN checks from upload v.0.3.0
- Vignette chunk with encoding and output issues causing WARNING will only compile locally for use in web documentation and now will not be compiled externally as it did not contain any crucial information and was used for additional detail. 
- Fixed missing package NOTE in rd xref

## Update - v.0.3.0

General update of package. Changes include:
- New search and metadata functionalities
- Expanded API data coverage
- New vignettes and docs
- Checked on win-release and win-dev

## Update - v.0.2.2

Minor update of package. Changes include:
- Testing to ensure no issues with R 4.0.0
- More efficient conversion between `sp` and `sf` spatial objects
- Fixes issue occasionally reported when reading large GeoJSON objects
- Adds `geojsonsf` dependency
- Replaces soft deprecated dplyr functions with updated calls

## Update - v.0.2.1

Minor update of package. Changes include:
- Minor documentation improvements
- Allow for search by vector identifier
- Allow child and parent vector retrieval by vector identifier
- Add functionality for future tax data release

## Update - v.0.2.0

Maintenance update of package with some quality of life improvements. Changes include:
- Improved variable tree searching
- Better handling of numeric/string/factor alignments
- Backward compatibility for some soft deprecated functions

Release checked locally, on r-hub, and winbuilder. 

## Update - v.0.1.8

Minor update of package. Changes include: 
- New minor usability features that do not change package internals
- Addition of minor fixes 
- Replacement of unnecessary references to development-version packages in vignettes where CRAN versions now exist

## Update - v.0.1.7

After initial acceptance, CRAN checks identified an issue stemming from the package using a local cache that writes to the user library rather than R's temporary director. Kurt Hornik advised to fix this and to submit an update. 

- This update changes default caching behaviour to confirm with CRAN requirements. 
- Users are prompted with a suggestion encouraging them to set up a persistent local cache on load, and a second reminder when making an API call. 

## Resubmission - v.0.1.6 - Approved

This is a resubmission addressing comments from reviewer Swetlana Herbrandt. In addition to recommendations for changes to package title and description to avoid redundancy and conform to standards, Swetlana noted that all examples are wrapped in `/dontrun{}` and are not tested. 

In response, all package functions not requiring an API key to call are now tested in the documentation. All package functions including those requiring API keys are built, evaluated, and tested locally with online  [documentation](https://mountainmath.github.io/cancensus/reference/index.html). Swetlana suggested that this would be sufficient in a follow up email. 

Documentation now includes a prebuilt static html vignette rather than pdf ones to better display functionality. Package fully builds and compiles on [Travis](https://travis-ci.org/mountainMath/cancensus).

## Submission - v.0.1.6

First submission for this package. This is the first CRAN package I've developed so I apologize in advance if I have made any oversight. Thanks for your time. 

## Test environments
* local OS X install, R 3.4.3
* ubuntu 14.04 (on travis-ci), R 3.4.2
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs and one NOTE:

 - A NOTE came up for "Maintainer:..." which I understand can be ignored

## Reverse dependencies
This is a new release and there are no dependencies.

## Vignettes
Vignettes require API authentification and code chunks are not evaluated when system variables are not enabled. Vignettes are prebuilt with pdfs included. Vignettes are evaluated locally and on Travis with [log results](https://travis-ci.org/mountainMath/cancensus).
