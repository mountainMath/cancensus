# cancensus

[![R-CMD-check](https://github.com/mountainMath/cancensus/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mountainMath/cancensus/actions/workflows/R-CMD-check.yaml)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/cancensus)](https://cran.r-project.org/package=cancensus)
[![CRAN_Downloads_Badge](https://cranlogs.r-pkg.org/badges/cancensus)](https://cranlogs.r-pkg.org/badges/cancensus)

<a href="https://mountainmath.github.io/cancensus/index.html"><img src="https://raw.githubusercontent.com/mountainMath/cancensus/master/images/cancensus-sticker.png" alt="cancensus logo" align="right" width = "25%" height = "25%"/></a>


Access, retrieve, and work with Canadian Census data and geography. 

* Download data and Census geography in tidy and analysis-ready format
* Convenience tools for searching for and working with Census regions and variable hierarchies
* Provides Census geography in multiple R spatial formats
* Provides data and geography at multiple Census geographic levels including province, Census Metropolitan Area, Census Division, Census Subdivision, Census Tract, Dissemination Areas, Enumeration Areas (for 1996), and Dissemination Blocks (for 2001-2021)
* Provides data for the 2021, 2016, 2011, 2006, 2001, and 1996 Census releases
* Access to taxfiler data at the Census Tract level for tax years 2000 through 2018
 
### Reference

[**Cancensus home page and reference guide**](https://mountainmath.github.io/cancensus/index.html)

### Installing the package

```
install.packages("cancensus")
library(cancensus)
```

Alternatively, the latest development version can be installed from Github.
```
remotes::install_github("mountainmath/cancensus")
library(cancensus)
```

### API key

**cancensus** requires a valid CensusMapper API key to use. You can obtain a free API key by [signing up](https://censusmapper.ca/users/sign_up) for a CensusMapper account. To check your API key, just go to "Edit Profile" (in the top-right of the CensusMapper menu bar). Once you have your key, you can store it in your system environment so it is automatically used in API calls. To do so just enter `set_cancensus_api_key('<your_api_key>', install = TRUE)`.

CensusMapper API keys are free and public API quotas are generous; however, due to incremental costs of serving large quantities of data, there are some limits to API usage in place. For most use cases, these API limits should not be an issue. Production uses with large extracts of detailed geographies may run into API quota limits.

The new `get_intersecting_geometries` function has a separate API quota. This functionality puts higher demands on server resources than other API calls and therefore comes from a different bucket. By default user keys are capped to 500 region identifiers a day or 5000 per month. This _should_ suffice for most casual use cases, but we will continue to monitor the impact on server load and may increase the default limits in the future.

For larger quotas, please get in touch with Jens [directly](mailto:jens@censusmapper.ca).

### Local Cache

For performance reasons, and to avoid unnecessarily drawing down API quotas, **cancensus** caches data queries under the hood. By default, **cancensus** caches in R's temporary directory, but this cache is not persistent across sessions. In order to speed up performance, reduce quota usage, and reduce the need for unnecessary network calls, we recommend assigning a persistent local cache using `set_cancensus_cache_path('<local cache path>', install = TRUE)`, this enables more efficient loading and reuse of downloaded data. Users will be prompted with a suggestion to change their default cache location when making API calls if one has not been set yet. 

Starting with version 0.5.2 **cancensus** will automatically check if for data that has been recalled by Statistics Canada and is stored in the local cache via the new data recall API implemented in [CensusMapper](https://censusmapper.ca). Statistics Canada occasionally detects and corrects errors in their census data releases, and **cancensus** will download a list of recalled data at the first invocation of `get_census()` in each session and emit a warning if it detected locally cached data that has been recalled. Removal of the cached recalled data has to be done explicitly by the user via the `remove_recalled_chached_data()` function. If data was cached with **cancensus** versions prior to version 0.5.0 there is insufficient metadata to determine all instances of recalled cached data, but the package will check every time cached data is loaded and can identify recalled data at this point at the latest and issues a warning if recalled data is loaded.

### Currently available datasets

**cancensus** can access Statistics Canada Census data for Census years 1996, 2001, 2006, 2011, 2016, and 2021. You can run `list_census_datasets` to check what datasets are currently available for access through the CensusMapper API.

Thanks to contributions by the Canada Mortgage and Housing Corporation (CMHC), **cancensus** now includes additional Census-linked datasets as open-data releases. These include annual taxfiler data at the census tract level for tax years 2000 through 2018, which includes data on incomes and demographics, as well as specialized crosstabs for Structural type of dwelling by Document type, which details occupancy status for residences. These crosstabs are available for the 2001, 2006, 2011, 2016, and 2021 Census years at all levels starting with census tract.

### Picking regions and variables

Census data contains thousands of different geographic regions as well as thousands of unique variables. There are several useful functions within **cancensus** to simplify accessing Census metadata, locating regions, and identifying variables.

```
# To view available Census datasets
list_census_datasets()

# To view available named regions at different levels of Census hierarchy for the 2016 Census (for example)
list_census_regions("CA16")

# To view available Census variables for the 2016 Census
list_census_vectors("CA16")
```

There is also an interactive tool that is available at the [CensusMapper API](https://censusmapper.ca/api) to visually select regions and variables and generate code for the API call. Calling `explore_census_vectors(dataset = "CA16")` or `explore_census_regions(dataset = "CA16")` will open a new browser window to this interactive tool, preconfigured for whichever Census dataset is set as an argument. 

### Getting the data

**cancensus** can return census data with or without associated Census geographical information that can be used for mapping and GIS. By default, **cancensus** returns tidy tabular data only, but has options to return spatial data objects in either [**sf**](https://cran.r-project.org/package=sf) or [**sp**](https://cran.r-project.org/package=sp) formats. 
```
# Return data only
census_data <- get_census(dataset='CA16', regions=list(CMA="59933"), 
                          vectors=c("v_CA16_408","v_CA16_409","v_CA16_410"), level='CSD')

# Return an sf-class data frame
census_data <- get_census(dataset='CA16', regions=list(CMA="59933"), 
                          vectors=c("v_CA16_408","v_CA16_409","v_CA16_410"), level='CSD', geo_format = "sf")
```
**cancensus** attempts to minimize bandwidth usage and download time by caching downloads. When attempting to download data that has previously been downloaded,  **cancensus** will instead access the locally cached equivalent. 

### Examples of work using cancensus

* [Income: A first look](https://doodles.mountainmath.ca/blog/2017/09/14/income-a-first-look/)
* [Language Diversity in Canada](https://www.dshkol.com/2017/language-diversity-in-canada/)
* [Diversity and Segregation in Canadian cities](https://www.dshkol.com/2018/diversity-and-segregation-canadian-cities/)
* [Census tract level T1FF data](https://doodles.mountainmath.ca/blog/2020/04/23/census-tract-level-t1ff-tax-data/)

We'd love to feature examples of work or projects that use cancensus.

### Vignettes 

* [Getting started with cancensus](https://mountainmath.github.io/cancensus/articles/cancensus.html)
* [Making maps with cancensus](https://mountainmath.github.io/cancensus/articles/Making_maps_with_cancensus.html)
* [Data discovery: resources for finding available and relevant data](https://mountainmath.github.io/cancensus/articles/data_discovery.html)
* [Additional datasets: Structural type of dwelling by document type](https://mountainmath.github.io/cancensus/articles/Dwellings_by_document_type_cross_tabulation.html)
* [Additional datasets: Annual T1FF taxfiler data](https://mountainmath.github.io/cancensus/articles/Taxfiler_Data.html)


### Related packages
The cancensus package is designed for working with Canadian Census data. In addition to Census data, Statistics Canada provides access to a vast [socio-economic data repository](https://www150.statcan.gc.ca/n1/en/type/data) with thousands of data tables available for public access. 

The [cansim package](https://mountainmath.github.io/cansim/index.html) is designed to retrieve and work with public Statistics Canada data tables. The cansim prepares retrieved data tables as analysis-ready tidy dataframes and provides a number of convenience tools and functions to make it easier to work with Statistics Canada data. It is available on CRAN and on [Github](https://github.com/mountainMath/cancensus). Data retrieved via the cansim package can be linked to census data via the `GeoUID` field.

The [tongfen package](https://mountainmath.github.io/tongfen/index.html) automates the task of obtaining census variables from different census years on a common geography. It is available on [Github](https://github.com/mountainMath/tongfen).

The [**cmhc**](https://mountainmath.github.io/cmhc/index.html) package, which accesses CMHC data on the housing in Canada and can be linked to census geographies via the `GeoUID`.

Taken together these packages offer a seamless view into important Canadian data.

### International census data
There are several other jurisdiction where census data is available via R packages, including

* US: [tidycensus](https://walker-data.com/tidycensus/) and [tigris](https://github.com/walkerke/tigris)
* Brasil: [geobr](https://github.com/ipeaGIT/geobr)
* Africa: [afrimapr](https://afrimapr.github.io/afrimapr.website/)
* Brazil: [geobr](https://ipeagit.github.io/geobr/)
* Chile: [chilemapas](https://pacha.dev/chilemapas/)
* Czech Republic: [RCzechia](https://github.com/jlacko/RCzechia)
* Finland: [geofi](https://ropengov.github.io/geofi/)
* Ghana: [rGhanaCensus](https://CRAN.R-project.org/package=rGhanaCensus)
* Spain: [mapSpain](https://github.com/rOpenSpain/mapSpain/)
* UK: [geographr](https://github.com/humaniverse/geographr)
* Uruguay: [geouy](https://github.com/RichDeto/geouy)
* Global (political administrative boundaries): [rgeoboundaries](https://github.com/wmgeolab/rgeoboundaries)


### Contributing

* We encourage contributions to improve this project. The best way is through issues and pull requests.
* If you want to get in touch, we are pretty good at responding via email or via twitter at [@dshkol](https://twitter.com/dshkol) or [@vb_jens](https://twitter.com/vb_jens). 

### Cite **cancensus**

If you wish to cite cancensus:

  von Bergmann, J., Aaron Jacobs, Dmitry Shkolnik (2022). cancensus: R package to
  access, retrieve, and work with Canadian Census data and geography. v0.5.8.


A BibTeX entry for LaTeX users is
```
  @Manual{cancensus,
    author = {Jens {von Bergmann} and Dmitry Shkolnik and Aaron Jacobs},
    title = {cancensus: R package to access, retrieve, and work with Canadian Census data and geography},
    year = {2024},
    note = {R package version 0.5.8},
    url = {https://mountainmath.github.io/cancensus/}
  }
```


### Statistics Canada Attribution

Subject to the Statistics Canada Open Data License Agreement, licensed products using Statistics Canada data should employ the following acknowledgement of source:

```
Acknowledgment of Source

(a) You shall include and maintain the following notice on all licensed rights of the Information:

  - Source: Statistics Canada, name of product, reference date. Reproduced and distributed on an "as is" basis with the permission of Statistics Canada.
 
(b) Where any Information is contained within a Value-added Product, you shall include on such Value-added Product the following notice:

  - Adapted from Statistics Canada, name of product, reference date. This does not constitute an endorsement by Statistics Canada of this product.
```
