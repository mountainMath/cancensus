# StatCan attribute files

``` r

library(cancensus)
library(dplyr)
```

## Background

Attribute files describe the detailed relationship of various Statistics
Canada geographic levels and provide population, household and dewlling
counts. This information can be useful for understanding the
hierarchical relationships of different geographic levels. The
[CensusMapper API](https://censusmapper.ca/api) that **cancensus** uses
for most of the data queries is ill-suited to get comprehensive data on
the hierarichal relationships Canada wide, so it can be helpful to have
direct access to this data in comprehensive tabular form.

## Match between Census Tracts and Census Subdivisions

If we are interested in understanding which Census Tracts respect
municipal boundaries and which ones don’t in 2021 we can consult the
geographic attributes file. It contains a row for each Census Block, the
basic building block of census geographies, and tags other levels of
geography the Census Block lies in.

Statistics Canada blocks programmatic downloads of these files, so
[`get_statcan_geographic_attributes()`](https://mountainmath.github.io/cancensus/reference/get_statcan_geographic_attributes.md)
reads copies hosted as parquet files, which requires the **duckdb** and
**DBI** packages. The result says where the data comes from.

``` r

attributes <- get_statcan_geographic_attributes("2021")
```

``` r

attr(attributes, "metadata")$attribution_en
#> [1] "Statistics Canada, 2021 Census Geographic Attribute File, product 92-151-X"
attr(attributes, "metadata")$licence_en
#> [1] "Statistics Canada Open Licence"
```

``` r

attributes %>% colnames()
#>  [1] "PRUID"          "PRDGUID"        "PRNAME"         "PRENAME"       
#>  [5] "PRFNAME"        "PREABBR"        "PRFABBR"        "CDUID"         
#>  [9] "CDDGUID"        "CDNAME"         "CDTYPE"         "FEDUID"        
#> [13] "FEDDGUID"       "FEDNAME"        "CSDUID"         "CSDDGUID"      
#> [17] "CSDNAME"        "CSDTYPE"        "DPLUID"         "DPLDGUID"      
#> [21] "DPLNAME"        "DPLTYPE"        "ERUID"          "ERDGUID"       
#> [25] "ERNAME"         "CCSUID"         "CCSDGUID"       "CCSNAME"       
#> [29] "SACTYPE"        "SACCODE"        "CMAPUID"        "CMAPDGUID"     
#> [33] "CMAUID"         "CMADGUID"       "CMANAME"        "CMATYPE"       
#> [37] "CTUID"          "CTDGUID"        "CTCODE"         "CTNAME"        
#> [41] "POPCTRRAPUID"   "POPCTRRAPDGUID" "POPCTRRAUID"    "POPCTRRADGUID" 
#> [45] "POPCTRRANAME"   "POPCTRRATYPE"   "POPCTRRACLASS"  "DAUID"         
#> [49] "DADGUID"        "DARPLAMX"       "DARPLAMY"       "DARPLAT"       
#> [53] "DARPLONG"       "DBUID"          "DBDGUID"        "DBPOP2021"     
#> [57] "DBTDWELL2021"   "DBURDWELL2021"  "DBAREA2021"     "DBIR2021"      
#> [61] "ADAUID"         "ADADGUID"       "ADACODE"
```

To answer our question, we filter Census Block by the ones that lie
within a Census Tract, and check for the collection of Census Blocks
within each Census Tract how many municipalities they lie in.

``` r

attributes %>%
  filter(CMATYPE %in% c("B","K")) |> # filter areas not in CTs
  group_by(CTCODE,CMATYPE) |>
  summarise(`Number of municipalities`=length(unique(CSDUID)),.groups="drop") |>
  count(`Number of municipalities`,CMATYPE) |>
  arrange(CMATYPE,`Number of municipalities`)
#> # A tibble: 7 × 3
#>   `Number of municipalities` CMATYPE     n
#>                        <int> <chr>   <int>
#> 1                          1 B        5997
#> 2                          2 B          33
#> 3                          3 B          13
#> 4                          4 B           5
#> 5                          6 B           4
#> 6                          1 K         194
#> 7                          2 K           1
```

This shows that most census tracts for both Census Metropolitan Areas
(CMATYPE=“B”) and tracted Census Agglomerations (CMATYPE=“K”) lie within
a single municipality, with some census tracts spanning several
municipalities.

## Older censuses

Attribute files are available back to 1981. From 2001 on they have a row
for each Census Block, identified by `DBUID`. Earlier censuses have a
row for each Enumeration Area, identified by `EAUID`, and different
columns. Each column has a description in its `"label"` attribute, and
the metadata of each file has notes on how it was built and on problems
with the source.

``` r

attributes_1981 <- get_statcan_geographic_attributes("1981")
attributes_1981 %>% colnames()
#>  [1] "EAUID"       "PRUID"       "FEDUID"      "FEDNAME"     "EACODE"     
#>  [6] "CDUID"       "CDNAME"      "CSDUID"      "CSDNAME"     "CSDTYPE"    
#> [11] "CSDTYPECODE" "CSDPOPGROUP" "CCSUID"      "CMAUID"      "CMANAME"    
#> [16] "CMATYPE"     "CMAPART"     "CMAPOPGROUP" "CTCODE"      "CTNAME"     
#> [21] "SPRUID"      "UACODE"      "UAPOPGROUP"  "EAPOP1981"   "EAPHH1981"  
#> [26] "EACF1981"    "EARPUTMZONE" "EARPUTMX"    "EARPUTMY"    "EARPLAT"    
#> [31] "EARPLONG"    "EARPLAMX"    "EARPLAMY"    "NTSMAP"
attr(attributes_1981$EAPHH1981, "label")
#> [1] "Private households, 1981"
attributes_1981 %>% summarise(Population = sum(EAPOP1981, na.rm = TRUE))
#> # A tibble: 1 × 1
#>   Population
#>        <int>
#> 1   24343181
attr(attributes_1981, "metadata")$notes_en
#> [1] "One row per enumeration area. Supplied by Statistics Canada as a fixed-width file with a scanned guide, which gives field descriptions but no field names, so the column names here were assigned to match the later years. Coordinates are the population centroid, latitude and longitude as given in degrees and whole minutes. Longitude is stored negative west of Greenwich, the source gives it unsigned. The file has no incomplete enumeration flag, so incompletely enumerated areas cannot be told apart from areas with no population. The subprovincial region is blank in the source on 271 EAs. The 1981 size groups were checked against the summed EA populations."
```

## Representative points

The attribute files have representative points for each area, as
latitude and longitude and in some years also as projected coordinates.
The metadata of each coordinate column gives its coordinate reference
system: the EPSG code in `crs_epsg` where there is one, the PROJ string
in `crs_proj`, and in `crs_en` how this was established. In most years
the source documentation doesn’t state the datum, so it was inferred by
checking the columns against each other, which `crs_basis` records.

``` r

point_metadata <- attr(attributes, "metadata")$columns$DARPLAT
point_metadata$crs_en
#> [1] "NAD83 geographic, EPSG:4269. Derived from the Lambert columns, which the 2021 reference guide (92-151-G) documents as NAD83. Fit to them as EPSG:3347: 0.04 m median."
point_metadata$crs_basis
#> [1] "inferred"
```

From 2006 on the representative points belong to dissemination areas,
and each point is repeated on every block row of its dissemination area.
Converting the rows directly would stack all blocks of a dissemination
area on one point, so we first reduce the data to one row per
dissemination area, here for Metro Vancouver, before converting it to an
**sf** object with the CRS from the metadata.

``` r

library(sf)
library(ggplot2)

da_points <- attributes %>%
  filter(CMAUID == "933") %>%
  group_by(DAUID, DARPLONG, DARPLAT) %>%
  summarise(Population = sum(DBPOP2021, na.rm = TRUE), .groups = "drop") %>%
  st_as_sf(coords = c("DARPLONG", "DARPLAT"), crs = point_metadata$crs_epsg)

ggplot(da_points) +
  geom_sf(aes(size = Population), alpha = 0.3, shape = 16) +
  scale_size_area(max_size = 3) +
  theme_void() +
  labs(title = "Metro Vancouver dissemination area representative points, 2021")
```

![](statcan_attribute_files_files/figure-html/unnamed-chunk-9-1.png)

The points differ between census years:

- 2001 is the only year with block level points, in `DBRPLAT` and
  `DBRPLONG`, in addition to the dissemination area points.
- 1981 to 1996 have one point per enumeration area, but on the NAD27
  datum (EPSG:4267) rather than NAD83. Transforming them to NAD83 is
  most accurate with the Canadian NTv2 grid, which PROJ can fetch when
  network access is enabled with `sf::sf_proj_network(TRUE)`. The
  projected Lambert columns of these years are on NAD27 as well and
  should not be read as EPSG:3347.
- 1981 and 1986 give latitude and longitude only to the whole minute,
  about 1.85 km north to south, so many enumeration areas share a point.
  The UTM columns are more precise, but their zone varies by row.

The `precision_en` entry of the column metadata has the details.

``` r

attributes_1996 <- get_statcan_geographic_attributes("1996")
crs_1996 <- attr(attributes_1996, "metadata")$columns$EARPLAT$crs_epsg
crs_1996
#> [1] 4267

ea_points <- attributes_1996 %>%
  filter(CMAUID == "933") %>%
  st_as_sf(coords = c("EARPLONG", "EARPLAT"), crs = crs_1996) %>%
  st_transform(st_crs(da_points))
```
