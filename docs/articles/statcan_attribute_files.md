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

``` r

attributes <- get_statcan_geographic_attributes("2021")

attributes %>% colnames()
#>  [1] "PRUID_PRIDU"                   "PRDGUID_PRIDUGD"              
#>  [3] "PRNAME_PRNOM"                  "PRENAME_PRANOM"               
#>  [5] "PRFNAME_PRFNOM"                "PREABBR_PRAABBREV"            
#>  [7] "PRFABBR_PRFABBREV"             "CDUID_DRIDU"                  
#>  [9] "CDDGUID_DRIDUGD"               "CDNAME_DRNOM"                 
#> [11] "CDTYPE_DRGENRE"                "FEDUID_CEFIDU"                
#> [13] "FEDDGUID_CEFIDUGD"             "FEDNAME_CEFNOM"               
#> [15] "CSDUID_SDRIDU"                 "CSDDGUID_SDRIDUGD"            
#> [17] "CSDNAME_SDRNOM"                "CSDTYPE_SDRGENRE"             
#> [19] "DPLUID_LDIDU"                  "DPLDGUID_LDIDUGD"             
#> [21] "DPLNAME_LDNOM"                 "DPLTYPE_LDGENRE"              
#> [23] "ERUID_REIDU"                   "ERDGUID_REIDUGD"              
#> [25] "ERNAME_RENOM"                  "CCSUID_SRUIDU"                
#> [27] "CCSDGUID_SRUIDUGD"             "CCSNAME_SRUNOM"               
#> [29] "SACTYPE_CSSGENRE"              "SACCODE_CSSCODE"              
#> [31] "CMAPUID_RMRPIDU"               "CMAPDGUID_RMRPIDUGD"          
#> [33] "CMAUID_RMRIDU"                 "CMADGUID_RMRIDUGD"            
#> [35] "CMANAME_RMRNOM"                "CMATYPE_RMRGENRE"             
#> [37] "CTUID_SRIDU"                   "CTDGUID_SRIDUGD"              
#> [39] "CTCODE_SRCODE"                 "CTNAME_SRNOM"                 
#> [41] "POPCTRRAPUID_CTRPOPRRPIDU"     "POPCTRRAPDGUID_CTRPOPRRPIDUGD"
#> [43] "POPCTRRAUID_CTRPOPRRIDU"       "POPCTRRADGUID_CTRPOPRRIDUGD"  
#> [45] "POPCTRRANAME_CTRPOPRRNOM"      "POPCTRRATYPE_CTRPOPRRGENRE"   
#> [47] "POPCTRRACLASS_CTRPOPRRCLASSE"  "DAUID_ADIDU"                  
#> [49] "DADGUID_ADIDUGD"               "DARPLAMX_ADLAMX"              
#> [51] "DARPLAMY_ADLAMY"               "DARPLAT_ADLAT"                
#> [53] "DARPLONG_ADLONG"               "DBUID_IDIDU"                  
#> [55] "DBDGUID_IDIDUGD"               "DBPOP2021_IDPOP2021"          
#> [57] "DBTDWELL2021_IDTLOG2021"       "DBURDWELL2021_IDRHLOG2021"    
#> [59] "DBAREA2021_IDSUP2021"          "DBIR2021_IDRI2021"            
#> [61] "ADAUID_ADAIDU"                 "ADADGUID_ADAIDUGD"            
#> [63] "ADACODE_ADACODE"
```

To answer our question, we filter Census Block by the ones that lie
within a Census Tract, and check for the collection of Census Blocks
within each Census Tract how many municipalities they lie in.

``` r

attributes %>%
  filter(CMATYPE_RMRGENRE %in% c("B","K")) |> # filter areas not in CTs
  group_by(CTCODE_SRCODE,CMATYPE_RMRGENRE) |>
  summarise(`Number of municipalities`=length(unique(CSDUID_SDRIDU)),.groups="drop") |>
  count(`Number of municipalities`,CMATYPE_RMRGENRE) |>
  arrange(CMATYPE_RMRGENRE,`Number of municipalities`)
#> # A tibble: 7 × 3
#>   `Number of municipalities` CMATYPE_RMRGENRE     n
#>                        <int> <chr>            <int>
#> 1                          1 B                 5997
#> 2                          2 B                   33
#> 3                          3 B                   13
#> 4                          4 B                    5
#> 5                          6 B                    4
#> 6                          1 K                  194
#> 7                          2 K                    1
```

This shows that most census tracts for both Census Metropolitan Areas
(CMATYPE_RMRGENRE=“B”) and tracted Census Agglomerations
(CMATYPE_RMRGENRE=“K”), with some census tracts spanning several
municipalities.
