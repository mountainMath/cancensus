## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = nzchar(Sys.getenv("COMPILE_VIG"))
)

## ---- message = FALSE, warning = FALSE-----------------------------------
library(cancensus)
library(sf)
# retrieve sf dataframe
toronto <- get_census(dataset='CA16', regions=list(CMA="35535"),
                         vectors=c("median_hh_income"="v_CA16_2397"), level='CSD', quiet = TRUE, 
                         geo_format = 'sf', labels = 'short')

## ------------------------------------------------------------------------
plot(toronto["median_hh_income"], main = "Toronto Household Income by CSD")

## ------------------------------------------------------------------------
plot(toronto[14], col = NA, main = "Toronto CSDs with Median HH Income > 100,000", lty = 3)
plot(toronto[toronto$median_hh_income > 100000,14], col = "red", add = TRUE)

## ------------------------------------------------------------------------
library(ggplot2)
ggplot(toronto) + geom_sf(aes(fill = median_hh_income))

## ------------------------------------------------------------------------
ggplot(toronto) + geom_sf(aes(fill = median_hh_income), colour = "grey") +
  scale_fill_viridis_c("Median HH Income", labels = scales::dollar) + theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) + 
  coord_sf(datum=NA) +
  labs(title = "Median Household Income", subtitle = "Toronto Census Subdivisions, 2016 Census")

## ----message=FALSE, warning=FALSE, include=FALSE-------------------------
# This chunk disables leaflet output in remaining chunks for PDF vignettes
if(Sys.getenv("COMPILE_VIG")==FALSE) {knitr::opts_chunk$set(eval = FALSE)}

## ---- message = FALSE----------------------------------------------------
library(leaflet)
leaflet(toronto) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons()

## ------------------------------------------------------------------------
bins <- c(0, 30000,40000, 50000,60000, 70000,80000, 90000,100000, 110000, Inf)
pal <- colorBin("RdYlBu", domain = toronto$median_hh_income, bins = bins)
leaflet(toronto) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(fillColor = ~pal(median_hh_income),
              color = "white",
              weight = 1,
              opacity = 1,
              fillOpacity = 0.65)

