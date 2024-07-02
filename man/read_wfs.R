rm(list = ls()); gc()

# get wfs tutorial
# https://inbo.github.io/tutorials/tutorials/spatial_wfs_services/


library(sf) # simple features packages for handling vector GIS data
library(httr) # generic webservice package
library(tidyverse) # a suite of packages for data wrangling, transformation, plotting, ...
library(ows4R) # interface for OGC webservices

url_text <- "https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_es_daten_2022"
url_text <- "https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_wohnlagenadr2021"


url <- parse_url(url_text)
url$query <- list(service = "wfs",
                  #version = "2.0.0", # facultative
                  request = "GetFeature"
)
request <- build_url(url)
request

bwk_client <- WFSClient$new(url_text, serviceVersion = "2.0.0")
bwk_client$getFeatureTypes(pretty = T)
# bwk_client$getFeatures()


bwk_client$
  getCapabilities()$
  findFeatureTypeByName("fis:s_wohnlagenadr2021")$
  getDescription(pretty = T)
x <- bwk_client$
  getCapabilities()$
  findFeatureTypeByName("fis:s_wohnlagenadr2021")$
  getFeatures()
plot(st_geometry(x))

# leaflet(x) %>%
#   addProviderTiles(providers$OpenStreetMap) %>%
#   addPolygons()

# getKeywords: function () 
#   getName: function () 
#     getNamespaceDefinition: function (recursive = FALSE) 
#       getTitle: function () 
#         