# ==================================================================================================
# ================ Vereinfachen und aggregieren von Zählsprengel-shapefiles ========================
# ==================================================================================================

packages <- c("dplyr", "ggmap", "rgdal", "rgeos", "maptools", "ctv", "sf", "leaflet", "htmltools", "rmapshaper", "raster", "leaflet.extras")
lapply(packages, library, character.only=TRUE)
library(tmaptools)
rm(list=ls())

path <- 'X:/Geodaten/Wien/Gebiete_2021'
setwd(path)


# ========================= Geo-Daten verinfachen und verschieben ==================================

{
# Shape-files Zählsprengel-ebene
shape <- st_read("OGDEXT_ZSP_1_STATISTIK_AUSTRIA_20210101", "STATISTIK_AUSTRIA_ZSP_20210101")
# nur Wien
shape <- shape[grep('^9',shape$id),]

# simplify and shift to correct position -----------------------------------------------------------

# Simplify polygons
shape <- ms_simplify(shape, keep=0.09)
# Change datum (projection?)
shape <- shape %>% st_transform(4326)
# Correct position
shape$geometry <- shape$geometry + c(0.00278, -0.000695)

## Test changes
if(F) {
shape2 <- ms_simplify(shape, keep=1)
shape2 <- shape2 %>% st_transform(4326)
shape2$geometry <- shape2$geometry + c(0.00278, -0.000695)

leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
  addProviderTiles(providers$CartoDB.VoyagerNoLabels, options=providerTileOptions(minZoom=0)) %>%
  addPolygons(data=shape, color = 'black', weight = 2.5, fill=F, fillOpacity = 0) %>%
  addPolygons(data=shape2, color = 'red', weight = 2.5, fill=F, fillOpacity = 0)
}

st_write(shape, 'Zaehlsprengel_2021_simple', 'Zaehlsprengel_2021_simple', driver="ESRI Shapefile", delete_dsn=T,delete_layer=T)

}


# ================================ Shape-files Aggregieren =========================================

shape <- st_read("X:/Geodaten/Wien/Gebiete_2021/Zaehlsprengel_2021_simple", "Zaehlsprengel_2021_simple")

# leaflet(shape) %>%
#   addProviderTiles(providers$OpenStreetMap) %>%
#   addPolygons(weight = 1, fillOpacity = 0, label = ~name)

{
# Zuordnung der Grätzl
zuordnung <- readxl::read_excel("Graetzl_Zaehlgebiete-2022-05-12.xlsx")
zuordnung$ZGEB2 <- ifelse(nchar(zuordnung$ZGEB)==4, paste0(0,zuordnung$ZGEB), zuordnung$ZGEB)
zuordnung$bez <- substring(zuordnung$ZGEB2, 1, 2)
zuordnung$ZGEB2 <- substr(zuordnung$ZGEB2, 3, nchar(zuordnung$ZGEB2))
zuordnung$ZGEB2 <- paste0(9, zuordnung$bez, '01', zuordnung$ZGEB2)
zuordnung <- zuordnung[colnames(zuordnung) %in% c('ZGEB2','bez', 'Graetzl_ID', 'Graetzl_Name')]
zuordnung <- rename(zuordnung, 'zsp_id'='ZGEB2', 'grz_id'='Graetzl_ID', 'grz_name'='Graetzl_Name',
                    'bez_id'='bez')

bezirke <- c(
  'Innere Stadt',
  'Leopoldstadt',
  'Landstraße',
  'Wieden',
  'Margareten',
  'Mariahilf',
  'Neubau',
  'Josefstadt',
  'Alsergrund',
  'Favoriten',
  'Simmering',
  'Meidling',
  'Hietzing',
  'Penzing',
  'Rudolfsheim-Fünfhaus',
  'Ottakring',
  'Hernals',
  'Währing',
  'Döbling',
  'Brigittenau',
  'Floridsdorf',
  'Donaustadt',
  'Liesing')

zuordnung$bez_name <- as.factor(zuordnung$bez_id)
levels(zuordnung$bez_name) <- bezirke
zuordnung$bez_name <- as.character(zuordnung$bez_name)
zuordnung <- zuordnung %>% mutate(across(everything(), as.character))

shape <- st_read("Zaehlsprengel_2021_simple", "Zaehlsprengel_2021_simple")
shape <-  left_join(zuordnung, shape, by = c('zsp_id'='id'))

# Shape Grätzl
shape_grzl <- shape %>% 
  group_by(grz_id, grz_name, bez_id, bez_name) %>% 
  summarize(geometry = st_union(geometry))

# Shape Bezirke
shape_bez <- shape %>% 
  group_by(bez_id, bez_name) %>% 
  summarize(geometry = st_union(geometry))

# Shape Wien
shape_wien <- shape %>% 
  summarize(geometry = st_union(geometry))

}


#======================================== Abspeichern ==============================================
{
st_write(shape_grzl, 'Graetzl_2022_simple', 'Graetzl_2022_simple', driver="ESRI Shapefile", delete_dsn=T,delete_layer=T)
st_write(shape_bez, 'Bezirke_2022_simple', 'Bezirke_2022_simple', driver="ESRI Shapefile", delete_dsn=T,delete_layer=T)
st_write(shape_wien, 'Stadtgrenze_2022_simple', 'Stadtgrenze_2022_simple', driver="ESRI Shapefile", delete_dsn=T,delete_layer=T)
}

# test
if(F) {
leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
  addProviderTiles(providers$CartoDB.VoyagerNoLabels) %>%
  addPolygons(data=shape_grzl$geometry, color = 'black', weight = 1.5, fill=F, fillOpacity = 0)
}

