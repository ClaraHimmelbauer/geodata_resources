
packages <- c("sf", "readxl", "dplyr", "leaflet", "leafem", "htmltools", "htmlwidgets", "xml2")
lapply(packages, library, character.only=TRUE)


rm(list=ls())

setwd("X:/3_Projekte-SOLL/2022/Microtargeting/Baugrundpreise")


#-----------------------------------------------------------------------------------------------------------------------#
# Datensachen
#-----------------------------------------------------------------------------------------------------------------------#

shape <- st_read("X:/Geodaten/aaa_Österreich/Verwaltungsgrenzen_vereinfacht", "Gem_simple")
Bezirke <- st_read("X:/Geodaten/aaa_Österreich/Verwaltungsgrenzen_vereinfacht", "Bez_lines")
BL <- st_read("X:/Geodaten/aaa_Österreich/Verwaltungsgrenzen_vereinfacht", "BL")

BL <- st_transform(BL, crs = 4326)

#---------------------------------#
# Matrei am Brenner zusammenfügen #
#---------------------------------#

shape <- shape[, colnames(shape) %in% c("GKZ", "BL", "BKZ", "Name", "Bev", "EK")]

geometry <- shape[shape$GKZ %in% c(70327, 70330, 70341), ]
geometry <- st_union(geometry)
geometry <- st_cast(geometry, "MULTIPOLYGON")
geometry <- st_sf(geometry)
geometry$GKZ <- 70370
geometry$BL <- 7
geometry$BKZ <- 703
geometry$Name <- "Matrei am Brenner"
geometry$Bev <- 3590
geometry$EK <- 36658
st_crs(geometry) <- st_crs(shape)
geometry <- geometry %>% relocate(geometry, .after = EK)
# df_matrei <- data.frame("GKZ" = 70370, "BL" = 7, "BKZ" = 703, "Name" = "Matrei am Brenner", "Bev" = 3590, "EK" = 36658, "geometry" = matrei)
# colnames(matrei)[1] <- "geometry"
shape <- rbind(shape, geometry)

shape <- shape[!(shape$GKZ %in% c(70327, 70330, 70341)), ]


#----------------------------------------#

data <- read_xlsx("baugruende_neu.xlsx", sheet = "Gemeinden")
shape <- left_join(shape, data, by = c("GKZ" = "ID"))

shape <- subset(shape, shape$GKZ >= 70000 & shape$GKZ < 80000)
Bezirke <- subset(Bezirke, Bezirke$BKZ >= 700 & Bezirke$BKZ < 800)

#-----------------------------------------------------------------------------------------------------------------------#
# Jahre für 500m²
#-----------------------------------------------------------------------------------------------------------------------#

shape$Jahre_aufrunden <- shape$Baugrund_21_schaetz * 500 / shape$Jahresbruttobezug
shape$Jahre_aufrunden <- ceiling(shape$Jahre_aufrunden)

hist(shape$Jahre_aufrunden, breaks = 35)

col1 <- "#fcc24b"
col2 <- "#FA8C00"
col3 <- "#E3420C"
col4 <- "#C71E1D"
col5 <- "#97193C"
col6 <- "#5F0E4F"

shape$col <- ifelse(shape$Jahre_aufrunden <= 1, col1,
                    ifelse(shape$Jahre_aufrunden >= 2 & shape$Jahre_aufrunden <= 2, col2,
                           ifelse(shape$Jahre_aufrunden >= 3 & shape$Jahre_aufrunden <= 4, col3,
                                  ifelse(shape$Jahre_aufrunden >= 5 & shape$Jahre_aufrunden <= 6, col4,
                                         ifelse(shape$Jahre_aufrunden >= 7 & shape$Jahre_aufrunden <= 10, col5, col6)))))

shape$EK_map <- format(shape$Jahresbruttobezug, big.mark = ".")
shape$Quadratmeterpreis_map <- format(round(shape$Baugrund_21_schaetz), big.mark = ".")
shape$labels <- paste0("<b>", shape$Name, ":</b><br>",
                       "Durchschnittliches Bruttoeinkommen 2021: ", shape$EK_map, " € <br>",
                       "Durchschnittlicher Preis für 1m² Baugrund 2021: ", shape$Quadratmeterpreis_map, " € <br>",
                       "Arbeitsjahre für 500m² Baugrund: ", round(shape$Jahre_aufrunden), " Jahre")
shape$labels2 <- paste0("<b>", shape$Name, ":</b> ", round(shape$Jahre_aufrunden), ifelse(shape$Jahre_aufrunden <= 1, " Jahr", " Jahre"))
# shape$labels2 <- gsub("1 Jahre", "1 Jahr", shape$labels2)
# shape$labels2 <- gsub("11 Jahr", "1 Jahre", shape$labels2)


tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    position: fixed !important;
    left: 50px;
    text-align: left;
    padding-left: 10px; 
    padding-right: 10px; 
    background: rgba(255,255,255,1);
    font-weight: bold;
    font-size: 20px;
    border-radius: 5px;
    box-shadow: 0 1px 5px rgba(0,0,0,0.4);
  }
"))
title <- tags$div(
  tag.map.title, HTML("Baugrundpreis in Tirol <br>
                      <small>Wie lange muss man für 500m² arbeiten"))


m <- leaflet() %>%
  addProviderTiles(providers$CartoDB.VoyagerNoLabels) %>%
  addPolygons(data=shape, color = 'black', weight = 1.5, fillColor = shape$col, fillOpacity = .7,
              label = lapply(shape$labels2, htmltools::HTML)) %>%
  addPolylines(data = Bezirke, color = "black", weight = 3, opacity = 1, fillOpacity = 0) %>%
  
  addLegend(colors = c(col1, col2, col3, col4, col5, col6),
            title = "Wie viele Jahre muss man für 500m²<br>
                    Baugrund arbeiten? <br>
                    <small>Basis: Daten zu Durchschnittseinkommen der <br>
                    ganzjährig Beschäftigten und Baugrundpreisen
                    <br align = 'right'><b><a href = 'https://www.ogm.at/'>www.ogm.at</a></b> 08/22 ",
            labels = c( "1 Jahr",   "2 Jahre","3-4 Jahre","5-6 Jahre", "7-10 Jahre", "11 oder mehr Jahre"),
            position = "topright", opacity = .8) %>%
  
  leafem::addLogo(img = 'https://www.ogm.at/wp-content/uploads/2020/09/ogm_researchandcomm.jpg',
                  src = "remote", url = 'https://www.ogm.at/',
                  position = "bottomright", width = 160, height = 27.5, offset.x = 0, offset.y = 15)
m

saveWidget(m, "raw.html", selfcontained = T)
source("X:/Geodaten/zzz_change_css/change_css_codes_xml2.R")
change_legend_css("raw.html", "Baugrundpreise_Tirol_Liste_Fritz.html")


# statischer plot

# Code für statischer Plot Niederösterreich
{
  Krems <- SpatialPoints(coordinates(gCentroid(Gem[Gem$BKZ == 301, ])),
                         proj4string = CRS("+proj=longlat +datum=WGS84"))
  St.Poelten <- SpatialPoints(coordinates(gCentroid(Gem[Gem$BKZ == 302, ])),
                              proj4string = CRS("+proj=longlat +datum=WGS84"))
  Waidhofen <- SpatialPoints(coordinates(gCentroid(Gem[Gem$BKZ == 303, ])),
                             proj4string = CRS("+proj=longlat +datum=WGS84"))
  Wr.Neustadt <- SpatialPoints(coordinates(gCentroid(Gem[Gem$BKZ == 304, ])),
                               proj4string = CRS("+proj=longlat +datum=WGS84"))
  
  plot(Graph_Bez)
  plot(gem1, col = col1, add = T)
  plot(gem2, col = col2, add = T)
  plot(gem3, col = col3, add = T)
  plot(gem4, col = col4, add = T)
  plot(gem5, col = col5, add = T)
  plot(gem6, col = col6, add = T)
  plot(Graph_Bez, add = T, lwd = 2)
  
  text(Krems, labels = paste("Krems an", "der Donau", sep = "/"), col = "white", cex = 0.5, border = "black")
  text(St.Poelten, labels = "St.Pölten", col = "white", cex = 0.5, border = "black")
  text(Waidhofen, labels = paste("Waidhofen an",  "der Ybbs", sep = "/"), col = "white", cex = 0.5, border = "black")
  text(Wr.Neustadt, labels = paste("Wiener", "Neustadt", sep = "/"), col = "white", cex = 0.5, border = "black")
  
  
  
  plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
  legend("topleft", legend =c("unter 250 m²", "250-500 m²", "500-750 m²", "750-1000 m²", "1000-1500 m²", "über 1500 m²"), pch=15, pt.cex=2.5, cex=1, bty='n',
         col = c(col1, col2, col3, col4, col5, col6))
  mtext(paste("Leistbarkeit von Baugrund", "für 1 Jahresgehalt", sep = ""), at = 0.35, cex=1.5)
  
  plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
  legend("topleft", legend =c("unter 250 m²", "250-500 m²", "500-750 m²", "750-1000 m²", "1000-1500 m²", "über 1500 m²"), pch=15, pt.cex=5, cex=2.5, bty='n',
         col = c(col1, col2, col3, col4, col5, col6))
}

plot(shape[17], col = shape$col, main = NA)
legend(0, 1,
       legend = c( "1 Jahr",   "2 Jahre","3-4 Jahre","5-6 Jahre", "7-10 Jahre", "11 oder mehr Jahre"),
       fill = c(col1, col2, col3, col4, col5, col6),
       border = NA, bty = "n", bg = NA, ncol = 2,
       title = "Wie lange muss man für 500m² Baugrund arbeiten?")


# Niederösterreich Code adaptieren, vielleicht ist das schöner
# ok nein, das funktioniert nicht
{
library(sp)
library(rgeos)
library(maptools)
library(rgdal)

shape_2 <- as_Spatial(shape)
Innsbruck <- SpatialPoints(coordinates(gCentroid(shape_2[shape_2$BKZ.x == 701, ])), proj4string = CRS("+proj=longlat +datum=WGS84"))
Imst <- SpatialPoints(coordinates(gCentroid(shape_2[shape_2$Name == "Imst", ])), proj4string = CRS("+proj=longlat +datum=WGS84"))
Kitzbühel <- SpatialPoints(coordinates(gCentroid(shape_2[shape_2$Name == "Kitzbühel", ])), proj4string = CRS("+proj=longlat +datum=WGS84"))
Kufstein <- SpatialPoints(coordinates(gCentroid(shape_2[shape_2$Name == "Kufstein", ])), proj4string = CRS("+proj=longlat +datum=WGS84"))
Landeck <- SpatialPoints(coordinates(gCentroid(shape_2[shape_2$Name == "Landeck", ])), proj4string = CRS("+proj=longlat +datum=WGS84"))
Lienz <- SpatialPoints(coordinates(gCentroid(shape_2[shape_2$Name == "Lienz", ])), proj4string = CRS("+proj=longlat +datum=WGS84"))
Reutte <- SpatialPoints(coordinates(gCentroid(shape_2[shape_2$Name == "Reutte", ])), proj4string = CRS("+proj=longlat +datum=WGS84"))
Schwaz <- SpatialPoints(coordinates(gCentroid(shape_2[shape_2$Name == "Schwaz", ])), proj4string = CRS("+proj=longlat +datum=WGS84"))

shape_2 <- spTransform(shape_2, "+proj=longlat +datum=WGS84")

gem1 <- shape_2[shape_2$col == col1, ]
gem2 <- shape_2[shape_2$col == col2, ]
gem3 <- shape_2[shape_2$col == col3, ]
gem4 <- shape_2[shape_2$col == col4, ]
gem5 <- shape_2[shape_2$col == col5, ]
gem6 <- shape_2[shape_2$col == col6, ]

plot(shape_2)
plot(gem1, col = col1, add = T)
plot(gem2, col = col2, add = T)
plot(gem3, col = col3, add = T)
plot(gem4, col = col4, add = T)
plot(gem5, col = col5, add = T)
plot(gem6, col = col6, add = T)
plot(shape_2, add = T, lwd = 1)

# text(Krems, labels = paste("Krems an", "der Donau", sep = "/"), col = "white", cex = 0.5, border = "black")
# text(St.Poelten, labels = "St.Pölten", col = "white", cex = 0.5, border = "black")
# text(Waidhofen, labels = paste("Waidhofen an",  "der Ybbs", sep = "/"), col = "white", cex = 0.5, border = "black")
# text(Wr.Neustadt, labels = paste("Wiener", "Neustadt", sep = "/"), col = "white", cex = 0.5, border = "black")

# error: cannot coerce type 'S4' to vector of type 'double'
text(Innsbruck, labels = "Innsbruck", col = "white", cex = 0.5, border = "black")
text(Imst, labels = "Imst", col = "white", cex = 0.5, border = "black")
text(Kitzbühel, labels = "Kitzbühel", col = "white", cex = 0.5, border = "black")
text(Kufstein, labels = "Kufstein", col = "white", cex = 0.5, border = "black")
text(Landeck, labels = "Landeck", col = "white", cex = 0.5, border = "black")
text(Lienz, labels = "Lienz", col = "white", cex = 0.5, border = "black")
text(Reutte, labels = "Reutte", col = "white", cex = 0.5, border = "black")
text(Schwaz, labels = "Schwaz", col = "white", cex = 0.5, border = "black")


# plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
# legend("topleft", legend =c("unter 250 m²", "250-500 m²", "500-750 m²", "750-1000 m²", "1000-1500 m²", "über 1500 m²"), pch=15, pt.cex=2.5, cex=1, bty='n',
#        col = c(col1, col2, col3, col4, col5, col6))
# mtext(paste("Leistbarkeit von Baugrund", "für 1 Jahresgehalt", sep = ""), at = 0.35, cex=1.5)
# 
# plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
# legend("topleft", legend =c("unter 250 m²", "250-500 m²", "500-750 m²", "750-1000 m²", "1000-1500 m²", "über 1500 m²"), pch=15, pt.cex=5, cex=2.5, bty='n',
#        col = c(col1, col2, col3, col4, col5, col6))
}