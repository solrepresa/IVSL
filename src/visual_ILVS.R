# ILVS
# Sol Represa
# 06/06/2019


# Objetivo: visualizar los ILVS

library(leaflet)
library(maptools)


ILVS <- readShapePoly("ILVS_wgs84.shp") ## Carga de puntos para extraer la info
proj4string(ILVS) <- CRS("+proj=longlat +ellps=WGS84 +no_defs") # Asignar proyeccion
slot(ILVS, "data") <- data.frame(Id = ILVS@data$link, ILVS = ILVS@data$IVSL_t, ILVS_cat = ILVS@data$cat_VS) # Asigno variables de inter?s (o quito el resto)


## Mapa leaflet ##

# Colores
bins <- c(0, 1, 2, 3, 4, 5)
pal <- colorBin("YlOrRd", domain = ILVS$ILVS_cat, bins = bins)

#Label
labels <- sprintf(
  "<strong>Radio censal <br/> %s </strong> <br/> ILVS %g ",
  ILVS$Id, ILVS$ILVS_cat
) %>% lapply(htmltools::HTML)


m <- leaflet(data = ILVS ) %>%
  addProviderTiles("Esri.WorldGrayCanvas", 
                   options = tileOptions(minZoom=5, maxZoom=7)) %>%
  addMiniMap(position = "bottomright") 

m <- m %>% addPolygons(fillColor = ~pal(ILVS_cat),
              stroke = FALSE, 
              smoothFactor = 0.3, 
              fillOpacity = 0.7,
              highlight = highlightOptions(
                weight = 5, 
                bringToFront = TRUE, 
                opacity = 1,
                color = "#666"),
              label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")
              )

m  <- m %>% addLegend(pal = pal, 
              values = ~ILVS_cat, 
              opacity = 0.7, 
              title = "ILVS",
              position = "bottomright")


library(mapview)
mapshot(m, file= "m.html", selfcontained = FALSE)
