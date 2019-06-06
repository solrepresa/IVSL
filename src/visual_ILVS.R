# ILVS
# Sol Represa
# 06/06/2019


# Objetivo: visualizar los ILVS
# Por problema de tamaño del file, este archivo solo trabaja con Prov. de Bs As.

library(leaflet)
library(maptools)
library(mapview) #guardar mapas



# # # # # # # # # # # # # # # # # # # # 

## 1) Cargamos datos

# # # # # # # # # # # # # # # # # # # # 

ILVS <- readShapePoly("ILVS_BsAsCABA.shp") # Carga shp
proj4string(ILVS) <- CRS("+proj=longlat +ellps=WGS84 +no_defs") # Asignar proyeccion
slot(ILVS, "data") <- data.frame(Id = ILVS@data$link, 
                                 ILVS = ILVS@data$IVSL_t, 
                                 ILVS_cat = ILVS@data$cat_VS) # Variables de interes



# # # # # # # # # # # # # # # # # # # # 

## 2) Armamos mapa con leaflet ##

# # # # # # # # # # # # # # # # # # # # 


# Colores: por categoria de ILVS
bins <- c(0, 1, 2, 3, 4, 5)
pal <- colorBin("YlOrRd", domain = ILVS$ILVS_cat, bins = bins)


# Label: armadas en HTML
labels <- sprintf(
  "<strong>Radio censal <br/> %s </strong> <br/> ILVS %g ",
  ILVS$Id, ILVS$ILVS_cat
) %>% lapply(htmltools::HTML)


# Mapa base
m <- leaflet(data = ILVS ) %>%
  addProviderTiles("Esri.WorldGrayCanvas",   # mapa base
                   options = tileOptions(minZoom = 1, maxZoom = 15)) %>%
  addMiniMap(position = "bottomleft")     # mini mapa

m <- m %>% addPolygons(fillColor = ~pal(ILVS_cat),
              weight = 1,
              smoothFactor = 0.3, 
              fillOpacity = 0.7,
              color = "white",
              highlight = highlightOptions(    # resalta la seleccion
                weight = 5, 
                bringToFront = TRUE, 
                opacity = 1,
                color = "#666"),
              label = labels,   # agrega etiquetas
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")
              )

m  <- m %>% addLegend(pal = pal, #agrega leyenda
              values = ~ILVS_cat, 
              opacity = 0.7, 
              title = "ILVS",
              position = "bottomright")


mapshot(m, url = paste0(getwd(), "/map_IVSL.html"), selfcontained = FALSE)
