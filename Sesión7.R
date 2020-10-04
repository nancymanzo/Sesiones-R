#leaflet es una paquetería que nos ayuda a crear maps.
library(leaflet)
library(ggplot2)
library(plotly)
library(ggplot2)
library(sf) 
library(cartogram)
library(magick)
library(purrr)
library(tidyverse)
library(dplyr)



#Ejemplos de las funciones básicas de leaflet
leaflet() %>% 
  addTiles() #Es la capa central del mapa


leaflet() %>% 
  addTiles() %>% 
  setView(lng=2.34, lat=48.85, zoom=5) %>% #Manipular la capa del mapa
  addProviderTiles("NASAGIBS.ViirsEarthAtNight2012") #Visualizador


leaflet() %>% 
  addTiles() %>% 
  setView( lng = 2.34, lat = 48.85, zoom = 3 ) %>% 
  addProviderTiles("Esri.WorldImagery") #fondo con estilo de google maps




#Crear un leaflet con datos de la denue
shp <- st_read("INEGI_DENUE_23092020.shp") #Cargo mi archivo shp
csv <- read.csv("INEGI_DENUE_23092020.csv")

    plot(st_geometry(shp)) #se requiere la paquetería sf
    
    names(shp)[names(shp) == "id" ] <- "ID" #Renombrar la variable de Nombre.




# Unión de datos
merge <- merge(shp, csv, by.x = "ID", by.y="ID")



gg09 <- ggplot(data = merge) +
  geom_sf(aes(fill=ID), data = merge, color='gray', size=0.3) +
  guides(fill=guide_colorbar(title = "09/03/2020")) +
  scale_fill_gradient(limits= c(0,3600), high = "#e34a33", low = "#fee8c8", guide = "colorbar") +
  xlab("Longitud") + ylab("Latitud") +
  ggtitle("COV-19 en España",
          subtitle = "Datos:09/03/2020") + 
  theme(plot.title = element_text(family="Comic Sans MS", face="bold"),
        legend.position = "left")


shp %>%
  select(entidad) %>%
  group_by(entidad) %>%
  summarise(total= n())

ggplot(mun) +
  geom_sf(aes(fill = total))+
  labs(title = "Bibliotecas Públicas en México.",
       subtitle = "Año: 2020")+
  theme_classic()


leaflet(shp) %>% 
  addTiles() %>% 
  addMarkers(clusterOptions = markerClusterOptions()) #Agrupa en cluster los datos
    

leaflet(shp)%>% 
  addTiles()%>% 
  addCircleMarkers() #Los datos los deja en aros

leaflet(shp)%>% 
  addTiles()%>% 
  addCircles() #Los datos los deja en circulos



names(providers)#Todas las opciones de mapa

leaflet(shp)%>% 
  addTiles()%>% #Capa del mapa
  addCircles() %>%  #Forma del marcador
  addProviderTiles("Esri.WorldImagery") #Estilo del fondo: 
                                                     # NASAGIBS.ViirsEarthAtNight2012
                                                     # providers$CartoDB.Positron
                                                     # Wikimedia
                                                     # Esri.OceanBasemap



leaflet(shp)%>% 
  addTiles()%>% #Capa del mapa
  addProviderTiles("Esri.WorldImagery") %>% 
  addMarkers(icon = greenLeafIcon)



leaflet(shp) %>% 
  addTiles() %>%
  addRectangles(
    lng1=-103.353468, lat1=20.707660,
    lng2=-103.371911, lat2=20.698110,
    fillColor = "transparent") 



