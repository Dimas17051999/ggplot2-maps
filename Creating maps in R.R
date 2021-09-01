#Creating maps in R 

install.packages("leaflet") #to create interactive maps 
library(leaflet)
install.packages("htmlwidgets")
library(htmlwidgets)
library(htmltools)




map<- leaflet() #creat an empty map 
map <- leaflet() %>% addTiles() #con esa funcion adicional nos permite entrar al mapa del mundo
map <- leaflet() %>% addTiles() %>% addMarkers(lng = -73.9851, lat=40.7589)
                                  #para añadir una ubicacion especifica 
mapTS <- leaflet() %>% addTiles() %>% addMarkers(lng = -73.9851, lat=40.7589, popup = "Times Square")

mapET <- leaflet() %>% addProviderTiles("Stamen.Watercolor") %>% addMarkers(lng = 2.2945, lat=48.8584, popup = "Eiffel Tower")
                      #para generar otro estilo de mapas

#Checar, porque hay varios tipos de mapas 



#ADDING MULTIPLE MARKERS 

head(quakes) #data set con diferentes registros de temblores. 
#Es básico contar con la latitud y longitud del lugar para fijar la ubicacion 

map_multiple <- leaflet(quakes)%>%
                  addTiles()%>%
                  addCircles(lng =  quakes$long, lat= quakes$lat) #funcion para agregar varios marcadores
#se ve muy amontonado

#Ahora los agruparemos para que se vea mejor 
map_cluster <- leaflet(quakes)%>%
                     addTiles()%>%
                    addMarkers(clusterOptions = markerClusterOptions())


#Para marca un area del mapa además del marker 

map_area <- leaflet() %>% addTiles() %>%
            addMarkers(lng=86.92, lat = 27.99, popup = "Mount Everest") %>%
            addRectangles(86.9, 27.95, 87, 28.05 )

 
  saveWidget(map_area, file="map_area.html", selfcontained = F)

  
  



















