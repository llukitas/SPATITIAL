library (raster)
library(rgdal)

#establecer escritorio donde se tiene el shape file
#El shape se encuentra en la carpeta shapefile de la carpeta data del modulo 3
#trabajaremos con el shape file a nivel distrital de Peru
getwd()
setwd("C:/Users/Personal/Desktop/ESTADÌSTICA ESPACIAL - Full Day/parte_3/codigo/RECORTAR SHAPE FILE")


#cargamos el shape
dist.map <- readOGR(dsn="DISTRITO_07_12_2015.shp")
plot(dist.map)#mostrar mapa

library(mapview)
mapView(dist.map)

head(dist.map@data) # mostrar los 6 primeros valores
names(dist.map@data) #mostrar nombres - trabajaremos con NOMBDEP
region <- c("ICA") #CREAMOS EL OBJETO region que guarda el nombre ICA


#se manipulará el shape
#si desearían trabajar con otro 
dist.map.corte <- dist.map[as.character(dist.map@data$NOMBDEP) %in% region, ]
plot(dist.map.corte)

mapView(dist.map.corte)

ica=as.data.frame(dist.map.corte@data)
names(dist.map.corte)
write.csv2(ica,file = "C:/Users/Personal/Desktop/ESTADÌSTICA ESPACIAL - Full Day/parte_3/data/Shapefile/dataica.csv")
           
#DATAS
getwd()
setwd("C:/Users/Personal/Desktop/ESTADÌSTICA ESPACIAL - Full Day/parte_3/codigo/RECORTAR SHAPE FILE")

ausentismo <- read.csv("ausentismo2011.csv", sep = ";")


#Unir Data dist.map con dos.lulas
dist.map.corte@data = merge(dist.map.corte@data,ausentismo,by.x="IDDIST",by.y="Ubigeo", sort=FALSE)
#mostrar data unida
head(dist.map.corte@data)
str(dist.map.corte@data)
#Convertir de factor a numerica
dist.map.corte@data$Porc_Pob_Aus=as.numeric(dist.map.corte@data$Porc_Pob_Aus)


#VISUALIZACIÓN
library("classInt") #Para discretizar - contiene la funcion classIntervals
library(RColorBrewer)
ausentismo.2011 <- classIntervals(dist.map.corte$Porc_Pob_Aus, n = 4, style = "quantile", intervalClosure = "right")
#Mapa de la votaciÃ³n de Lula 2002 (%Votos por Municipio - 1era Vuelta)
spplot(dist.map.corte, "Porc_Pob_Aus", col = "transparent", col.regions = brewer.pal(4,"Blues"), 
       at = ausentismo.2011$brks, main= "Porcentaje de Ausentismo - Primera Vuelta 2011")
dev.off()
