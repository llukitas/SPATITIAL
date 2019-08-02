rm(list=ls())

# Instalación de Paquetes
install.packages(c("sp","spdep","gstat","maps","rgeos","pgirmess",
                   "splancs","ggplot2", "rgdal","foreign","maptools","RColorBrewer","classInt",
                   "geoR","Matrix","spdep","RANN","LearnBayes","spatstat","googleVis","plyr",
                   dependencies = c("Depends")))

library(sp) #Análisis Espacial: Paquete con identificación básica de 
#los objetos espaciales y que contiene los métodos para la representación 
#visual de datos bajo la forma de mapas. Permite la selección de geometr�???as, 
#la creación de objetos de tipo SpatialDataFrame, la obtención de coordenadas 
#geográficas, la superposición entre geograf�???as y el cálculo de distancias.
library(spdep) # Autocorelación Espacial
library(pgirmess) # Autocorelación Espacial
library(gstat) # Geoestad�???stica
library(splancs) # Densidad de Kernel
library(maps) #Para generar mapas. Contiene mapas predefinidos de las 
#fronteras de pa�???ses de todo el mundo, además de un conjunto de datos de ejemplo
library(rgeos) 
library(gtable) #Tablas
library(scales) #Discretización.
library(ggplot2) #Visusalización.
library(rgdal) #Para manejar archivos de la biblioteca GDAL. Complementa a maptools.
library(foreign) #Importar bases de datos
library(maptools) #Para manejar SHAPEFILES. Permite la importación y exportación de 
#archivos originados en distintos Sistemas de Información Geográfica (SIG), as�??? 
#como permite la manipulación de objetos como la unión, fusión y agregación de mapas.
library(RColorBrewer) #Visualización.
library(classInt) #Intervalos de clase.
library(geoR)#Análisis de Data Geoestadistica.
library(Matrix) #Paquete requerido por spdep.
library(spdep) #Para establecer estructuras de vecindad.
library(RANN) #Para establecer estructuras de vecindad.
library(LearnBayes)
library(spatstat)#Paquete destinado al análisis de patrones de puntos en R. Contiene 
#funciones destinadas a la creación de patrones de puntos, el análisis exploratorio, 
#la identificación de clusters y el contraste de modelos. Entre las funciones más 
#interesantes se encuentran el cálculo de densidad de Kernel, la identificación de 
#regiones de interés (convex hull), la extracción de coordenadas geográficas, la 
#creación de ventanas de observación (objetos owin) y la obtención de matrices de 
#vecindad entre objetos de distinto tipo.
library(googleVis) #para interactividad con mapa y tabla
library(plyr)#contiene la función count

#####################################################################
#   Cargar el Archivo de Poligonos y las Bases de Datos             #
#####################################################################

#D#ATAS
#setwd("C:/Users/SOCIAL DATA/Dropbox/ESTADISTICA ESPACIAL/modulo3/data")
rm(list=ls())
setwd("C:/Users/Personal/Desktop/ESTAD�STICA ESPACIAL - Full Day/parte_3/data")
crimen <- read.csv("Crimen.csv", sep = ",")
dos.lulas <- read.csv("Doslulas.csv", sep = ";")
voto.cap <- read.csv("Capitales.csv",sep = ",")

#ARCHIVOS DE POLIGONOS
setwd("C:/Users/Personal/Desktop/ESTAD�STICA ESPACIAL - Full Day/parte_3/data/Shapefile")

#MAPA DISTRITOS DE BRASIL
dist.map <- readOGR(dsn="MUNIC.shp")
plot(dist.map)#mostrar mapa
library(mapview)
mapView(dist.map)

dev.off()
head(dist.map@data)#mostrar los seis primeros valores
dist.ggmap <- fortify(dist.map, region = "GEOCODIGO")
#La funcion fortify transforma un objeto de R (en este caso dist.map) en un data frame con el fin
#de poder usarlo para graficar la informacion.

#MAPAS PROVINCIAS DEL PERU
prov.map <- readOGR(dsn="C:/Users/Personal/Desktop/ESTAD�STICA ESPACIAL - Full Day/parte_3/data/Shapefile",layer="PER_prov")
plot(prov.map)
dev.off()
head(prov.map@data)
prov.ggmap <- fortify(prov.map, region = "IDPROV")

#####################################################################
#         Fundir Archivos de Poligonos y Bases de Datos             #
#####################################################################

#Unir Data dist.map con dos.lulas
dist.map@data = merge(dist.map@data,dos.lulas,by.x="GEOCODIGO",by.y="�..CODIGO", sort=FALSE)
#mostrar data unida
head(dist.map@data)

#Unir Data dist.map con dos.lulas
prov.map@data = merge(prov.map@data,voto.cap,by.x="IDPROV",by.y="IDPROV", sort=FALSE)
#mostrar data unida
head(prov.map@data)

#####################################################################
#         Entendimiento de la data: Analisis Descriptivo            #
#####################################################################
#Para conocer la Estructura de Datos de la data Dos LUlas:
str(dos.lulas) #No siempre R Lee las variables según su tipo, por 
#lo quedebemos indicarles en algunos casos
#Indicaremos a R que las variables NORTE, NORESTE, SURESTE, SUR y 
#C_OESTE son factores o fariables dummy(dicotómicas)
dos.lulas$NORTE<-factor(dos.lulas$NORTE)
dos.lulas$NORESTE<-factor(dos.lulas$NORESTE)
dos.lulas$SURESTE<-factor(dos.lulas$SURESTE)
dos.lulas$SUR<-factor(dos.lulas$SUR)
dos.lulas$C_OESTE<-factor(dos.lulas$C_OESTE)
#Consultamos la nueva estructura de datos
str(dos.lulas) 

#PARA EL ANALISIS DESCRIPTIVO EN R VER EL SCRIP ANALISISESPACIAL


#####################################################################
#                  Visualizacion de Data Espacial                   #
#####################################################################
#######################################
#   VISUALIZACION DE DATA DE PUNTOS   #
#######################################
#Crear una matrix de coordenadas
sp_point <- cbind(crimen$LONG, crimen$LAT)
colnames(sp_point) <- c("LONG","LAT")
head(sp_point)
## Proyeccion: Sistema de coordenadas universal transversal de Mercator (UTM) Zona 17
proj <- CRS("+proj=utm +zone=17 +datum=WGS84")
#Crear el objeto espacial
data.sp <- SpatialPointsDataFrame(coords=sp_point,crimen,proj4string=proj)
## Caja delimitadora de los data de puntos
bbox(data.sp)
#Elaboracion del grafico de localizacion de la criminalidad
par(mar=c(2,2,0.2,0.2))
plot(data.sp,pch=16, cex=.5, axes=T)
dev.off()

library(leaflet)
leaflet() %>%
  addTiles() %>%
  addMarkers(lat= crimen$LAT, lng = crimen$LONG, popup="Puntos de Crimen") 
#######################################
#  VISUALIZACION DE DATA DE POLIGONOS #
#######################################
str(dist.map$LULA02_1)

dist.map$LULA02_1 = is.numeric(dist.map$LULA02_1)
#2002
#Lula 2002 - 1era Vuelta
lula.02.1 <- classIntervals(dist.map$LULA02_1, n = 4, style = "quantile", intervalClosure = "right")
#Mapa de la votación de Lula 2002 (%Votos por Municipio - 1era Vuelta)
spplot(dist.map, "LULA02_1", col = "transparent", col.regions = brewer.pal(4,"Blues"), 
       at = lula.02.1$brks, main= "Lula 2002 - Primera Vuelta")
dev.off()

franja<- round(quantile(dist.map$LULA02_1, probs = seq(0, 1, 0.25), na.rm = T), digits = 2)
lula.02.1_char <- as.character(franja[2:5])
color <- brewer.pal(4,"Blues")
plot(dist.map, col = color[findInterval(dist.map$LULA02_1, franja, all.inside = TRUE)], main= "Lula 2002 - Primera Vuelta")
legend("bottomleft", legend = lula.02.1_char, fill = color, cex = 1, pt.cex = cex, title = "Intensidad del Voto")
dev.off()

help(legend)#ayuda

#Lula 2002 - 2da Vuelta
lula.02.2 <- classIntervals(dist.map$LULA02_2, n = 4, style = "quantile", intervalClosure = "right")
#Mapa de la votación de Lula 2002 (%Votos por Municipio - 2da Vuelta)
spplot(dist.map, "LULA02_2", col = "transparent", col.regions = brewer.pal(4,"Blues"), at = lula.02.2$brks, 
       main= "Lula 2002 - Segunda Vuelta")
dev.off()

#2006
#Lula 2006 - 1era Vuelta
lula.06.1 <- classIntervals(dist.map$LULA06_2, n = 4, style = "quantile", intervalClosure = "right")
#Mapa de la votaci?n de Lula 2002 (%Votos por Municipio - 1era Vuelta)
spplot(dist.map, "LULA06_2", col = "transparent", col.regions = brewer.pal(4,"Blues"), at = lula.06.1$brks, 
       main= "Lula 2006 - Primera Vuelta")
dev.off()

#Lula 2006 - 2da Vuelta
lula.06.2 <- classIntervals(dist.map$LULA06_2, n = 4, style = "quantile", intervalClosure = "right")
#Mapa de la votaci?n de Lula 2002 (%Votos por Municipio - 2da Vuelta)
spplot(dist.map, "LULA06_2", col = "transparent", col.regions = brewer.pal(4,"Blues"), at = lula.06.2$brks, 
       main= "Lula 2006 - Segunda Vuelta")
dev.off()

