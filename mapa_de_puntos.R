install.packages("googleVis", dependencies = T)

# Cargar librerías de los Paquetes instalados
library(rJava)
library(XLConnectJars)
library(XLConnect)
library(sp) #Análisis Espacial
library(maps) #Para generar mapas
library(rgeos) 
library(gtable)
library(scales)
library(ggplot2)
library(rgdal) #Para manejar archivos de la biblioteca GDAL
library(foreign)
library(maptools) #Para manejar SHAPEFILES
library(RColorBrewer)
library(classInt)
library(geoR)
library(Matrix) #Paquete requerido por spdep
library(spdep) #Para establecer estructuras de vecindad
library(RANN) #Para establecer estructuras de vecindad
library(LearnBayes)
library(googleVis) #para interactividad con mapa y tabla
library(plyr)#contiene la función count

############ Cargar el Archivo de Polígonos y la Base de Datos  ############

#Programar el directorio de trabajo
setwd("C:/Users/manue/Desktop/Mapas")
#Cargar el archivo de polígonos
prov.map <- readOGR(dsn="C:/Users/manue/Desktop/Mapas",layer="PROVINCIA_07_12_2015")
plot(prov.map)
#Mostrar los primeros 6 valores
head(prov.map@data)

prov.ggmap <- fortify(prov.map, region = "IDPROV")
#La función fortify transforma un objeto de R (en este caso dist.map) en un data frame con el fin
#de poder usarlo para graficar la información.

#Abrir data Dos Lulas
#Data que se graficará
voto.cap <- readWorksheet(loadWorkbook("C:/Users/manue/Desktop/Mapas/Proceso-Keiko.xlsx"),sheet=3)
#Mostrar los primeros 6 valores de la data Dos Lulas
head(voto.cap)
names(voto.cap)

#Unir Data dist.map con dos.lulas
prov.map@data = merge(prov.map@data,voto.cap,by.x="IDPROV",by.y="IDPROV", sort=FALSE)
#mostrar data unida
head(prov.map@data)

############ PPK - Mapa Voto en las Capitales de Provincias  ############

#creo una paleta de colores 
#basada en http://colorbrewer2.org/ 9-class Oranges
#pero sin los colores iniciales para que no quede muy claro el mapa
paleta <- colorRampPalette(c("#fdd0a2","#fd8d3c","#d94801","#7f2704"))

spplot(prov.map,"X..Votos.Válidos.PPK", col.regions=paleta(30),
       main=list(label="PPK - Intensidad del Voto en las Capitales de Provincias",cex=1.5),
       sub=list(label="www.cipol-peru.org/estratega",cex=0.5))

Geo <- gvisGeoChart(voto.cap, locationvar='IDPROV',hovervar = 'capital', colorvar='X..Votos.Válidos.PPK', options=list(height=500, width=600, magnifyingGlass.enable=TRUE, magnifyingGlass.zoomFactor=8)) 
plot(Geo)
Tbl <- gvisTable(voto.cap[,1:2], options=list(height=500, width=300))

plot(gvisMerge(Geo, Tbl, horizontal=TRUE))

############ KF - Mapa Voto en las Capitales de Provincias  ############






############ PPK - Mapa Tematico - SIMBOLOS PROPORCIONALES  ############
#valor mínimo
min(prov.map$X..Votos.Válidos.PPK)
#Valor maximo
max(prov.map$X..Votos.Válidos.PPK)
#estructura
str(prov.map$X..Votos.Válidos.PPK)
# Convierte el mapa de poligonos en a un conjunto de puntos (centroides)
ccaapt <- SpatialPointsDataFrame(prov.map, data.frame(prov.map))
ccaapt <- ccaapt[ order( ccaapt$X..Votos.Válidos.PPK ),  ]

# Define el tamano maximo del tamano del punto a ser representado
nmax <- 1
# Calculo del tamano de cada punto (con base en el tamano maximo de punto previamente definido)
# Dos tipos:  Matematico - cada cual es representado por su valor relativo de la variable en cuestion
#             Perceptual - Reduce el peso visual de los valores mas grandes
ccaapt$size <- (ccaapt$X..Votos.Válidos.PPK/max(ccaapt$X..Votos.Válidos.PPK, na.rm=F))^0.5*nmax       # Matematico

# Calculo del tamaño, de la legenda y del orden de diseño de los elementos
legs <-round(quantile(ccaapt$X..Votos.Válidos.PPK,seq(0,1,1/3), na.rm=T),digits = 2)
#rig <- ccaapt$X..Votos.Válidos.PPK
ri <- (legs/max(legs, na.rm=T))^0.5*nmax      # Matematico
#ri <- (legs/max(legs, na.rm=T))^0.57*nmax # Perceptual

# Establece el orden de diseno de los mayores a los menores para evitar que la superposicion oculte puntos.
ccaapt$plotOrder<- order(ccaapt$size, decreasing=F)


# Dibuja el mapa de las provincias del Peru
plot(prov.map, col="wheat1",main= "PPK - Intensidad del Voto en las Capitales de Provincias")
#color <- brewer.pal(3,"Blues")#paleta de 3 colores
color <- c("steelblue3","steelblue3", "blue3")
plot(ccaapt, cex=ccaapt$size, col="black", bg=color[findInterval(ccaapt$X..Votos.Válidos.PPK, legs, all.inside = TRUE)], pch=21, add=T)
#add=T sirve para superponer el plot de datos con el plot del mapa de poligonos
#bg= establece el color de fondo del tipo de pch seleccionado (solo del 21:25).
#cex es la expansion de caracteres 
#findInterval = busca en ccaapt$X..Votos.Válidos.PPK los intervalos establecidos en legs, que son tres.
    #para establecer la paleta de colores fijada en el objeto color.

# Formatea los valores para que sean mas legibles
legs <- format(legs, big.mark=".")
legend("bottomleft", legend=legs[2:length(legs)], col="black", pt.bg=color,pch=21, cex=1, pt.cex=ri[2:length(ri)], bty="n", bg="transparent", y.intersp=.7, x.intersp=.7, border= "black", title = "Intensidad del Voto")


############ KF - Mapa Tematico - SIMBOLOS PROPORCIONALES  ############
#valor mínimo
min(prov.map$X..Votos.Válidos.kF)
#Valor maximo
max(prov.map$X..Votos.Válidos.kF)
#estructura
str(prov.map$X..Votos.Válidos.kF)
# Convierte el mapa de poligonos en a un conjunto de puntos (centroides)
ccaapt <- SpatialPointsDataFrame(prov.map, data.frame(prov.map))
ccaapt <- ccaapt[ order( ccaapt$X..Votos.Válidos.kF),  ]

# Define el tamano maximo del tamano del punto a ser representado
nmax <- 1
# Calculo del tamano de cada punto (con base en el tamano maximo de punto previamente definido)
# Dos tipos:  Matematico - cada cual es representado por su valor relativo de la variable en cuestion
#             Perceptual - Reduce el peso visual de los valores mas grandes
ccaapt$size <- (ccaapt$X..Votos.Válidos.kF/max(ccaapt$X..Votos.Válidos.kF, na.rm=F))^0.5*nmax       # Matematico

# Calculo del tamaño, de la legenda y del orden de diseño de los elementos
legs <-round(quantile(ccaapt$X..Votos.Válidos.kF,seq(0,1,1/3), na.rm=T),digits = 2)
#rig <- ccaapt$X..Votos.Válidos.PPK
ri <- (legs/max(legs, na.rm=T))^0.5*nmax      # Matematico
#ri <- (legs/max(legs, na.rm=T))^0.57*nmax # Perceptual

# Establece el orden de diseno de los mayores a los menores para evitar que la superposicion oculte puntos.
ccaapt$plotOrder<- order(ccaapt$size, decreasing=F)


# Dibuja el mapa de las provincias del Peru
plot(prov.map, col="wheat1",main= "KF - Intensidad del Voto en las Capitales de Provincias")
#color <- brewer.pal(3,"Blues")#paleta de 3 colores
color <- c("yellow","green", "red")
plot(ccaapt, cex=ccaapt$size, col="black", bg=color[findInterval(ccaapt$X..Votos.Válidos.kF, legs, all.inside = TRUE)], pch=21, add=T)
#add=T sirve para superponer el plot de datos con el plot del mapa de poligonos
#bg= establece el color de fondo del tipo de pch seleccionado (solo del 21:25).
#cex es la expansion de caracteres 
#findInterval = busca en ccaapt$X..Votos.Válidos.PPK los intervalos establecidos en legs, que son tres.
#para establecer la paleta de colores fijada en el objeto color.

# Formatea los valores para que sean mas legibles
legs <- format(legs, big.mark=".")
legend("bottomleft", legend=legs[2:length(legs)], col="black", pt.bg=color, pch=21, cex=1, pt.cex=ri[2:length(ri)], bty="n", bg="transparent", y.intersp=.7, x.intersp=.7, border= "black", title = "Intensidad del Voto")


