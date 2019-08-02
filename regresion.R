library(sp)
library(maptools)
library(rgdal)
library(spdep)
library(classInt)
library(RColorBrewer)

setwd("C:/Users/SOCIAL DATA/Social Data Dropbox/Social Data Consulting/ESTADISTICA ESPACIAL/modulo3/data")
base <- read.csv("Doslulas.csv", sep = ";")
setwd("C:/Users/SOCIAL DATA/Social Data Dropbox/Social Data Consulting/ESTADISTICA ESPACIAL/modulo3/data/Shapefile")
mapa1 = readShapePoly("MUNIC.shp")

#Unir Archivos
mapa1@data = merge(mapa1@data,base,by.x="GEOCODIGO",by.y="ï..CODIGO", sort=FALSE)
#Ver Data
head(mapa1@data) 

W_cont_el <- poly2nb(mapa1, queen=T)#queen=True un unico punto de prontera compartido
#cumple la condicion de continuidad.
W_cont_el_mat <- nb2listw(W_cont_el, style="W", zero.policy=TRUE)


library(spdep)
regr_esp__err <- errorsarlm(LULA06_2 ~ BFRENTA6 + TS_URB00 + LULA02_2 + NORTE + NORESTE + SURESTE + C_OESTE, data = mapa1@data, listw = W_cont_el_mat)


summary(soco_err)
