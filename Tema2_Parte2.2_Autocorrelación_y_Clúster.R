#####################################################################
#                     Estructuras de Vecindad                       #
#####################################################################

#Leer Archivo de Poligonos
setwd("C:/Users/SOCIAL DATA/Social Data Dropbox/Social Data Consulting/ESTADISTICA ESPACIAL/modulo3/data/Shapefile")
mapa1 = readShapePoly("MUNIC.shp")
head(mapa1@data)
plot(mapa1)
#Leer Archivo base
setwd("C:/Users/SOCIAL DATA/Social Data Dropbox/Social Data Consulting/ESTADISTICA ESPACIAL/modulo3/data")
base <- read.csv("Doslulas.csv", sep = ";")
head(base)
#Unir Archivos
mapa1@data = merge(mapa1@data,base,by.x="GEOCODIGO",by.y="�..CODIGO", sort=FALSE)
#Ver Data
head(mapa1@data) 

#Creando Estructuras de Vecindad
#Número de caso
mapa1<-dist.map
nobs <- length(mapa1)
#Estructuras de vecindad
coords <- coordinates(mapa1)
mapa.nb <- poly2nb(mapa1,queen = FALSE)#Correr para consultar estructura de vecindad
plot(mapa1, border="grey")
#Mostrar el Mapa de vecindad
plot(mapa.nb, coords, add=TRUE, main= "Estructuras de Vecindad - Distritos de Brasil")
dev.off()

## Crear Matriz de Poligonos
map_crd <- coordinates(mapa1)
## Matriz de vecindad por continuidad
W_cont_el <- poly2nb(mapa1, queen=T)#queen=True un unico punto de prontera compartido
#cumple la condicion de continuidad.
W_cont_el_mat <- nb2listw(W_cont_el, style="W", zero.policy=TRUE)
## Mapa de Conecciones de Vecindad
par(mar=rep(0,4))
plot(W_cont_el_mat,coords=map_crd,pch=19, cex=0.1, col="gray")
dev.off()

#####################################################################
#                     Autocorrelacion Espacial                      #
#####################################################################

#AUTOCORRELACION GLOBAL
## Test de Autocorrelación Global: Moran I
#Para la Primera Vuelta de 2002
moran.test(mapa1$LULA02_1, listw=W_cont_el_mat, zero.policy=T)
#Para la Segunda Vuelta de 2002
moran.test(mapa1$LULA02_2, listw=W_cont_el_mat, zero.policy=T)
#Para la Primera Vuelta de 2006
moran.test(mapa1$LULA06_1, listw=W_cont_el_mat, zero.policy=T)
#Para la Segunda Vuelta de 2006
moran.test(mapa1$LULA06_2, listw=W_cont_el_mat, zero.policy=T)

#Grafica de Dispersion de Moran I Global 2da Vuelta 2006
par(mar=c(4,4,1.5,0.5))
moran.plot(mapa1$LULA06_2, listw=W_cont_el_mat, zero.policy=T, xlim=c(0,100),ylim=c(0,100), pch=16, 
           col="black",cex=.5, quiet=F, labels=as.character(mapa1$LULA06_2),xlab="Porcentaje por Lula", 
           ylab="Porcentaje por Lula (Spatial Lag)", main="Moran Scatterplot Lula 2006-2da Vuelta")
dev.off()
#Grafica de Dispersion de Moran I Global 2da Vuelta 2002
par(mar=c(4,4,1.5,0.5))
moran.plot(mapa1$LULA02_2, listw=W_cont_el_mat, zero.policy=T, xlim=c(0,100),ylim=c(0,100), pch=16, 
           col="black",cex=.5, quiet=F, labels=as.character(mapa1$LULA06_2),xlab="Porcentaje por Lula", 
           ylab="Porcentaje por Lula (Spatial Lag)", main="Moran Scatterplot Lula 2002-2da Vuelta")
dev.off()

#AUTOCORRELACION LOCAL

#Lula 2006 2da Vuelta
#Autocorrelacion: Moran I Local (Asumiendo Normalidad)
lm1 <- localmoran(mapa1$LULA06_2, listw=W_cont_el_mat, zero.policy=T)
colnames(lm1)
summary(lm1)
#Mapa de Moran I Local Lula 2006 2da Vuelta
mapa1$lm1 <- abs(lm1[,4]) ## Extract z-scores
lm.palette <- colorRampPalette(c("white","yellow","orange", "red"), space = "rgb")
spplot(mapa1, zcol="lm1", col.regions=lm.palette(20), 
       main="Moran I Local Lula 2006 2da Vuelta (|z| scores)", pretty=T)
dev.off()

#Lula 2002 2da Vuelta
#Autocorrelación: Moran I Local (Asumiendo Normalidad)
lm2 <- localmoran(mapa1$LULA02_2, listw=W_cont_el_mat, zero.policy=T)
colnames(lm2)
summary(lm1)
#Mapa de Moran I Local Lula 2006 2da Vuelta
mapa1$lm2 <- abs(lm2[,4]) ## Extract z-scores
lm.palette <- colorRampPalette(c("white","yellow","orange", "red"), space = "rgb")
spplot(mapa1, zcol="lm2", col.regions=lm.palette(20), 
       main="Moran I Local Lula 2002 2da Vuelta (|z| scores)", pretty=T)
dev.off()


names(base)


