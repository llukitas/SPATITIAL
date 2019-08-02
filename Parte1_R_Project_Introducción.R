#####################################################################
#                     Introduccion a R Project                      #
#####################################################################
############              Instalacion                    ############
############               Funciones                     ############
############                Objetos                      ############
#####################################################################

#instalar el paquete agricolae (desarrollado por un prof. Agraria)
install.packages("agricolae")

#Cargar la libreria que contiene el paquete
library(agricolae)

#Operadores binarios
234+123#suma
234-123#resta
55*21#multiplicacion
55/21#division
4^3#potenciacion
146%%12#modulo: resto de dividir un numero por otro

#funciones matematicas
abs(118-129)#valor absoluto
sqrt(16)#raiz cuadrada
log(124)#logaritmo 
exp(12)#exponencial
log10(124)#logaritmo base 10
factorial(12)#factorial

#FUnciones trigonometricas
sin(pi/2)#seno
cos(pi/2)#coseno
tan(pi/2)#tangente

#Funciones de redondeo
options(digits=16)
1/3
round(1/3,2)
pi
round(pi,2)

#limpiar
rm(list=ls())

#Función media mean()
x <- c(0,5,7,9,1,2,8)
x
mean(x)

#Función ordenar = sort()
y <- c(4,2,0,9,5,3,10)
y
sort(y)
sort(y, decreasing=TRUE)

#Ayuda help()
?mean
help(mean)
help.search("mean")
example(mean)

#necesitamos construir una nube de palabras
install.packages(("tm"))
library(tm)
help(package=tm)

#Que directorio
getwd()
#Establecer el escritorio
#Ejemplo
setwd("C:/Users/SOCIAL DATA/Social Data Dropbox/Social Data Consulting/ESTADISTICA ESPACIAL/modulo1")

#simulando 1000 números con una distribucion normal
x <- rnorm(1000)
#revisar los objetos en la memoria
objects()
#lista de objetos
ls()

#Limpiar
rm(list=ls(all=TRUE))

#################################################################
###########           Introducción            ###################
########### Manipulación de Base de Datos en R ##################
#################################################################

# Creación de una  pequeña base de  datos en R

cigarros <- c(15, 19, 14, 12, 15, 18, 19, 14, 0 ,16, 15, 1 ,1, 11, 19, 11)
id <-	1:16 
sexo <- c(0, 1, 1, 0,0,1,0,1,1,1,0,0,1,1,0,0)
nombres <- c ("Ana", "Antonio", "David", "Filipa", "Joana","Joao", "Maria", "Paulo", "Pedro", "Ricardo", "Rita", "Rute", "Rui", "Bruno", "Albertina", "Vanesa")
BaseCig <- data.frame(id,   nombres, sexo, cigarros) #Creación de una hoja de datos (data frame)

class(BaseCig) 

BaseCig #Visualizar la data creada

######################################
#### Importar base de datos con R ####
######################################

# Importar Base1

install.packages("readr") #Para importar base de datos en formato CSV
library(readr)    # Llamar al paquete para implementarlo

# Establecer el directorio del archivo que vamos a importar

Base1 <- read_csv("C:/Users/SOCIAL DATA/Social Data Dropbox/Social Data Consulting/ESTADISTICA ESPACIAL/modulo1/data/Base1.csv")
Base1 <- as.data.frame (Base1)
class(Base1)

# Exploración de la base

Base1
S <-Base1   
S  

#Se establece la dimensión de la matriz de datos. 
dim(S) #

# Nos menciona el nombre de las variables
names (S)		

# Observaciones de la Base de Datos
head (S)    # Primeras 6 obs.
head (S, 3) # Primeas 3 observaciones 
tail (S)		# Últimas 6 obs.

# Resumen de las variables
summary(S) 	

# Estructura del Data.frame
str (S)

# De los datos tomamos por filas (Observaciones) y columnas (Variables)
S [c(1,2,3)  ,    c(1,2)]

# Consideramos todas las filaes y excluimos las dos primeras  columnas (Variables)
S [         ,    -c(1,2)] 

#####################################
###### Base de Datos de Creada ######
######        BaseCig      ##########
#####################################

# Variables (Nombres-Sexo-Cigarros)
D <- BaseCig
D
names(D) #Nombre de las variables

# Observaciones de la variable "cigarros"
D$cigarros

# Filtramos las observaciones de la "variable cigarro"
D$cigarros [D$cigarros >15 ]

# Filtramos en base a las variables "cigarros" y "sexo"
D$cigarros [D$cigarros >15 & D$sexo==0] 	

# Reseumen de una determinada Variable
summary (D$cigarros)

# Personas que fuman 0 cigarros
D$nombres [ which (D$cigarros ==0)]                        

# Nombres de personas del quartil 3 que fuman
D$  nombres [ which (D$cigarros >14.5 & D$cigarros < 16.5)]  

# Identificar las observaciones del quartil 1, para una variable
summary (D$cigarros)

# Nombre de persona que fuman 11 cigarros
D [ which (D$cigarros ==11),  c( "nombres", "cigarros")]                        


##########################################
###### Medidas de Tendencia Central ######
##########################################
# Lista de objetos
ls()

#Establecer directorio de ubicación
Base1 <- read_csv("C:/Users/SOCIAL DATA/Social Data Dropbox/Social Data Consulting/TEXT MINING/modulo1/data/Base1.csv")
S <- Base1 
S
names(S)
head(S)

# Media
mean(S$Chocolate)

# Mediana
median(S$Chocolate)

# Para la Moda
install.packages("lsr")
library("lsr")
modeOf(S$Chocolate)


###############################################
############## Gráficas con R #################
###############################################

# Gráficas sencillas en R

plot(D$cigarros)
hist(D$cigarros)  
dotchart (D$cigarros)
boxplot (cigarros, range=0)

abline (h=mean (D$cigarros) ,    col="red")
abline (h=10 )
abline (h=4, col="green" )

colours()


# Otras gráficas más avanzadas

y <- S$Chocolate
x <- S$Ansiedad

# Modelo de gráfica 1

plot(x,    y, 
     main="Gráfica 1: Cantidad de Chocolates por Nivel de 
     Ansiedad",   
     sub="",
     xlab="Nível de Ansiedad",  
     ylab="Número de Chocolocates",
     xlim=c(0, 4),         
     ylim=c(0, 20))

# Modelo de gráfica 2

D [ , ]
names(D)
D [ , c(1,2)]
plot (D$cigarros) #default
plot (D$cigarros,         ylim= c(0,20),              axes=FALSE,  ann=FALSE )
lines (D$cigarros,        col ="brown",               lty=2)
axis(1, at=c(1,6,16),     lab=c("Ana","João","Vanesa"),   cex.axis=.9)
axis(2, at=c(seq (0,20,5)),                             cex.axis=.9)
legend(1,8,
       c("Chocolates"), 
       cex=0.5, 
       col=c("brown"), 
       lty=c(1),
       bty="o")
title ("Consumo de Chocolates" , cex.main=.7)
box()

# Ejemplos de Gráficas

example (plot) # enviar cursor para consola (ctrl 2), y avanzar clickando 'enter'
example (hist)
example (barplot)
example (boxplot)

