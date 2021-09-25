#Analisis de vuelos nacionales que partieron de los tres principales aeropuertos de la ciudad de New York en 2013
#Se geeraran resumenes graficos y numericos simples de datos de estos vuelos
#se exploraran los tiempos de demora
library(statsr)
library(dplyr)
library(ggplot2)
data("nycflights")
names(nycflights)
?nycflights

#Estructura, para ver el marco de datos, dimensiones y tipos de datos
str(nycflights)

#Examinar los retrasos de salida para todos los vuelos
ggplot(data = nycflights ,aes(x = dep_delay)) + geom_histogram(binwidth = 15)
#Genera un histograma donde los tiempos de retraso caen como intervalos en eleje x, en el eje y el numero de datos que entran en ese intervalo
#binwidth significa ancho de las barras en el histograma

#Retrasos de salida que solo se dirigen a RDU
#Primero se deben de filtrar los retrasos de los vuelos que solo se dirigen a RDU (dest == RDU)
rdu_flight <- nycflights %>% filter(dest == "RDU")
ggplot(data = rdu_flight ,aes(x = dep_delay)) + geom_histogram(binwidth = 30)

#Resumen estadistico, media,desviacion estandar y total de datos(n)
rdu_flight %>% summarise(media = mean(dep_delay),ds = sd(dep_delay), n = n())
                                        