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

#Filtrar de acuerdo a multiples criterios, ejemplo: vuelos en a San Francisco en Febrero
sfo_feb_flight <- nycflights %>% filter(dest == "SFO" , month == 2)
#Nota tambien puede usarse | en lugar de coma 

#Histograma retraso de arrivo(llegada) y resumen estadistico
ggplot(data = sfo_feb_flight, aes(x = arr_delay)) + geom_histogram(binwidth = 15)
sfo_feb_flight %>% summarise(media =mean(arr_delay), ds = sd(arr_delay), n = n())

#Para calcular sumarios estadisticos para varios grupos de un marco de datos, de una categoria (variable), se toman los mismos elemnetos 
#y se obtienen sus estadisticas para cada grupo
sfo_feb_flight %>% group_by(origin) %>% summarise(media = mean(arr_delay), ds = sd(arr_delay), n = n())
sfo_feb_flight %>% group_by(carrier) %>% summarise(media = mean(arr_delay), ds = sd(arr_delay), n = n(), IQR = IQR(arr_delay))

#Se ordenan los datos por categorias (meses) y despues se obtienen el sumario estadistico (media), se ordena la media en orden desendente
nycflights %>% group_by(month) %>% summarise(media = mean(dep_delay)) %>% arrange(desc(media))
#Para ordenar los datos arrange(desc(media))
nycflights %>% group_by(carrier) %>% summarise(media = mean(dep_delay)) %>% arrange(desc(media))

feb_fligth <- nycflights %>% filter(month == 7)
ggplot(data = feb_fligth, aes(x = dep_delay)) + geom_histogram()

#Obtencion de graficos de caja y bigote para el retraso de salida por meses
ggplot(nycflights, aes(x = factor(month), y = dep_delay)) + geom_boxplot()

#porcentaje de vuelos por putualidad
nycflights <- nycflights %>% mutate(dep_type = ifelse(dep_delay < 5, "on time", "delayed"))
nycflights %>% group_by(origin) %>% summarise(a_tiempo = sum(dep_type == "on time") / n()) %>% arrange(desc(a_tiempo))

#Grafico de barras segmentada 
ggplot(data = nycflights, aes(x = origin, fill = dep_type)) + geom_bar()

#Velocidad promedio por cola de vuelo, marca de vuelo?
#nycflights <- nycflights %>% mutate(avg_speed = distance / (air_time/60)) %>% select(avg_speed, tailnum)
#Nota la funcion select() solo guardara los vectores de datos seleccionados en la tabla
nycflights <- nycflights %>% mutate(avg_speed = distance / (air_time/60))
speed <- nycflights %>% mutate(avg_speed = distance / (air_time/60)) %>% select(avg_speed, tailnum)

#Este analisis va por cuenta propia, en promedio que cola de vuelo es el mas rapido?
flight_speed <- nycflights %>% group_by(tailnum) %>% summarise(speed_mean = mean(avg_speed), n = n()) %>% arrange(desc(speed_mean))

ggplot(data = nycflights, aes(x = avg_speed, y = distance)) + geom_point()

nycflights <- nycflights %>% mutate(arr_type = ifelse(arr_delay <= 0, "on time", "delayed"))
nycflights <- nycflights %>% mutate(dep_type = ifelse(dep_delay < 5, "on time", "delayed"))


