library (dplyr)
library (ggpubr)
library(tidyverse)
library(ez)

# Grupo 5 integrantes:
# Wladimir Durán
# Rodrigo Hernández
# Manuel Villar


# Enunciado grupo 5:
# En este momento, los investigadores buscan determinar si existen diferencias en el tiempo que tardan los usuarios 
# en formular consultas para problemas con diferente nivel de dificultad en el área de Economía.

# Se leen los datos del excel y se filtran por "Economía", y se asignan a "datosFiltrados"
datos <- read.csv2("C:\\Users\\villa\\OneDrive\\Escritorio\\EP08 Datos.csv")

datosFiltrados <- filter(datos, area %in% c("Economía"))

# Cambiamos el formato de la variable id y dificultad a categórica
datosFiltrados[["dificultad"]] <- factor(datosFiltrados[["dificultad"]])
datosFiltrados[["id"]] <- factor(datosFiltrados[["id"]])


# Se asigna un nivel de significación alfa = 0.05

# Hipótesis:
# H0: El tiempo que tardan los usuarios en formular consultas es igual para las tres dificultades de los problemas. 
# Ha: El tiempo que tardan los usuarios en formular consultas es distinto en al menos una de las dificultades de los problemas.

# H0: Ta = Tb = Tm    (dificultad Alta, Baja y Media)
# Ha: Ta != Tb || Ta != Tc || Tb != Tc

# 1. La escala con que se mide la variable dependiente tiene las propiedades de una escala de intervalos iguales.
# Como la variable estudiada es el tiempo de formulación de una consulta, entonces cumple con la escala de intervalos iguales.

# 2- Como los tiempos para formular una consulta de un problema son realizados por personas, se sabe que son independientes entre sí
#    ya que no todas las personas piensan de igual manera.

# 3- Comprobar normalidad a través de un gráfico Q-Q

g <- ggqqplot (datosFiltrados ,
               x = "tiempo",
               y = "dificultad",
               color = "blue")

g <- g + facet_wrap (~ dificultad)
g <- g + rremove ("x.ticks") + rremove ("x.text")
g <- g + rremove ("y.ticks") + rremove ("y.text")
g <- g + rremove ("axis.title")
print(g)

# 4. La matriz de varianzas-covarianzas es esférica. Esta condición establece que las varianzas entre los diferentes niveles
# de las medidas repetidas deben ser iguales. Para esto, se ejecutará la prueba ezNOVA que incluye una prueba de esfericidad
# de Mauchly.

ezAnova <- ezANOVA(data = datosFiltrados, dv = tiempo, within = dificultad,
                  wid = id, return_aov = TRUE)

print(ezAnova)

# Se compureba por tanto que cumple la prueba de esfericidad de Maunchly ya que p = 0.1111538 > alfa

#Dado que p es igual a 0.26752 y este valor es superior a nuestro alfa 0.05. Por lo tanto, como p > alfa, fallamos en rechazar 
#la hipótesis nula, por lo que podemos decir, con un 95% de confianza que no hay evidencia suficiente para rechazar la hipótesis nula.
#Esto indica que el tiempo que tardan los usuarios en formular consultas es igual para las tres dificultades de los problemas.

#Análisis post-hoc:
#Dado que se concluye con un 95% de confianza que no hay evidencia para rechazar la hipótesis nula, no es necesario realizar un 
#análisis post-hoc dado que este estudio se realiza para saber qu? dificultad de las preguntas es distinta en el tiempo tardado 
#y al en este caso no existir un tiempo distinto para formular una consulta, no tiene sentido su realizaci?n.


