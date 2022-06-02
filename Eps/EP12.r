library(dplyr)
library(WRS2)
library(ggpubr)


# Integrantes Grupo 5:
# Wladimir Durán
# Rodrigo Hernández
# Manuel Villar

#--------------------------------------------PREGUNTA 1---------------------------------------------------------------

# 1. En el trabajo de título de un estudiante del DIINF se reportan los siguientes tiempos de ejecución (en 
# milisegundos) medidos para dos versiones de un algoritmo genético para resolver instancias del problema 
# del vendedor viajero disponibles en repositorios públicos. ¿Es uno de los algoritmos más rápido que el otro?

# Se define un alfa
alfaElJefe <- 0.05

# Se definen los datos de la tabla provista en el enunciado
instanciaA <- c(171,27,162,102,89,185,158,64,149,14)
tiempoA <- c(70599,783108,4428151,37449,834565,48705,842079,210041,402929,62764)
instanciaB <- c(190,95,183,198,16,151,11,169,187,135)
tiempoB <- c(120276,1174562,2196277,180141,35974,1252837,92932,6701654,6568968,4629726)

algoritmoA <- data.frame(instanciaA,tiempoA)
algoritmoB <- data.frame(instanciaB,tiempoB)

# Se comprueba normalidad de los tiempos de ambos algoritmos
normalidadA <- shapiro.test(tiempoA)
normalidadB <- shapiro.test(tiempoB)
print(normalidadA)
print(normalidadB)

# Al no estar distribuidos normalmente, entonces se aplica transformación logarítmica para los tiempos
logTiempoA <- log(tiempoA)
logTiempoB <- log(tiempoB)

# Se comprueba nuevamente normalidad, pero ahora con la transformación hecha
normalidadLogA <- shapiro.test(logTiempoA)
normalidadLogB <- shapiro.test(logTiempoB)
print(normalidadLogA)
print(normalidadLogB)

# Ahora que los datos siguen una distribución normal, se plantea la hipótesis:
# H0 : El tiempo medio de ejecución de los algoritmos es igual
# Ha : El tiempo medio de ejecución de los algoritmos es distinto

# Se aplica la prueba t de Student para dos muestras independientes (Se infiere esto por las instancias)
prueba <- t.test( x = logTiempoA,
                  y = logTiempoB,
                  paired = F,
                  alternative = "two.sided",
                  mu = 0,
                  conf.level = 1 - alfaElJefe)
print(prueba)

# Por tanto, como el p = 0.21 > alfa = 0.05 entonces se falla en rechazar la hipótesis nula. Se concluye
# con un 95% de confianza que el tiempo medio de ejecución de los algoritmos es igual.

#--------------------------------------------PREGUNTA 2---------------------------------------------------------------

# 2. Analice la primera pregunta abordada en el ejercicio práctico 11, con los mismos datos, utilizando un
# método robusto adecuado. 
# Pregunta: 
# 1. Propongan una pregunta de investigación original, que involucre la comparación de las medias de dos grupos 
# independientes (más abajo se dan unos ejemplos). Fijando una semilla propia, seleccionen una muestra aleatoria 
# de hogares (250 < n < 500) y respondan la pregunta propuesta utilizando una simulación Monte Carlo.

# Lectura y filtro de datos
datos <- read.csv2("C:\\Users\\Asus\\Downloads\\EP12\\EP12\\EP11 Datos.csv")

# Se define una semilla
set.seed(69420)

# Pregunta de investigación: Se quiere saber si el sueldo de los hombres con educación media completa afiliados a un sistema previsional es 
# similar o distinto a los que no lo están.
# Variables a utilizar:
# id.vivienda. Identificador de cada hogar
# sexo. Hombre o Mujer
# o28. Se encuentra afiliado a algún sistema previsional (sistema de pensiones)?
# e6a. ¿Cuál fue el nivel educacional más alto alcanzado o el nivel educacional actual?
# ytotcorh. Ingreso total del hogar corregido


# Se seleccionan solo los datos que se trabajarán.
datosSeleccionados <- datos %>% select(id.vivienda, sexo, o28, e6a, ytotcorh)

# Se filtran los datos y se cambia el tipo de variable del nivel educacional para poder obtener una media.
datosFiltrados <- datosSeleccionados %>% filter(sexo == "Hombre",e6a %in% c("Educaci�n Media Cient�fico-Humanista", 
                                                                            "Humanidades (Sistema Antiguo)", 
                                                                            "T�cnica, Comercial, Industrial o Normalista (Sistema Antiguo)", 
                                                                            "Educaci�n Media T�cnica Profesional")) 
                          
# Se hace la separación entre los hombres con sistema de previsión y los que no
hombresConPrevision <- datosFiltrados %>% filter(o28 == "S�")
hombresSinPrevision <- datosFiltrados %>% filter(o28 == "No")

# Se estipula la hipótesis a contrastar:
# H0: El sueldo de los hombres con educación media completa que estan afiliadas a un sistema previsional es similiar al de los que no están afiliadas.  (u1 - u2 = 0)
# Ha: El sueldo de los hombres con educación media completa que estan afiliadas a un sistema previsional es distinto al de los que no están afiliadas.  (u1 - u2 != 0)

# Se obtienen el tamaño de las muestras de la población (250 < n < 500)
n1 <- sample(c(250:500), size = 1, replace = F)
n2 <- sample(c(250:500), size = 1, replace = F)

# Se sacan las muestras de los datos correspondientes
muestraConPrevision <- as.numeric(sample(hombresConPrevision[["ytotcorh"]],size=n1))
muestraSinPrevision <- as.numeric(sample(hombresSinPrevision[["ytotcorh"]],size=n2))

normalidadConPrevision <- shapiro.test(muestraConPrevision)
normalidadSinPrevision <- shapiro.test(muestraSinPrevision)

print(normalidadConPrevision)
print(normalidadSinPrevision)

# Luego de revisar la normalidad con Shapiro, se observa que ninguna de las muestras sigue una distribución similar a la normal, por lo que se realiza
# la prueba de Yuen.

gamma <- 0.2

podaConPrevision <- n1 * gamma
podaSinPrevision <- n2 * gamma

previsionTruncada <- muestraConPrevision[podaConPrevision:(n1 - podaConPrevision)]
sinPrevisionTruncada <- muestraSinPrevision[podaConPrevision:(n2 - podaSinPrevision)]

sueldo <- c(previsionTruncada, sinPrevisionTruncada)
prevision <- c(rep("Prevision", length(previsionTruncada)) , rep("SinPrevision", length(sinPrevisionTruncada)))

datosTruncados <- data.frame(prevision, sueldo)

g <- ggqqplot(datosTruncados, x = "sueldo", facet.by = "prevision",
              palette = c("blue", "red") , color = "prevision" )

print(g)

prueba <- yuen(sueldo ~ prevision, data = datosTruncados, tr = gamma)
print(prueba)

# Luego de observar los resultados de la prueba de Yuen, se obtiene un valor p < 0.05, por lo que se rechaza la hipótesis nula en favor de la
# hipótesis alternativa, es decir, con un 95% de confianza, el sueldo de los hombres con educación media completa y con previsión, difiere al sueldo
# de los hombres con educación media completa pero sin previsión.

#--------------------------------------------PREGUNTA 3---------------------------------------------------------------

# 2. Propongan una pregunta de investigación original, que involucre la comparación de las medias de más de
# dos grupos independientes (más abajo se dan unos ejemplos). Fijando una semilla distinta a la anterior,
# seleccionen una muestra aleatoria de hogares (400 < n < 600) y respondan la pregunta propuesta utilizando
# bootstrapping. Solo por ejercicio académico, aplique un análisis post-hoc con bootstrapping aunque este no
# sea necesario.


# Pregunta de investigación: 
# ¿El ingreso per capita es igual entre hombres que pasaron la mayor parte del tiempo viviendo con sólo su madre, sólo su padre o   ambos padres?

# Se seleccionan solo los datos que se trabajarán.
datosSeleccionados2 <- datos %>% select(id.vivienda, sexo, r11, ytotcorh)

# Se filtran los datos de lo que ocuparemos
datosFiltrados2 <- datosSeleccionados2 %>% filter(sexo == "Hombre"  & r11 %in% c("S�lo su padre", 
                                                                                 "S�lo su madre", 
                                                                                 "Ambos padres"))

# Se estipula la hipótesis a contrastar:
#H0: Las 3 medias se distribuyen de manera similar entre si
#HA: Las 3 medias se distribuyen de manera diferente entre si

#Fijamos el nivel de significaci�n
alfaElJefe = 0.05

gamma <- 0.2

set.seed(42069)

# Se filtran los datos para hombres con sólo su padre, sólo su madre y ambos padres por separado
hombresSoloPadre <- datosFiltrados2 %>% filter(r11 == "S�lo su padre")
hombresSoloMadre <- datosFiltrados2 %>% filter(r11 == "S�lo su madre")
hombresAmbosPadres <- datosFiltrados2 %>% filter(r11 == "Ambos padres")

#hombresSoloPadreSamp <- as.numeric(sample(hombresSoloPadre,size=100))
#hombresSoloMadreSamp <- as.numeric(sample(hombresSoloMadre,size=100))
#hombresAmbosPadresSamp <- as.numeric(sample(hombresAmbosPadres,size=100))

#Sacamos los ingresos
hombresSoloPadreIngresos <- hombresSoloPadre[["ytotcorh"]]
hombresSoloMadreIngresos <- hombresSoloMadre[["ytotcorh"]]
hombresAmbosPadresIngresos <- hombresAmbosPadres[["ytotcorh"]]



ingresos <- c(hombresSoloPadreIngresos, hombresSoloMadreIngresos, hombresAmbosPadresIngresos)

crianza <- c(rep("S�lo su padre", length(hombresSoloPadreIngresos)), rep("S�lo su madre", length(hombresSoloMadreIngresos)), rep("Ambos padres", length(hombresAmbosPadresIngresos)))

datos <- data.frame(ingresos, crianza)

medidas <- t1way(ingresos ~  crianza, data = datos, tr = gamma, alpha = alfaElJefe)
print(medidas)

#Realizamos post hoc incluso cuando no es necesario
postHoc <- lincon(ingresos ~ crianza, data = datos, tr = gamma, alpha = alfaElJefe)
print(postHoc)

#Se hace bootstraping

bootstrap <- t1waybt(ingresos ~ crianza, data = datos, tr = gamma, nboot = 500)
print(bootstrap)

#Post hoc de bootstrapping
postHocBoot <- mcppb20(ingresos ~ crianza, data = datos, tr = gamma, nboot = 500)
print(postHocBoot)

#Dado que P < alfa, se rechaza la hip�tesis nula en favor de la alternativa, por lo que se puede decir con un 95% de confianza que
#las medias de los ingresos entre personas que fueron criadas por s�lo sus padres, sus madres y sus ambos p�dres son diferentes.

#La prueba de Lincon luego arroja que nuestra hip�tesis nula es cierta con un 95% de confianza para la comparaci�n entre s�lo ser criado por su
#padre y s�lo haber sido criado con su madre contra haber sido criado por ambos padres, siendo rechazada para la comparativa entre s�lo madre
#contra s�lo padre en favor de la hip�tesis alternativa, por lo que se puede afirmar con un 95% de confianza que la media de ingresos es similar
#entre personas criadas s�lo con su madre y s�lo con su padre, pero distinta a haber sido criado por ambos padres.

#Para el caso del bootstrapping, se llega a la conclusi�n de que con un 95% de confianza, existe una diferencia de medias de sueldo entre s�lo
#madre y ambos padres, siendo que para las otras comparativas se acepta la hip�tesis nula.

