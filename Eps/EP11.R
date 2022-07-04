library(dplyr)
library(ggpubr)
library(ggplot2)
library(boot)
library(simpleboot)
library(readxl)

#Wladimir Dur?n
#Rodrigo Hern?ndez
#Manuel Villar

options(scipen=999)

# 1. Propongan una pregunta de investigación original, que involucre la comparación de las medias de dos grupos
# independientes (más abajo se dan unos ejemplos). Fijando una semilla propia, seleccionen una muestra
# aleatoria de hogares (250 < n < 500) y respondan la pregunta propuesta utilizando una simulación Monte
# Carlo.

# Se fija la semilla a utilizar
set.seed(69420)

# Se establece el nivel de confianza a utilizar
alfa <- 0.05

# Pregunta de investigación: ¿El ingreso per cápita de las mujeres que viven solas y poseen estudios superiores es similar a las mujeres
# que viven solas y poseen solo estudios de enseñanza media?

# Lectura de datos
datos <- read.csv2("C:\\Users\\villa\\OneDrive\\Documentos\\inferencia\\EP11 Datos.csv")

# Se seleccionan solo los datos que se trabajarán.
datosSeleccionados <- datos %>% select(id.vivienda, sexo, numper, e6a, ytotcorh)

# Se filtran los datos que se necesitan, es decir, mujeres que vivan solas y tengan educación media o superior completa.
datosFiltrados <- datosSeleccionados %>% filter(numper == 1 & sexo == "Mujer" & e6a %in% c("Educaci?n Media Cient?fico-Humanista", 
"Humanidades (Sistema Antiguo)", "T?cnica, Comercial, Industrial o Normalista (Sistema Antiguo)", "Educaci?n Media T?cnica Profesional", 
"T?cnico Nivel Superior Incompleto (Carreras 1 a 3 a?os)", "T?cnico Nivel Superior Completo (Carreras 1 a 3 a?os)", 
"Profesional Incompleto (Carreras 4 o m?s a?os)", "Profesional Completo (Carreras 4 o m?s a?os)", "Postgrado Incompleto", "Postgrado Completo"))

# Se estipula la hipótesis a contrastar:
# H0: Las medias del ingreso de las mujeres con educación media y que viven solas es similar a la de las mujeres con educación superior y que viven solas.   (u1 - u2 = 0)
# Ha: Las medias del ingreso de las mujeres con educación media y que viven solas es distinta a la de las mujeres con educación superior y que viven solas.  (u1 - u2 != 0)

# Se filtra la población de mujeres con educación media
mujeresEdMedia <- datosFiltrados %>% filter(e6a %in% c("Educaci?n Media Cient?fico-Humanista", "Humanidades (SistemaAntiguo)", "T?cnica, Comercial, Industrial o Normalista (Sistema Antiguo)",
    "Educaci?n Media T?cnica Profesional", "T?cnico Nivel Superior Incompleto (Carreras 1 a 3 a?os)", "Profesional Incompleto (Carreras 4 o m?s a?os)"))

# Se filtra la población de mujeres con educación superior
mujeresEdSuperior <- datosFiltrados %>% filter(e6a %in% c("Profesional Completo (Carreras 4 o m?s a?os)", "Postgrado Incompleto", "Postgrado Completo"))


# Crear una gran cantidad P de permutaciones (generalmente terminada en 9 para simplificar los cómputos) 
# a partir de las muestras originales, usando muestreo sin reposición sobre la muestra combinada, 
# y obtener el estad?stico  para cada una de las muestras.

# Se define la cantidad de permutaciones que se realizarán.
P <- 3458

# Se saca el tamaño las muestras de la población
n1 <- sample(c(250:500), size = 1, replace = F)
n2 <- sample(c(250:500), size = 1, replace = F)

# Se sacan las muestras de los datos correspondientes
muestraEdMedia <- as.numeric(sample(mujeresEdMedia[["ytotcorh"]],size=n1))
muestraEdSuperior <- as.numeric(sample(mujeresEdSuperior[["ytotcorh"]],size=n2))


# A continuación se definen las funciones a utilizar para contrastar la hipótesis.

# FUNCIÓN PARA CALCULAR LA DIFERENCIA DE MEDIAS (solo para dos muestras)
# Entrada: -muestras: corresponden a una lista con las muestras utilizadas para analizar.
#         -FUN: corresponde a la función que calcula el estadístico deseado.
# Salida: el valor de la diferencia de las medias de las muestras.
calcularDiferencias <- function(muestras, FUN){
  muestra1 <- muestras[[1]]
  muestra2 <- muestras[[2]]
  diferencia <- FUN(muestra1) - FUN(muestra2)
  return(diferencia)
}

# FUNCIÓN PARA CREAR PERMUTACIONES
# Entrada: -i: corresponde al iterador para llamadas posteriores.
#         -muestra1, muestra2: corresponden a las muestras de la población.
# Salida: una lista con las muestras luego de la permutación.
obtenerPermutacion <- function(i, muestra1, muestra2){
  n1 <- length(muestra1)
  combinada <- c(muestra1, muestra2)
  n <- length(combinada)
  permutacion <- sample(combinada, n, replace = F)
  nueva1 <- permutacion[1:n1]
  nueva2 <- permutacion[(n1+1):n]
  return(list(nueva1,nueva2))
}

# FUNCIÓN PARA GRAFICAR
# Entrada:-distribucion: distribución nula de las permutaciones.
#         -colorRojo: color del gráfico Q-Q.
# Salida: gráficos de histograma y Q-Q de la distribución dada.
graficarDistribucion <- function(distribucion, colorRojo){
  observaciones <- data.frame(distribucion)
  
  h1 <- gghistogram(observaciones, x = "distribucion",
                    xlab = "Estad?stico de inter?s",
                    ylab = "Frecuencia", bins = 30)
  
  g1 <- ggqqplot(observaciones, x = "distribucion", color = colorRojo)
  
  figura <- ggarrange ( h1 , g1 , ncol = 2 , nrow = 1)
  print(figura)

}

# FUNCIÓN PARA CALCULAR EL VALOR P
# Entrada: -distribucion: corresponde a la distribución de las permutaciones.
#         -valorObservado: corresponde al valor de la diferencia de las medias de las muestras originales.
#         -repeticiones: corresponde a la cantidad de repeticiones de las permutaciones generadas.
#         -alternative: corresponde al tipo de hipótesis a contrastar.
# Salida: el valorP calculado con los parámetros de entrada.
calcularValorP <- function(distribucion, valorObservado, repeticiones, alternative){
    if(alternative == "two.sided"){
      numerador <- sum(abs(distribucion) > abs(valorObservado)) + 1
      denominador <- repeticiones + 1
      valorP <- numerador/denominador
    }
  
    else if(alternative == "greater"){
      numerador <- sum(distribucion > valorObservado) + 1
      denominador <- repeticiones + 1
      valorP <- numerador/denominador
    }
  
    else{
      numerador <- sum(distribucion <  valorObservado) + 1
      denominador <- repeticiones + 1
      valorP <- numerador/denominador
    }
  
  return(valorP)
}
  
# FUNCIÓN QUE CONTRASTA LA HIPÓTESIS
# Entrada: -muestra1,muestra2: las muestras a comparar (numéricas, en este caso, el ingreso)
#         -repeticiones: cantidad de permutaciones que se realizarán.
#         -FUN: función del estadístico que se utilizará. (en este caso, mean)
#         -alternative: Tipo de hipótesis que se va a contrastar.
# Salida: Indica los valores originales y luego de las permutaciones, incluyendo el valorP para concluir en base a ello.
contrastarHipotesisPermutaciones <- function(muestra1, muestra2, repeticiones, FUN, alternative){
  cat ( "Prueba de permutaciones \n\n " )
  cat ( "Hip?tesis alternativa : " , alternative , "\n" )
  observado <- calcularDiferencias(list(muestra1,muestra2), FUN)
  cat ( "Valor observado : " , observado , "\n" )
  
  # Se generan las permutaciones
  permutaciones <- lapply(1:repeticiones, obtenerPermutacion, muestraEdMedia, muestraEdSuperior)
  
  # Se genera la distribución de las permutaciones si la hipótesis nula fuera cierta.
  distribucion <- sapply(permutaciones, calcularDiferencias, mean)
  
  # Se grafica la distribución obtenida anteriormente
  graficarDistribucion(distribucion, "red")
  
  # Calcular el valorP
  valorP <- calcularValorP(distribucion, observado, repeticiones, alternative)
  
  cat("Valor p: ", valorP, "\n\n")
}

# Se hace la prueba de permutaciones con las medias de las muestras
contrastarHipotesisPermutaciones(muestraEdMedia, muestraEdSuperior, repeticiones = P, FUN = mean, alternative = "two.sided")

# En base a lo observado en los gráficos, luego de realizar la prueba de permutaciones, se distribuye normalmente, pero como indica por pantalla, el valor
# observado en un inicio para las muestras, indica que la diferencia de las medias es distinta a 0, y luego con el cálculo del valorP mediante la función
# definida, nos da 0.000289100896212778 con 3458 permutaciones realizadas, por lo tanto, podemos concluir con un 95% de confianza que se rechaza la hipótesis
# nula en favor de la hipótesis alternativa, es decir, las mujeres que viven solas con educación superior tienen distintos ingresos a aquellas que viven solas
# sin poseer educación superior.




# # 2. Propongan una pregunta de investigación original, que involucre la comparación de las medias de más de
# # dos grupos independientes (más abajo se dan unos ejemplos). Fijando una semilla distinta a la anterior,
# # seleccionen una muestra aleatoria de hogares (400 < n < 600) y respondan la pregunta propuesta utilizando
# # bootstrapping. Solo por ejercicio académico, aplique un análisis post-hoc con bootstrapping aunque este no
# # sea necesario.
# 
# # Se fija una nueva semilla a utilizar
# set.seed(42069)
# 
# # Se establece el nivel de confianza a utilizar
# alfa <- 0.05
# 
# # Pregunta de investigación: ¿El ingreso per cápita de las mujeres que viven solas y poseen estudios superiores es similar a las mujeres
# # que viven solas y poseen solo estudios de enseñanza media?
# 
# # Pregunta de investigación: 
# # ¿El ingreso per capita es igual entre hombres que pasaron la mayor parte del tiempo viviendo con sólo su madre, sólo su padre ambos padres?
# 
# 
# # Se seleccionan solo los datos que se trabajarán.
# datosSeleccionados2 <- datos %>% select(id.vivienda, sexo, r11, ytotcorh)
# 
# # Se filtran los datos de lo que ocuparemos
# datosFiltrados2 <- datosSeleccionados2 %>% filter(sexo == "Hombre"  & r11 %in% c("S?lo su padre", 
#                                                                                  "S?lo su madre", 
#                                                                                  "Ambos padres"))
# 
# # Se estipula la hipótesis a contrastar:
# #H0: Las 3 medias se distribuyen de manera similar entre si
# #HA: Las 3 medias se distribuyen de manera diferente entre si
# 
# # Se filtran los datos para hombres con sólo su padre, sólo su madre y ambos padres por separado
# hombresSoloPadre <- datosFiltrados2 %>% filter(r11 == "S?lo su padre")
# hombresSoloMadre <- datosFiltrados2 %>% filter(r11 == "S?lo su madre")
# hombresAmbosPadres <- datosFiltrados2 %>% filter(r11 == "Ambos padres")
# 
# # Se saca el tamaño las muestras de la población
# n1 <- sample(c(400:600), size = 1, replace = F)
# n2 <- sample(c(400:600), size = 1, replace = F)
# n3 <- sample(c(400:600), size = 1, replace = F)
# 
# 
# 
# 
# # Se sacan las muestras de los datos correspondientes
# muestraSoloPadre <- as.numeric(sample(hombresSoloPadre[["ytotcorh"]],size=n1))
# muestraSoloMadre <- as.numeric(sample(hombresSoloMadre[["ytotcorh"]],size=n2))
# muestraAmbosPadres <- as.numeric(sample(hombresAmbosPadres[["ytotcorh"]],size=n3))
# 
# #Realizamos un Shapiro test
# 
# print(shapiro.test(muestraSoloPadre))
# print(shapiro.test(muestraSoloMadre))
# print(shapiro.test(muestraAmbosPadres))
# 
# # Se calculan las medias de las muestras
# mediaSoloPadre <- mean(muestraSoloPadre)
# mediaSoloMadre <- mean(muestraSoloMadre)
# mediaAmbosPadres <- mean(muestraAmbosPadres)
# 
# # Se calcula la diferencia entre las tres medias, como la de ambos padres es mayor, entonces:
# diffAmbosSoloP <- mediaAmbosPadres - mediaSoloPadre
# diffAmbosSoloM <- mediaAmbosPadres - mediaSoloMadre
# diffSoloMSoloP <- mediaSoloMadre - mediaSoloPadre
# 
# # Se realiza Bootstrapping
# B <- 1500
# 
# # Bootstrapping entre la diferencia de medias de hombres con ambos padres y sólo padre
# btsAmbosSoloP <- two.boot(muestraAmbosPadres,muestraSoloPadre, FUN = mean, R = B)
# valoresAmbosSoloP <- data.frame(btsAmbosSoloP$t)
# colnames(valoresAmbosSoloP) <- "valoresAmbosSoloP"
#  
# # Bootstrapping entre la diferencia de medias de hombres con ambos padres y sólo madre
# btsAmbosSoloM <- two.boot(muestraAmbosPadres,muestraSoloMadre, FUN = mean, R = B)
# valoresAmbosSoloM <- data.frame(btsAmbosSoloM$t)
# colnames(valoresAmbosSoloM) <- "valoresAmbosSoloM"
# 
# # Bootstrapping entre la diferencia de medias de hombres con sólo madre y sólo padre
# btsSoloMSoloP <- two.boot(muestraSoloMadre,muestraSoloPadre, FUN = mean, R = B)
# valoresSoloMSoloP <- data.frame(btsSoloMSoloP$t)
# colnames(valoresSoloMSoloP) <- "valoresSoloMSoloP"
# 
# # Se realizan los histogramas para cada uno
# histogramaAmbosSoloP <- gghistogram(valoresAmbosSoloP, x = "valoresAmbosSoloP", color = "blue",
#                            fill= "blue", bins = 100)
# 
# histogramaAmbosSoloM <- gghistogram(valoresAmbosSoloM, x = "valoresAmbosSoloM", color = "blue",
#                            fill= "blue", bins = 100)
# 
# histogramaSoloMSoloP <- gghistogram(valoresSoloMSoloP, x = "valoresSoloMSoloP", color = "blue",
#                            fill= "blue", bins = 100)
# print(histogramaAmbosSoloP)
# print(histogramaAmbosSoloM)
# print(histogramaSoloMSoloP)
# 
# # INTERVALOS DE CONFIANZA
# intervaloAmbosSoloP <- boot.ci(btsAmbosSoloP, conf = 1-alfa, type = "norm")
# intervaloAmbosSoloM <- boot.ci(btsAmbosSoloM, conf = 1-alfa, type = "norm")
# intervaloSoloMSoloP <- boot.ci(btsSoloMSoloP, conf = 1-alfa, type = "norm")
# 
# print(intervaloAmbosSoloP)
# print(intervaloAmbosSoloM)
# print(intervaloSoloMSoloP)
# 
# 
# #Seg?n el test de normalidad, dado que todos los valores p fueron menores a 0.05, se puede afirmar con un 95% de confianza que
# #las variables no se distribuyen de manera normal, por lo que se rechaza H0.
# 
# print(summary(btsAmbosSoloM))
# print(summary(btsAmbosSoloP))
# print(summary(btsSoloMSoloP))