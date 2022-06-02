# Se importan los paquetes necesarios.
if (! require ( dplyr ) ) {
  install.packages ("dplyr", dependencies = TRUE )
  require ( dplyr )
  
}
if (! require ( ggpubr ) ) {
  install.packages ("ggpubr", dependencies = TRUE )
  require ( ggpubr )
  
}
if (! require ( tidyr ) ) {
  install.packages ("tidyr", dependencies = TRUE )
  require ( tidyr )
  
}
if (! require ( ggplot2 ) ) {
  install.packages ("ggplot2", dependencies = TRUE )
  require ( ggplot2 )
  
}

# Pregunta 1:

# Enunciado:

# El artículo “Automatic Segmentation of Medical Images Using Image Registration: 
# Diagnostic and Simulation Applications” (Journal of Medical Engeeniering and Technology
# 2005) propuso una nueva técnica para la identificación automática de los bordes de 
# estructuras significativas en una imagen médica utilizando desplazamiento lineal promedio
# (ALD, por sus siglas en inglés). El artículo dio las siguientes observaciones de ALD
# con una muestra de 49 riñones (en pixeles y usando punto en vez de coma decimal):

# Los autores comentaron que el ALD medio sería de al menos 1.0 pixel. 
# ¿Los datos soportan esta afirmación?

# RESPUESTA:

# Para este caso, se han definido las siguientes hipótesis:

# Hipótesis Nula (H0)

# H0: El ALD medio (µ) de la nueva técnica para la identificación de imágenes médicas es de
#     1.0 pixel, es decir: µ = 1.0 
#     

# Hipótesis alternativa (HA)

# HA: El ALD medio (µ) de la nueva técnica para la identificación de imágenes médicas es
#     mayor a 1.0 pixel, es decir: µ > 1.0


# Matemáticamente las hipótesis formuladas quedan como:
# H0: µ = 1.0
# HA: µ > 1.0


# Utilizando un nivel de significación de α = 0.05 (es decir, un nivel de confianza
# de 95%), se procede a verificar si las observaciones provienen de una distribución
# cercana a la normal.

# A través de la prueba de Shapiro-test se puede ver que el valor p obtenido (p = 0.6202)
# es mucho mayor que nuestro nivel de significación, lo cual se puede comprobar con el
# gráfico Q-Q, donde a pesar de observar algunos valores atípicos la mayoría de los valores
# se encuentra dentro del rango aceptable para suponer normalidad en el conjunto de datos.

# Se procederá a trabajar con la prueba T de Student para una muestra, ya que la cantidad
# de observaciones es menor a 30 y se desconoce la desviación estándar poblacional. Como 
# los datos fueron escogidos al azar, podemos suponer que las observaciones son independientes, además al realizar
# la prueba de Shapiro-test como se mencionó anteriormente, ésta sigue una distribución 
# cercana a la normal.


# Procedimiento:


# Obtención de los datos.
texto <- "1.38 1.28 1.09 1.07 0.96 1.28 0.91 1.49 1.11 0.66 1.14 1.13 0.91 0.94 1.30
0.87 0.73 0.92 1.00 1.05 1.12 1.10 0.95 1.29 0.86 0.96 0.94 1.45 1.12 1.06
0.71 0.88 0.96 1.14 1.03 0.89 0.81 1.04 1.15 0.75 1.12 1.01 1.11 0.64 1.25
0.68 1.44 1.28 1.21"

file <- textConnection(texto)
datos <- scan(file)
muestra <- data.frame(datos)

# Prueba de normalidad para los datos.
normalidad <- shapiro.test(datos)
print(normalidad)


# Gráfico Q-Q para identificar si la muestra sigue una distribución cercana a la normal.
g <- ggqqplot(muestra, x = "datos", 
              color = "red",
              xlab  = "Teórico",
              ylab  = "Muestra",
              title = "P1: Gráfico Q-Q muestra v/s distr. normal")
print(g)


# Se establecen los datos conocidos.
n <- length(datos)
grados_libertad <- n - 1
valor_nulo <- 1.0
alfa <- 0.05


# Se calcula el estadístico T, P-value, intervalo de confianza y la media.
prueba <- t.test(datos, 
                 alternative = "greater",
                 mu = valor_nulo, 
                 conf.level = 1 - alfa)
print(prueba)

# Datos obtenidos a través de estadístico T y la prueba de Shapiro:
# t = 1.4887
# p-value = 0.07155
# Intervalo de confianza = [0.9943915, Inf[
# media = 1.044286 
# p-value(Shapiro-test) = 0.6202

# Conclusión:

# Como p = 0.07155 es un relativamente superior a nuestro α = 0.05, y la media de las 
# observaciones se encuentra dentro del intervalo de confianza, se falla al rechazar
# la hipótesis nula, afirmando con un 95% de confianza; que el ALD medio es de 1.0 pixel. 


# Pregunta número 2:

# Enunciado:

# Se sabe que la lactancia estimula una pérdida de masa, ósea para proporcionar
# cantidades de calcio adecuadas para la producción de leche. Un estudio intentó
# determinar si madres adolescentes podían recuperar niveles más normales a pesar
# de no consumir suplementos (Amer. J. Clinical Nutr., 2004; 1322-1326). El estudio
# obtuvo las siguientes medidas del contenido total de minerales en los huesos del
# cuerpo (en gramos) para una muestra de madres adolescentes tanto durante la lactancia
# (6-24 semanas postparto) y posterior a ella (12-30 semana postparto): 

# ¿Sugieren los datos que el contenido total de minerales en los huesos del cuerpo 
# durante el posdestete excede el de la etapa de lactancia por más de 60 g?


# RESPUESTA:

# Para este caso, se han definido las siguientes hipótesis:

# Hipótesis Nula (H0)

# H0: La diferencia del contenido total de minerales en los huesos entre la media de
#     la etapa del posdestete(µ1) y la media de la etapa de lactancia(µ2) es igual 
#     a 60g.

# Hipótesis alternativa (HA)

# HA: La diferencia del contenido total de minerales en los huesos entre la media de
#     la etapa del posdestete(µ1) y la media de la etapa de lactancia(µ2) es mayor 
#     a 60g.


# Matemáticamente las hipótesis formuladas quedan como:
# H0: µ1-µ2 = 60
# HA: µ1-µ2 > 60

# Utilizando un nivel de significación de α = 0.05 (es decir, un nivel de confianza 
# de 95%), se procede a verificar si la diferencia de las medias proviene de una 
# distribución normal.

# Con la prueba de Shapiro se obtuvo un valor de p (p = 0.139) mucho mayor que nuestro
# nivel de significación, lo cual se puede comprobar con el gráfico Q-Q, donde no se 
# identifican valores atípicos, por lo que es posible suponer que la muestra proviene de
# una distribución normal.

# Adicionalmente, se determina que las muestras son pareadas, pues cada valor de de las
# observaciones correspondientes al posdestete se encuentra relacionado con solo una de 
# las observaciones de la lactancia.

# Se procederá a trabajar con la prueba T de Student, ya que los datos fueron escogidos
# al azar, podemos suponer que las observaciones son independientes, además al realizar
# la prueba de Shapiro-test como se mencionó anteriormente, ésta sigue una distribución 
# normal.


# Procedimiento:

# Obtención de los datos.
texto <- "1928 2549 2825 1924 1628 2175 2114 2621 1843 2541"
file <- textConnection(texto)
lactancia <- scan(file)

texto <- "2126 2885 2895 1942 1750 2184 2164 2626 2006 2627"
file <- textConnection(texto)
posdestete <- scan(file)

# Cálculo de la diferencia entre las muestras del posdestete y la lactancia. 
diferencia <- posdestete - lactancia

# Prueba de normalidad para la diferencia.
normalidad <- shapiro.test(diferencia)
print(normalidad)

# Gráfico Q-Q para identificar si la muestra sigue una distribución normal.
g <- ggqqplot(diferencia, 
              color = "SteelBlue",
              xlab  = "Teórico",
              ylab  = "Muestra",
              title = "P2: Gráfico Q-Q muestra v/s distr. normal")
print(g)

# Se establecen los datos conocidos.
valor_nulo <- 60
alfa <- 0.05

# Se calcula estadístico T, P-value, intervalo de confianza y la media.
prueba <- t.test(x = posdestete, 
                 y = lactancia, 
                 paired = TRUE,
                 alternative = "greater",
                 mu = valor_nulo,
                 conf.level = 1 - alfa)
print(prueba)


# Datos obtenidos a través de estadístico T y la prueba de Shapiro:
# t = 1.3917
# p-value = 0.09873
# Intervalo de confianza = [45.50299 , Inf[
# media = 105.7
# p-value(Shapiro-test) = 0.1389

# Conclusión:

# Por último, como p = 0.098 es un valor superior a nuestro α = 0.05, y  la media
# de las diferencias se encuentra dentro del intervalo de confianza, se falla al rechazar
# la hipótesis nula afirmando con un 95% de confianza que el contenido total de 
# minerales en los huesos del cuerpo durante el posdestete pareciera no exceder por más
# de 60g, en comparación con la etapa de lactancia, aunque sería necesario aumentar
# el tamaño de la muestra para verificar nuestra hipótesis, 
# con el fin de obtener un mayor grado de certeza.



# Pregunta 3.

# Enunciado:

# La avicultura de carne es un negocio muy lucrativo, y cualquier método que ayude al
# rápido crecimiento de los pollitos es beneficioso, tanto para las avícolas como para
# los consumidores. En el paquete datasets de R están los datos (chickwts) de un experimento 
# hecho para medir la efectividad de varios suplementos alimenticios en la tasa de crecimiento
# de las aves. Pollitos recién nacidos se separaron aleatoriamente en 6 grupos, y a cada grupo
# se le dio un suplemento distinto. Para productores de la 7ma región, es especialmente 
# importante saber si existe diferencia en la efectividad entre el suplemento basado en 
# linaza (linseed) y el basado en habas (horsebean).

# RESPUESTA:

# Para este caso, se han definido las siguientes hipótesis:

# Hipótesis Nula (H0)
# H0: No hay diferencia en la efectividad promedio entre el suplemento basado en linaza 
#     (linseed) y el basado en habas (horsebean).


# Hipótesis Alternativa (HA)
# HA: Existe una diferencia en la efectividad promedio entre el suplemento basado en linaza
#     (linseed) y el basado en habas (horsebean).

# Matemáticamente las hipótesis formuladas quedan como:
# µ1: media linaza
# µ2: media habas
# H0: µ1-µ2 = 0
# HA: µ1-µ2 /= 0

# Utilizando un nivel de significación de α = 0.05 (es decir, un nivel de confianza 
# de 95%), se procede a verificar si cada muestra proviene de una distribución
# cercana a la normal.

# Con la prueba de Shapiro realizada para ambas muestras, se obtuvieron valores mayores que
# nuestro nivel de significación, lo cual se puede comprobar en cada gráfico Q-Q, donde
# los valores observados tienden a seguir una distribución normal.

# Se procederá a trabajar con la prueba T de Student para muestras independientes, pues
# se observa que ambas muestras son de este tipo, debido a que los pollitos recién 
# nacidos se separaron aleatoriamente en 6 grupos, y a cada grupo se le dio un suplemento 
# distinto. Además al realizar la prueba de Shapiro-test para ambas muestras como se mencionó
# anteriormente, éstas siguen una distribución cercana a la normal.


# Procedimiento:

# Obtención de datos y filtros por suplementos:
datos <- chickwts
linaza <- datos  %>% filter(feed == "linseed")
habas <- datos  %>% filter(feed == "horsebean")
pesosLinaza <- linaza[["weight"]]
pesosHabas <- habas[["weight"]]

# Prueba de normalidad para las muestras independientes.
normalidad_A <- shapiro.test(pesosLinaza)
print(normalidad_A)
normalidad_B <- shapiro.test(pesosHabas)
print(normalidad_B)

# Gráfico Q-Q para identificar si la muestra de linaza sigue una distribución normal.
g1 <- ggqqplot(pesosLinaza, 
               color = "SteelBlue",
               xlab  = "Teórico",
               ylab  = "Muestra",
               title = "P3: Gráfico Q-Q muestra linaza v/s distr. normal")
print(g1)

# Gráfico Q-Q para identificar si la muestra de habas sigue una distribución normal.
g2 <- ggqqplot(pesosHabas, 
               color = "red",
               xlab  = "Teórico",
               ylab  = "Muestra",
               title = "P3: Gráfico Q-Q muestra habas v/s distr. normal")
print(g2)

# Se establecen los datos conocidos.
valor_nulo <- 0
alfa <- 0.05


# Se calcula estadístico T, P-value, intervalo de confianza y la media.
prueba <- t.test(x = pesosLinaza, 
                 y = pesosHabas, 
                 paired = FALSE,
                 alternative = "two.sided",
                 mu = valor_nulo,
                 conf.level = 1 - alfa)
print(prueba)


# Se calcula la diferencia entre las medias.
media_A <- mean(pesosLinaza)
media_B <- mean(pesosHabas)
diferencia <- media_A - media_B
cat(" Diferencia de las medias =", diferencia , "[g]\n")

# Datos obtenidos a través de estadístico T y la prueba de Shapiro:
# t = 3.0172
# p-value = 0.006869
# Intervalo de confianza = [18.0403, 99.0597]
# media_A = 218.75
# media_B = 160.20 
# diferencia = 58.55
# p-value(Shapiro-test / pesosLinaza) = 0.9035
# p-value(Shapiro-test / pesosHabas) = 0.5264

# Conclusión:

# Como el valor p = 0.006869 es significativamente menor a nuestro α = 0.05, y la media de las 
# diferencias se encuentra dentro del intervalo de confianza, se puede concluir que la 
# evidencia a favor de HA es contundente, en consecuencia se rechazará H0 en favor de HA, 
# afirmando con un 95% de confianza que existe una diferencia en la efectividad de ambos
# suplementos.


