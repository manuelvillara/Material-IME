library(dplyr)
library(ggpubr)
library(pwr)
library(ggplot2)

# Se sabe que una máquina que envasa detergentes industriales llena bidones con un volumen de producto que 
# sigue una distribución normal con desviación estándar de 1 litro. Usando una muestra aleatoria de 100 botellas, 
# el ingeniero a cargo de la planta requiere determinar si la máquina está llenando los bidones con una media de 
# 10 litros.

# 1. Identificar qué se pide determinar en cada pregunta, justificando explícitamente con comentarios en el script.
# 2. Escribir código R para crear gráficos y/o determinar el factor deseado o la probabilidad solicitada.
# 3. Redactar respuestas a las preguntas planteadas (comentarios) en base a los resultados del análisis realizado

# Grupo 5:
#   1. Si el ingeniero está seguro de que el verdadero volumen medio no puede ser inferior a 10 litros y piensa 
# rechazar la hipótesis nula cuando la muestra presente una media mayor a 10,5 litros, ¿cuál es la probabilidad 
# de que cometa un error de tipo I?
#   2. Si el verdadero volumen medio de los bidones fuera de 10,3 litros, ¿cuál sería la probabilidad de que el 
# ingeniero, que obviamente no conoce este dato, cometa un error de tipo II?
#   3. Como no se conoce el verdadero volumen medio, genere un gráfico del poder estadístico con las condiciones 
# anteriores, pero suponiendo que el verdadero volumen medio podría variar de 10 a 10,7 litros.
# 4. Considerando un volumen medio de 10 litros, ¿cuántos bidones deberían revisarse para conseguir un poder 
# estadístico de 0,8 y un nivel de significación de 0,05?
#   5. ¿Y si el ingeniero fuese muy exigente y quisiera reducir la probabilidad de cometer un error de tipo I a un 1% 
# solamente?


#PREGUNTA 1:

desvEst <- 1
n <- 100
mediaNula <- 10

#Hipótesis:
#H0: La máquina llena los bidones con una media de 10 [litro]
#Ha: La máquina llena los bidones con una media distinta de 10[litro]

SE  <- desvEst / sqrt(n)

# x <- seq( 5 * SE, 15 * SE, 0.01)
x <- seq( 9, 11, 1)
y <- dnorm(x, mean = mediaNula, sd = SE)
g1 <- ggplot(data = data.frame(x,y), aes(x))


g1 <- g1 + stat_function(
  fun = dnorm,
  args = list ( mean = mediaNula , sd = SE ) ,
  colour = "blue", size = 1)


g1 <- g1 + ylab ("")
g1 <- g1 + scale_y_continuous( breaks = NULL )
g1 <- g1 + scale_x_continuous( name = "Media de llenado de bidones",
                             breaks = seq ( 9 , 11 , 2) )

g1 <- g1 + theme_pubr()
g1 <- g1 + ggtitle("Distribución de las medias")
print(g1)

z1 <- 10
z2 <- 10.5

g2 <- g1 + geom_area(data = subset(data.frame(x,y), x < z1), aes(y = y), colour = "red", fill = "red", alpha = 0.5)
g2 <- g1 + geom_area(data = subset(data.frame(x,y), x > z2), aes(y = y), colour = "red", fill = "red", alpha = 0.5)

print(g2)

# Calcular la probablidad que suman las regiones de rechazo.
alfa1 <- pnorm(z1, mean = mediaNula, sd = SE, lower.tail = TRUE)
alfa2 <- pnorm(z2, mean = mediaNula, sd = SE, lower.tail = FALSE)
alfa <- alfa1 + alfa2
print("La probabilidad de cometer un error tipo I es:")
print(alfa)
#La probabilidad de cometer un error sería alfa1 + alfa2

#PREGUNTA 2:

#2. Si el verdadero volumen medio de los bidones fuera de 10,3 litros, ¿cuál sería la probabilidad de que el 
# ingeniero, que obviamente no conoce este dato, cometa un error de tipo II?

mediaVerdadera <- 10.3

x1 <- seq(mediaVerdadera - 3 * SE, mediaVerdadera + 3 * SE)
y1 <- dnorm(x1, mean = mediaVerdadera, sd = SE)
g3 <- g2 + stat_function(fun = dnorm, n = 9999, args = list(mean = mediaVerdadera, sd = SE), colour = "red", size = 1)
g3 <- g3 + geom_vline(xintercept = mediaVerdadera, colour = "red", linetype = "longdash")

g3 <- g3 + geom_area(data = subset(data.frame(x = x1, y = y1), x1 >= z1 & x1 <= z2), aes(y = y), colour = "red", fill = "red", alpha = 0.5)
g3 <- g3 + ggtitle("P2")

print(g3)

beta1 <- pnorm(z1, mean = mediaVerdadera, sd = SE,
                       lower.tail = TRUE)
beta2 <- pnorm(z2, mean = mediaVerdadera, sd = SE,
                       lower.tail = FALSE)
beta <- beta2 - beta1

print("Probabilidad de cometer un error tipo II: ")
print(beta)




