library(ggpubr)
library(pwr)
library (ggplot2)

##############################################################################################
#  ___   ___   ___    ___   _   _   _  _   _____     _       _ 
# | _ \ | _ \ | __|  / __| | | | | | \| | |_   _|   /_\     / |
# |  _/ |   / | _|  | (_ | | |_| | | .` |   | |    / _ \    | |
# |_|   |_|_\ |___|  \___|  \___/  |_|\_|   |_|   /_/ \_\   |_|                                                                                                                     
##############################################################################################
# Se sabe que el proceso de fabricación de barras de acero para concreto reforzado producen 
# barras con medidas de dureza que siguen una distribución normal con desviación estándar de
# 10 kilogramos de fuerza por milímetro cuadrado. Usando una muestra aleatoria de tamaño 25, 
# un ingeniero quiere averiguar si una línea de producción está generando barras con dureza 
# media de 170 [kgf mm-2] 

# Pregunta 1: Si el ingeniero piensa rechazar la hipótesis nula cuando la muestra presente 
# una media menor a 167 [kgf mm-2] o mayor a 173 [kgf mm-2], ¿cuál es la probabilidad de que
# cometa un error de tipo 1? 

# Datos conocidos
n <- 25
diferencia <- 3
sd <- 10
poder <- 0.8
mediaTeorica <- 170
resultado <- power.t.test(n = n,
                          delta = diferencia,
                          sd = sd,
                          sig.level = NULL,
                          power = poder,
                          type = "one.sample",
                          alternative = "two.sided")$sig.level

print(resultado) 

# Cálculo del error estándar
SE <- sd / sqrt(n)

# Gráfico con la  distribución de la media teórica
x <- seq(162, 180, 0.01)
y <- dnorm(x, mean = mediaTeorica, sd = SE)
dfTeorico <- data.frame(x, y)
g <- ggplot(data = dfTeorico, aes(x))

g <- g + stat_function( fun = dnorm,  
                        args = list(mean = mediaTeorica, sd = SE),
                        colour = "red", 
                        size = 1)
g <- g + ylab("")
g <- g + scale_y_continuous(breaks = NULL,name = "Probabilidad")
g <- g + scale_x_continuous(name = "Dureza media [kgf mm-2]")
g <- g + ggtitle("Distribución de la muestra")
g <- g + theme_pubr()

# Área de rechazo de H0
Z_critico <- qnorm(resultado/2, mean = 0, sd = SE, lower.tail = FALSE)
q_critico_inferior <- mediaTeorica - Z_critico
q_critico_superior <- mediaTeorica + Z_critico

g <- g + geom_area(data = subset(dfTeorico, x < q_critico_inferior),
                   aes(y = y),
                   colour = "red",
                   fill = "red",
                   alpha = 0.5)

g <- g + geom_area(data = subset(dfTeorico, x > q_critico_superior),
                   aes(y = y),
                   colour = "red",
                   fill = "red",
                   alpha = 0.5)

print(g)

# Conclusión
# Como se puede apreciar en el resultado de la función power.t.test() así como también en el gráfico 
# de la distribución, el valor obtenido de α corresponde a 0.51, lo cual nos indica
# que la probabilidad de que el ingeniero cometa un error del tipo 1 es bastante
# alta, alcanzando casi un 50%.



##############################################################################################
#  ___   ___   ___    ___   _   _   _  _   _____     _       ___ 
# | _ \ | _ \ | __|  / __| | | | | | \| | |_   _|   /_\     |_  )
# |  _/ |   / | _|  | (_ | | |_| | | .` |   | |    / _ \     / / 
# |_|   |_|_\ |___|  \___|  \___/  |_|\_|   |_|   /_/ \_\   /___|                                                         
##############################################################################################
# Si la verdadera dureza media de la línea de producción fuera 172 [kgf mm-2], 
# ¿cuál sería la probabilidad de que el ingeniero, que obviamente no conoce este dato, cometa
# un error de tipo 2? 


# Formulación de la hipótesis 
# Tomando µ1 como la media muestral equivalente a 170 tenemos:

# Hipótesis nula
# H0: µ1 = 170

# Hipótesis alternativa
# HA: µ1 /= 170

# Datos conocidos
n <- 25
sd <- 10
mediaTeorica <- 170
mediaMuestral <- 172
diferencia <- mediaMuestral - mediaTeorica
d <- (mediaMuestral - mediaTeorica) / sd  #Tamaño del efecto (d de Cohen)
alfa <- 0.05

# Cálculo del poder usando la función power.t.test()
poder <- power.t.test(n = n,
                      delta = diferencia,
                      sd = sd,
                      sig.level = alfa,
                      type = "one.sample",
                      alternative = "two.sided")$power

cat("Poder = ", poder, "\n")

# Cálculo del poder Teórico en R
poderTeorico <- pwr.t.test(n = n,
                           d = d,
                           sig.level = alfa,
                           power = NULL,
                           type = "one.sample",
                           alternative = "two.sided")$power

# Cálculo del poder Experimental 

# Error estándar
SE <- sd / sqrt(n)

# Gráfico con la  distribución de la media teórica
x <- seq(162, 180, 0.01)
y <- dnorm(x, mean = mediaTeorica, sd = SE)
dfTeorico <- data.frame(x, y)
g <- ggplot(data = dfTeorico, aes(x))
g <- g + stat_function( fun = dnorm, 
                        args = list(mean = mediaTeorica, sd = SE),
                        colour = "orange",
                        size = 1)
g <- g + ylab("")
g <- g + scale_y_continuous(breaks = NULL,name ="Probabilidad")
g <- g + scale_x_continuous(name = "Dureza media [kgf mm-2]")
g <- g + theme_pubr()

# Área de rechazo de H0
Z_critico <- qnorm(alfa/2, mean = 0, sd = SE, lower.tail = FALSE)
q_critico_inferior <- mediaTeorica - Z_critico
q_critico_superior <- mediaTeorica + Z_critico

g <- g + geom_area(data = subset(dfTeorico, x < q_critico_inferior),
                   aes(y = y),
                   colour = "orange",
                   fill = "orange",
                   alpha = 0.5)

g <- g + geom_area(data = subset(dfTeorico, x > q_critico_superior),
                   aes(y = y),
                   colour = "orange",
                   fill = "orange",
                   alpha = 0.5)

# Gráfico con la  distribución de la media muestral
x1 <- seq(162, 180, 0.01)
y1 <- dnorm(x, mean = mediaMuestral, sd = SE)
dfMuestral <- data.frame(x1, y1)

g <- g + stat_function( fun = dnorm, 
                        args = list(mean = mediaMuestral, sd = SE),
                        colour = "blue",
                        size = 1)

g <- g + geom_area(data = subset(dfMuestral, x < q_critico_inferior),
                   aes(x = x1, y = y1),
                   colour = "blue",
                   fill = "blue",
                   alpha = 0.5)

g <- g + geom_area(data = subset(dfMuestral, x > q_critico_superior),
                   aes(x = x1, y = y1),
                   colour = "blue",
                   fill = "blue",
                   alpha = 0.5)
g <- g + ggtitle("Distribuciones de las medias")

print(g)

# Cálculo del poder experimental
probAreaInf <- pnorm(q_critico_inferior, mean = mediaMuestral, sd = SE, lower.tail = TRUE)
probAreaSup <- pnorm(q_critico_superior, mean = mediaMuestral, sd = SE, lower.tail = FALSE)
poderExperimental <- probAreaInf + probAreaSup

# Cálculo de la probabilidad de cometer un error tipo II 
betaTeorico <- 1 - poderTeorico
betaExperimental <- 1- poderExperimental
cat("Poder teórico = ", poderTeorico, "\n")
cat("Poder experimental = ", poderExperimental, "\n")
cat("Beta teórico = ", betaTeorico, "\n")
cat("Beta experimental = ", betaExperimental, "\n")

# Conclusión
# Si la verdadera media es de 172 [kgf mm-2] existe una posibilidad de que el ingeniero cometa
# un error del tipo 2, es decir, que considere correcta H0 en lugar de HA, cuando HA es la 
# correcta, pues como se ve en el gráfico, la región azul, que permite determinar el poder 
# estadístico, cubre menos del 20% de la curva de dicho color, lo cual se puede corroborar al
# calcular el área que esta abarca, obteniéndose un poder de 0.17, de donde podemos afirmar que 
# existe una probabilidad de casi el 83% de que el ingeniero cometa un error del tipo 2.



##############################################################################################
#  ___   ___   ___    ___   _   _   _  _   _____     _       ____
# | _ \ | _ \ | __|  / __| | | | | | \| | |_   _|   /_\     |__ /
# |  _/ |   / | _|  | (_ | | |_| | | .` |   | |    / _ \     |_ \
# |_|   |_|_\ |___|  \___|  \___/  |_|\_|   |_|   /_/ \_\   |___/                                                       
##############################################################################################
# Como no se conoce la verdadera dureza media, genere un gráfico del poder 
# estadístico con las condiciones anteriores, pero suponiendo que las verdaderas durezas 
# medias podrían variar de 162 a 178 [kgf mm-2].

# Datos conocidos 
n <- 25
sd <- 10
alfa <- 0.05
mediaTeorica <- 170
mediaInferior <- 162
mediaSuperior <- 178

deltaInf <- mediaInferior- mediaTeorica
deltaSup <- mediaSuperior- mediaTeorica

intervaloDelta <- seq(deltaInf, deltaSup, 0.01)
intervaloDurezas <- seq(mediaInferior, mediaSuperior, 0.01)

# Se calcula el poder para cada dureza media
poder <- power.t.test(n = n,
                      delta = intervaloDelta,
                      sd = sd,
                      sig.level = alfa,
                      power = NULL,
                      type = "one.sample",
                      alternative = "two.sided")$power

# Se crea un data frame
datos <- data.frame(intervaloDurezas, poder)

# Gráfico de la curva de poder
g <- ggplot(datos, aes(intervaloDurezas, poder))
g <- g + geom_line(colour = "blue")
g <- g + ylab("Poder estadístico")
g <- g + xlab("Dureza media")
g <- g + theme_pubr()
g <- g + geom_vline(xintercept = 170, linetype = "dashed")
g <- g + scale_x_continuous(breaks = seq(mediaInferior, mediaSuperior, 2))
g <- g + ggtitle("Relación entre el poder y la dureza media")
print(g)

# Conclusión
# En el gráfico se observa que, a medida que la dureza media se aleja de la media teórica (170), 
# el poder de la prueba comienza a aumentar, disminuyendo así la probabilidad de cometer errores 
# del tipo II.

##############################################################################################
#  ___   ___   ___    ___   _   _   _  _   _____     _       _ _  
# | _ \ | _ \ | __|  / __| | | | | | \| | |_   _|   /_\     | | | 
# |  _/ |   / | _|  | (_ | | |_| | | .` |   | |    / _ \    |_  _|
# |_|   |_|_\ |___|  \___|  \___/  |_|\_|   |_|   /_/ \_\     |_|                                                                                                                      
##############################################################################################
# ¿Cuántas barras deberían revisarse para conseguir un poder estadístico de 
# 0,80 y un nivel de significación de 0,05? 

# Datos conocidos
poder <- 0.8
alfa <- 0.05
sd <- 10
mediaTeorica <- 170
mediaMuestral <- 172  

diferencia <- mediaMuestral - mediaTeorica

# Cálculo del tamaño de la muestra usando la función power.t.test()
resultado <- power.t.test(n = NULL,
                          delta = diferencia,
                          sd = sd,
                          sig.level = alfa,
                          power = poder,
                          type = "one.sample",
                          alternative = "two.sided")

n <- ceiling(resultado[["n"]])
print(n)

# Conclusión    
# A través de la prueba realizada se puede concluir que se necesitan revisar 199 barras
# para conseguir un poder estadístico de 0.8 y un nivel de significación de 0.05.

##############################################################################################
#  ___   ___   ___    ___   _   _   _  _   _____     _       ___ 
# | _ \ | _ \ | __|  / __| | | | | | \| | |_   _|   /_\     | __|
# |  _/ |   / | _|  | (_ | | |_| | | .` |   | |    / _ \    |__ \
# |_|   |_|_\ |___|  \___|  \___/  |_|\_|   |_|   /_/ \_\   |___/                                                                                                                                                                                   
##############################################################################################
# ¿Y si quisiera ser bien exigente y bajar la probabilidad de cometer un error
# de tipo 1 a un 1% solamente?

# Datos conocidos
poder <- 0.8
alfa <- 0.01
sd <- 10
mediaTeorica <- 170
mediaMuestral <- 172   
diferencia <- mediaMuestral - mediaTeorica

resultado <- power.t.test(n = NULL,
                          delta = diferencia,
                          sd = sd,
                          sig.level = alfa,
                          power = poder,
                          type = "one.sample",
                          alternative = "two.sided")

n <- ceiling(resultado[["n"]])
print(n)

# Conclusión  
# Con un poder de 0.8 y un alfa menor que el ejercicio anterior, en este caso un nivel
# de significancia de 0.01 equivalente al 1%, el tamaño de la muestra debería ser
# de 296.
