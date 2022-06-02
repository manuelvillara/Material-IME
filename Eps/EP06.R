
library(dplyr)
library(ggplot2)
library(pwr)
library(Hmisc)
options(scipen = 100)

# Grupo 5:

#Wladimir Durán
#Rodrigo Hernández
#Manuel Villar

# Crear tabla de contingencia.
mujeres <- c(54, 71, 35, 30, 45, 44, 56, 21, 17)
hombres <- c(52, 66, 41, 42, 65, 62, 88, 40, 35)
valorNulo <- 0.42

tabla <- as.table( rbind ( mujeres , hombres ) )

dimnames ( tabla ) <- list ( sexo = c(" mujeres ", " hombres ") ,
                               especialidad = c("Pediatría", "Obstetricia", "Dermatología",
                                                "Psiquiatría", "Medicina Interna", "Oncología",
                                                "Neurología", "Anestesiología", "Radiología") )

print ( tabla )


#---- Pregunta 1 ----
#   1. Estudios previos habían determinado que la proporción de autoras en la especialidad de medicina interna era 
# de 42%. ¿Respaldan estos datos tal estimación?

# Hipotesis nula: La proporción de autoras en medicina interna es de 42%
# Hipotesis alternativa: La proporción de autoras en medicina interna es distinta de 42%

# Se agrupan tanto a las mujeres de medicina interna como a la totalidad de especialistas de Medicina interna en variables
mujeresMedInt <- tabla[1,"Medicina Interna"]
medicinaInterna <- sum(mujeresMedInt,tabla[2,"Medicina Interna"])

# Se calcula como probabilidad de exito el encontrar una mujer dentro de la muestra
probExito <- round(mujeresMedInt / medicinaInterna,5)
probFracaso <- 1 - probExito
exitos <- probExito*medicinaInterna
alfa <- 0.05

# Se aplica prueba de wilson para comprobar la hipótesis planteada
pruebaWilson <- prop.test( exitos , n = medicinaInterna , p = valorNulo ,
                           alternative = "two.sided", conf.level = 1 - alfa )

print(pruebaWilson)
cat("Mediante la prueba de wilson, nos indica que la proporción de autoras y autores en medicina interna es de 0.40909, además de indicar que p > alfa, es decir el 40,9% de la gente de medicina interna corresponden a las mujeres, por lo que se cumple la hipótesis alternativa y se indica que la proporción de autoras es distinta del 42% estípulado como hipótesis nula.",sep = "\n")


#---- Pregunta 2 ----

#   2. Según estos datos, ¿es igual la proporción de autoras en las áreas de obstetricia y radiología?
#H0: La proporción de autoras en obstetricia es igual a la proporción de autoras en radiología.
#Ha: La proporción de autoras en obstetricia es distinta a la proporción de autoras en radiología.

#Valor nulo 0 debido a que la diferencia entre las proporciones debería ser 0 para que se cumpla la hipótesis nula.
valorNulo1 <- 0
alfa1 <- 0.05
mujeresObstetricia <- tabla[1,"Obstetricia"]
mujeresRadiologia <- tabla[1, "Radiología"]

# Se contabilizan la cantidad de personas en obstetricia y radiología
nObstetricia <- sum(mujeresObstetricia,tabla[2,"Obstetricia"])
nRadiologia <- sum(mujeresRadiologia,tabla[2,"Radiología"])
  
# Se calcula la proporción de éxito, en este caso, de encontrar mujeres en estas áreas estudiadas
propExitoObste <- round(mujeresObstetricia / nObstetricia,5)
propExitoRad <- round(mujeresRadiologia / nRadiologia,5)

# Se calcula la diferencia de las proporciones para poder comprobar la hipótesis planteada
diferencia <- probExitoObste - probExitoRad

# Se calcula los errores para obtener las zonas críticas de los intérvalos de confianza
errorObste <- (propExitoObste * (1 - propExitoObste)) / nObstetricia
errorRad <- (propExitoRad * (1 - propExitoRad)) / nRadiologia
errorEst <- sqrt (errorObste + errorRad)
zCritico <- qnorm (alfa1 / 2 , lower.tail = FALSE)
inferior <- diferencia - zCritico * errorEst
superior <- diferencia + zCritico * errorEst
cat ("\n Intervalo de confianza = [", inferior , ", ", superior , "]\n", sep = "")

# Prueba de hipótesis
pAgrupada <- ( probExitoObste + probExitoRad ) / ( nObstetricia + nRadiologia)
errorObste1 <- ( pAgrupada * (1 - pAgrupada ) ) / nObstetricia
errorRad1 <- ( pAgrupada * (1 - pAgrupada ) ) / nRadiologia


errorEstHip <- sqrt ( errorObste + errorRad1 )
Z <- ( diferencia - valorNulo1 ) / errorEstHip
p <- 2 * pnorm(Z , lower.tail = FALSE)

cat (" Hipótesis alternativa bilateral \n")
cat ("Z =", Z , "\n")
cat ("p =", p , "\n")


cat("\n Como el valor p se encuentra fuera del intervalo de confianza y además p < alfa, entonces no podemos decir que la proporción de autoras en obstetricia es igual a la de radiología, por lo que se acepta la hipótesis alternativa y se concluye que sus proporciones son distintas",sep = "\n")

#---- Pregunta 3 ----

#   3. Suponiendo que la diferencia en la proporción de autoras en la especialidad de anestesiología y la de pediatría
# es de 0,28. ¿A cuántos autores deberíamos monitorear para obtener un intervalo de confianza del 95% y poder 
# estadístico de 80%, si se intenta mantener aproximadamente la misma proporción de gente estudiada en cada 
# caso?   

# Se guardan las cantidades de mujeres en anestesiología, la totalidad de la muestra y se calcula la proporción de la mujeres en la anestesiología 
mujeresAnestesiologia <- tabla[1, "Anestesiología"]
totalAnestesiologia <- sum(mujeresAnestesiologia, tabla[2, "Anestesiología"])
proporcionAnestesiologia <- mujeresAnestesiologia/totalAnestesiologia

# Se guardan las cantidades de mujeres en pediatria, la totalidad de la muestra y se calcula la proporción de la mujeres en la pediatria 
mujeresPediatria <- tabla[1, "Pediatría"]
totalPediatria <- sum(mujeresPediatria, tabla[2, "Pediatría"])
proporcionPediatria <- mujeresPediatria/totalPediatria

# Se elige un alfa y se obtiene la fracción necesaria para calcular el tamaño de muestra necesario para las condiciones planteadas
alfa <- 0.05
fraccion <- totalAnestesiologia/(totalAnestesiologia + totalPediatria)

# Finalmente se calcula el tamaño de la muestra necesario para obtener un intervalo de confianza del 95% y poder estadístico del 80%
tamanoMuestra <- bsamsize(p1 = proporcionAnestesiologia, p2 = proporcionPediatria, fraction = diferencia, alpha = alfa, power = 0.8)
print(tamanoMuestra)
cat("\n Para poder cumplir con las condiciones pedidas de tanto poder 80% con un intervalo de confianza del 95%, se necesitarían a 83.95018 autores en anestesiología y a 424.31002 autores de pediatría",sep = "\n")
