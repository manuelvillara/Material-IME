

library(dplyr)
library(ggpubr)

datos <- read.csv2("EP04 datos.csv")
set.seed(69420)

# Grupo 5:
# WLADIMIR DURÁN
# RODRIGO HERNÁNDEZ
# MANUEL vILLAR

#--- Pregunta 1 ---
#   1. El Comité Olímpico cree que el mejor tiempo medio de los atletas blancos después de ingresar al programa 
# de entrenamiento es inferior a 12,87 segundos. ¿Soportan los datos esta afirmación?
# Hipótesis Nula: Los atletas blancos marcan 12,87 segundos como media.
# Hipótesis Alternativa: Los atletas blancos no marcan 12,87 segundos como media.

razaBlanca <- datos %>% filter(Raza == "Blanca")
mediaPosteriorBlanca <- mean(razaBlanca[["Posterior"]])
mejorTiempoMedio <- min(mediaPosteriorBlanca)
normalidad <- shapiro.test(razaBlanca[["Posterior"]])
print(normalidad)
tResult <- t.test(razaBlanca[["Posterior"]], mu = 12.87,alternative = "two.sided")

print("Eligiendo un nivel de significación de α = 0.1, se procede a hacer la prueba de normalidad de Shapiro")
print("La prueba entrega:")
print(normalidad)
print("Con el valor de P obtenido, se falla en rechazar la hipótesis nula, por tanto se asume normalidad en los datos")
print("Se ejecuta la prueba t de Student:")
print(tResult)
print("Se rechaza el Ho en favor del Ha ya que el mejor tiempo medio de los atletas blancos es distinto de 12,87 segundos.")
print("Se concluye por tanto, que los datos soportan la afirmación,")


#--- Pregunta 2 ---
#   2. ¿Sugieren los datos que la mejor marca de los atletas orientales se reduce en más de 5,27 segundos tras el 
# entrenamiento?
# Hipótesis Nula: Los atletas orientales reducen su mejor marca en más de 5,27 segundos tras el entrenamiento.
# Hipótesis Alternativa: Los atletas orientales no reducen su mejor marca en más de 5,27 segundos tras el entrenamiento.

razaOriental <- datos %>% filter(Raza == "Oriental")
minimoOrientalPrevio <- razaOriental[["Previo"]]
minimoOrientalPosterior <- razaOriental[["Posterior"]]
normalidadPrevio <- shapiro.test(razaOriental[["Previo"]])
normalidadPosterior <- shapiro.test(razaOriental[["Posterior"]])

print("Eligiendo un nivel de significación de α = 0.1, se procede a hacer la prueba de normalidad de Shapiro")
print(normalidadPosterior)
print(normalidadPrevio)
print("Con el valor de P obtenido, se falla en rechazar la hipótesis nula, por tanto se asume normalidad en los datos")
print("Se ejecuta la prueba t de Student:")
tResult2 <- t.test(razaOriental[["Previo"]],razaOriental[["Posterior"]], mu = 5.27,alternative = "less")
print(tResult2)
print("Se acepta el Ho ya que la mejor marca se reduce en más de 5,27 segundos.")
print("Por tanto, los datos los avalan que se reduce en más de 5,27 segundos la marca de los atletas orientales tras el entrenamiento.")


#--- Pregunta 4 ---
#   4. ¿Es posible afirmar que, en promedio, los atletas negros superan a los blancos por 0,76 segundos antes del 
# entrenamiento?
# Hipótesis Nula: Los atletas negros superan a los blancos por 0,76 segundos antes del entrenamiento. 
# Hipótesis Alternativa: Los atletas negros no superan a los blancos por 0,76 segundos antes del entrenamiento. 

razaNegra <- datos %>% filter(Raza == "Negra")
normalidadNegra <- shapiro.test(razaNegra[["Previo"]])
normalidadBlanca <- shapiro.test(razaBlanca[["Previo"]])
print("Eligiendo un nivel de significación de α = 0.1, se procede a hacer la prueba de normalidad de Shapiro")
print(normalidadNegra)
print(normalidadBlanca)
print("Con el valor de P obtenido, se falla en rechazar la hipótesis nula, por tanto se asume normalidad en los datos")
print("Se ejecuta la prueba t de Student:")
tResult3 <- t.test(razaNegra[["Previo"]],razaBlanca[["Previo"]], mu = 0.76,alternative = "two.sided",paired=FALSE)
print(tResult3)
print("Se rechaza el Ho en favor del Ha ya que la diferencia de las medias antes del entrenamiento no es de 0.76 segundos.")
print("Por tanto, no es posible afirmar que, en promedio, los atletas negros superan a los blancos antes del entrenamiento")


