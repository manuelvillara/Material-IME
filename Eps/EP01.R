library(dplyr)

# ¿Qué variables se han cargado?
# Resp: Se han cargado dos variables, las "regiones" y las "fechas". Estas representan los números de casos diarios por región.
#¿Qué tipo tiene cada una de estas variables?
# Resp: Variable categórica nominal las "regiones" y Variable categórica ordinal las "fechas".
#¿Qué escalas parecen tener estas variables?
#Resp: Escala nominal las "regiones" y escala ordinal las "fechas".

#Se cargan los datos al csv.
datos <- read.csv2("EP01 Datos Covid.csv")

#Se filtra el dataframe cargado anteriormente obteniendo solamente los datos de la región del Maule con la función filter().
maule <- datos %>% filter(Region == "Maule")

#Se filtra nuevamente, ahora por las fechas  especificadas en el enunciado y luego se obtiene el máximo del intervalo
#con la función max().
intervalo <- maule %>% select(ends_with("01.03.2021"):ends_with("31.08.2021"))
max <- max(intervalo)

#Se define un contador.
c = 0

#Se itera sobre el intervalo especificado y se va aumentando el contador, al llegar al máximo que se guardó en la variable
#max, se printea el día y la cantidad de casos de ese día que es el máximo.
for(i in intervalo){
  c = c + 1
  if(i == max){
    cat("Día con la cantidad máxima de casos del periodo:\n")
    print(intervalo[c])
  }
}

#Para cada uno de los meses se aplica la suma de las filas seleccionando desde el intervalo las fechas especificadas
#se usa ends_with() para buscar aquellas fechas que terminen con el string especificado.

marzo2021 <- rowSums(intervalo %>% select(ends_with("01.03.2021"):ends_with("31.03.2021")))
cat("Casos con síntomas Marzo-2021: ", marzo2021)

abril2021 <- rowSums(intervalo %>% select(ends_with("01.04.2021"):ends_with("30.04.2021")))
cat("Casos con síntomas Abril-2021: ", abril2021)

mayo2021 <- rowSums(intervalo %>% select(ends_with("01.05.2021"):ends_with("31.05.2021")))
cat("Casos con síntomas Mayo-2021: ", mayo2021)

junio2021 <- rowSums(intervalo %>% select(ends_with("01.06.2021"):ends_with("30.06.2021")))
cat("Casos con síntomas Junio-2021: ", junio2021)

julio2021 <- rowSums(intervalo %>% select(ends_with("01.07.2021"):ends_with("31.07.2021")))
cat("Casos con síntomas Julio-2021: ", julio2021)

agosto2021 <- rowSums(intervalo %>% select(ends_with("01.08.2021"):ends_with("31.08.2021")))
cat("Casos con síntomas Agosto-2021: ", agosto2021)
