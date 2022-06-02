# ¿qué variables se han cargado?
# Resp: 
# Las variables que podemos observar son las regiones y las fechas
# en que hubo contagios.



# ¿qué tipo tiene cada una de estas variables?
# Resp:
# Region: Corresponde a una variable categorica del tipo nominal.
# Fechas: Al cargar los datos corresponde a una variable numerica del tipo discreta.
# Al momento de ocupar de pivotar la matriz esta pasa a ser una variable categorica 
# del tipo ordinal. 

# Ademas al pivotar la matriz se crea la variable contagiados la cual es numerica
# del tipo discreta




# ¿qué escala parecen tener estas variables?
# Resp: 
# Region: escala nominal

# Fecha: escala discreta (al inicio)
#        escala ordinal (al pivotar la matriz)

# Contagiados: escala discreta (al pivotar la matriz)

#############################################################################
#############################################################################
#############################################################################

# Instalacion de paquetes a usar
if(!require(dplyr)){
  install.packages("dplyr", dependencies = TRUE)
  require(dplyr)
}

if(!require(tidyr)){
  install.packages("tidyr", dependencies = TRUE)
  require(tidyr)
}

# Se lee el archivo de entrada
# Al usar el comando View(covid) en la consola se puede observar que
# los tipos de variables se reflejan correctamente
covid <- read.csv(file.choose(), encoding = "UTF-8")

# Se selecciona la fila que necesitamos
aricaParinacota <- covid[1:1,]

# Se alarga la matriz para trabajar las fechas en filas
aricaParinacotaPivot <- aricaParinacota %>% pivot_longer(cols = starts_with("X"),
                                                         names_to = "Fecha",
                                                         values_to = "Contagiados",
                                                         values_drop_na = TRUE)

# Se filtra por las fechas solicitadas
fechasRango <- aricaParinacotaPivot %>% filter(Fecha >= "X2020.07.01" & Fecha <= "X2020.12.31")

# Se encuentra el mayor numero de contagiados
MaxCasos <- max(fechasRango[["Contagiados"]])

# Se muestra el dia en el que detecto un mayor numero de contagios
Dia <- fechasRango %>% filter(Contagiados == MaxCasos)
print(Dia[["Fecha"]])

# Se filtra por cada mes para obtener el total de casos
mes7 <- fechasRango %>% filter(Fecha >= "X2020.07.01" & Fecha <= "X2020.07.31")
print(sum(mes7$Contagiados))

mes8 <- fechasRango %>% filter(Fecha >= "X2020.08.01" & Fecha <= "X2020.08.31")
print(sum(mes8$Contagiados))

mes9 <- fechasRango %>% filter(Fecha >= "X2020.09.01" & Fecha <= "X2020.09.30")
print(sum(mes9$Contagiados))

mes10 <- fechasRango %>% filter(Fecha >= "X2020.10.01" & Fecha <= "X2020.10.31")
print(sum(mes10$Contagiados))

mes11 <- fechasRango %>% filter(Fecha >= "X2020.11.01" & Fecha <= "X2020.11.30")
print(sum(mes11$Contagiados))

mes12 <- fechasRango %>% filter(Fecha >= "X2020.12.01" & Fecha <= "X2020.12.31")
print(sum(mes12$Contagiados))