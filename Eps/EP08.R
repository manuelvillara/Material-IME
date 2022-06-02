library (dplyr)
library (ggpubr)

#Wladimir Dur�n
#Rodrigo Hern�ndez
#Manuel Villar

datos <- read.csv2("C:\\Users\\villa\\OneDrive\\Escritorio\\EP08\\EP08 Datos.csv")
datos1 <- filter(datos, area %in% c("Economía", "Biología", "Computación"))

# Enunciado grupo 5:
# En este momento, los investigadores buscan determinar si existen diferencias en el tiempo que tardan los usuarios
# en formular una consulta para un problema de dificultad fácil en las áreas de biología, economía y computación.

#Hipótesis:
#H0: El tiempo que tardan los usuarios en formular una consulta para un problema fácil es igual para todas las áreas.
#Ha: El tiempo que tardan los usuarios en formular una consulta para un problema fácil es distinto para al menos una área.

#Para realizar la prueba de ANOVA se debe cumplir lo siguiente:

# 1. La escala con que se mide la variable dependiente tiene las propiedades de una escala de intervalos iguales.
cat("Se comprueban las condiciones para realizar ANOVA:\n")
#Se cumplen las propiedades de una escala de intervalos, debido a que la variable a medir es el tiempo (en segundos)

# 2. Las k muestras son obtenidas de manera aleatoria e independiente desde la(s) población(es) de origen.
#Con los datos ya filtrados, se tienen solamente las áreas a estudiar (Biología, Economía y Computación) y se procede a comprobar normalidad

# 3. Se puede suponer razonablemente que la(s) población(es) de origen sigue(n) una distribución normal.
#En los gráficos Q-Q de cada una de las áreas, se observa que las poblaciones se distribuyen normalmente
g <- ggqqplot (datos1 ,
               x = "tiempo",
               y = "area",
               color = "blue")

g <- g + facet_wrap (~ area)
g <- g + rremove ("x.ticks") + rremove ("x.text")
g <- g + rremove ("y.ticks") + rremove ("y.text")
g <- g + rremove ("axis.title")
print(g)

# 4. Las k muestras tienen varianzas aproximadamente iguales. (aplicar ANOVA)
cat("Se aplica la prueba de ANOVA usando aov: \n\n")
anova <- aov(tiempo ~ area, data = datos1)
print(summary(anova))
#Eligiendo un nivel de significación alfa = 0.025, se compara con el valor p recibido de la prueba: 
#Como p = 0.245 > alfa = 0.025, fallamos en rechazar la hipótesis nula.
#Para concluir, con un 97.5% de confianza, no hay evidencia suficiente para rechazar la hipótesis nula.
#por lo que el tiempo que tardan los usuarios en formular una consulta para un problema fácil es igual para todas las áreas (Economía, Biología y Computació


# Análisis Post-hoc
#Dado que se concluy� con un 97.5% de confianza que no hay evidencia para rechazar la hip�tesis nula, no es necesario realizar un 
#an�lisis post-hoc, esto porque este estudio se realiza para saber qu� elemento del grupo era distinto y al en este caso no existir un tiempo
#distinto para formular una consulta, no tiene sentido su realizaci�n.

