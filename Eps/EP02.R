#Grupo 5

#La pregunta asignada fue �Son similares los ingresos registrados en las diferentes provincias de la RM? 
#Se ocupar� la "media" y se comparar�n cada una de las medias obtenidas en las distintas provincias. 
#Para graficar se utilizar� uno de barras, con la funci�n ggbarplot, y se comparar�n donde el eje X ser�n las provincias y en el eje Y se encontrar�n los valores (sueldos).

library(ggpubr)
library(dplyr)

#Recordar colocar la ruta en donde está guardado el archivo en su equipo en caso de que no se abra.
datos <- read.csv2("EP02 Datos Casen 2017.csv", stringsAsFactors = FALSE)

region <- datos %>% filter(region == "RegiÃ³n Metropolitana de Santiago")

provincias <- region %>% filter(provincia != "")

#Se agrupan las provincias y se les asigna la media de ingresos respectiva
ingresosprovincias <- group_by(provincias, provincia) %>%
  summarise (count = n(), ingresos = mean(ytot))

#Se separan los ingresos y las provincias y luego se crea un dataframe con los datos
ingresos <- ingresosprovincias[["ingresos"]]
provincias <- ingresosprovincias[["provincia"]]
datos <- data.frame(provincias,ingresos)

#Se crea el gráfico de barras para ver los ingresos por provincia
g <- ggbarplot ( datos ,
                 x = "provincias",
                 y = "ingresos",
                 title = "Ingresos por provincia",
                 xlab = " Provincias ",
                 ylab = " Ingresos [clp]")

print (g)
print(ingresosprovincias)