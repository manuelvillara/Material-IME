library (dplyr)
library (ggpubr)

# Integrantes Grupo 5:
# Wladimir Durán
# Rodrigo Hernández
# Manuel Villar

#--------------------------------------------PREGUNTA 1---------------------------------------------------------------
# 1. ¿Existe diferencia en la puntuación obtenida por los envases diseñados por LaKajita según las evaluaciones 
# realizadas por jóvenes y adultos?
  
# Lectura y filtro de datos
datos <- read.csv2("C:\\Users\\villa\\OneDrive\\Documentos\\inferencia\\EP10 Datos.csv")

# En este caso, se filtra por los jóvenes y adultos que puntuaron a "LaKajita"
datos1 <- filter(datos, Edad %in% c("Adulto","Joven") & Diseno == "LaKajita")

# Se grafican las dos muestras para comprobar normalidad de los datos mediante gráfico Q-Q
g <- ggqqplot (datos1 ,
               x = "Puntaje",
               y = "Edad",
               color = "blue")

g <- g + facet_wrap (~ Edad)
g <- g + rremove ("x.ticks") + rremove ("x.text")
g <- g + rremove ("y.ticks") + rremove ("y.text")
g <- g + rremove ("axis.title")
print(g)

# Como en los gráficos se observa que no están distribuidos normalmente, entonces aplicamos la prueba
# de Wilcoxon-Mann-Whitney

# Hipótesis:
# H0 -> no hay diferencia en la puntuación realizada por jóvenes y adultos. (u0 = ua)
# Ha -> hay diferencia en la puntuación realizada por jóvenes y adultos. (u0 != ua)

alfa <- 0.05
jovenes <- filter(datos1, Edad == "Joven")
adultos <- filter(datos1, Edad == "Adulto")

wilcox <- wilcox.test(jovenes[["Puntaje"]], adultos[["Puntaje"]], alternative = "two.sided", conf.level = 1 - alfa)
print(wilcox)

# Al obtener un p > alfa, se falla al rechazar la hipótesis nula. Se concluye, con un 95% de confianza, que no existe una diferencia significativa 
# entre el puntaje de jóvenes y adultos que le dieron al diseño de "LaKajita". 
# Por tanto, como la prueba omnibus no encontró diferencias significativas, no se realiza una prueba post-hoc.

#--------------------------------------------PREGUNTA 2---------------------------------------------------------------
# 2. ¿Existen diferencias entre las puntuaciones obtenidas para los diferentes envases de alfajor? De ser así, 
# ¿cuál(es) envase(s) se diferencia(n) de los demás?

# Se filtran los datos a utilizar
datos2 <- filter(datos, Producto == "Alfajor")

# Se comprueba normalidad mediante los gráficos Q-Q
g1 <- ggqqplot (datos2 ,
               x = "Puntaje",
               y = "Diseno",
               color = "red")

g1 <- g1 + facet_wrap (~ Diseno)
g1 <- g1 + rremove ("x.ticks") + rremove ("x.text")
g1 <- g1 + rremove ("y.ticks") + rremove ("y.text")
g1 <- g1 + rremove ("axis.title")
print(g1)

# Nos damos cuenta que las muestras no son independientes ya que una persona es capaz de calificar más de tipo de envase de alfajor.

# Cabe notar que no se cumple la condición 3 de ANOVA la cual indica que la población debe distribuirse normalmente, cosa que, mediante
# el gráfico Q-Q se comprueba que no cumple con esto, por lo que se opta por una prueba alternativa no parametrica.

# Por lo que se opta por utilizar la prueba de Friedman. Se comprobarán las condiciones para aplicar la prueba:

# 1. La variable independiente debe ser categórica y tener a lo menos tres niveles.
# Sí, es categórica y tiene 4 niveles.

# 2. La escala de la variable dependiente debe ser, a lo menos, ordinal.
# Sí, es de intervalos.

# 3. Los sujetos son una muestra aleatoria e independiente de la población.
# Se puede suponer razonablemente que las personas fueron elegidas aleatoriamnente e independientemente de la población.


# Hipótesis:
# H0: Todos los envases de alfajor poseen puntuaciones similares. (ua = ub = uc = ud)
# Ha: Al menos un envase de alfajor posee distinta puntuación a los demás. (ua != ub | ua != uc | ua != ud | ub != uc | ub != ud | uc != ud)

# Notar que se pasa el Id de variable numérica a categórica
datos2[["Id"]] <- factor(datos2[["Id"]])

# Se aplica la prueba de friedman
friedman <- friedman.test(Puntaje ~ Diseno | Id, data = datos2)
print(friedman)

# Al obtener un p > alfa, se falla al rechazar la hipótesis nula. Se concluye, con un 95% de confianza, que no existe una diferencia significativa 
# entre los distintos envases de alfajor. Por tanto, como la prueba omnibus no encontró diferencias significativas, no se realiza una prueba post-hoc.
