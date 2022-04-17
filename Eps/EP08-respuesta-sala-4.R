library("MASS")
library(ez)
library (tidyverse)
library(dplyr)
library ( ggpubr )
#-------------------------------------------------------------------------------
#                           PREGUNTA 1
#-------------------------------------------------------------------------------
# Los desórdenes alimenticios son un problema de salud mental bastante frecuente 
# que afectan, solo en Estados Unidos, a millones de niños y adolescentes. 
# El paquete MASS de R incluye el conjunto de datos anorexia que reporta el 
# peso inicial, el peso final y el tratamiento recibido por 72 mujeres con 
# diagnóstico de anorexia. Determine si existen diferencias significativas 
# en el peso final de las mujeres para cada tratamiento.

# Respuesta:

# Manejo de datos

datos <- anorexia
datos [["instancia"]] <- factor(1:nrow ( datos ) )

# Hipótesis a contrastar son: 

# Hipótesis Nula (H0)
# H0: No existen diferencias significativas en el peso final de las mujeres para
# cada tratamiento.

# Hipótesis alternativa (HA)
# HA: Existen diferencias significativas en el peso final de las mujeres para 
# cada tratamiento.


# Verificación de ANOVA

# 1. La escala con que se mide la variable dependiente tiene las propiedades de 
# una escala de intervalos iguales.

# Para este caso, las mediciones de las variables se encuentran dentro de un 
# mismo rango de valores, por lo que se puede asumir que estas poseen una escala
# de intervalos iguales ?


# 2. Las k muestras son obtenidas de manera aleatoria e independiente desde 
# la(s) población(es) de origen.

# Las muestras cumplen con esta condición, puesto que estas representan solo
# una parte de la población de estudio y ademas, una muestra no depende de los
# resultados de la otra.


# 3. Se puede suponer razonablemente que la(s) población(es) de origen sigue(n) 
# una distribución normal.

g <- ggqqplot ( datos ,x = "Postwt",y = "Treat",color = "Treat")
g <- g + facet_wrap (~ Treat )
g <- g + rremove ("x.ticks") + rremove ("x.text")
g <- g + rremove ("y.ticks") + rremove ("y.text")
g <- g + rremove ("axis.title")
print ( g )

# A partir de los gráficos obtenidos, se observa que existen ciertos valores
# atípicos, en especial en el tratamiento de tipo FT, sin embargo, estos tienden
# a seguir una distribución normal, por lo que será recomendable utilizar un 
# nivel de significación bajo para evitar cometer errores.


# 4. Las k muestras tienen varianzas aproximadamente iguales.

# Para verificar esta condición, se utiliza el la prueba ezANOVA que incluye el
# test de Levene, el cual nos ayudará a determinar si las muestras tienen 
# varianzas similares.

pruebaAnova <- ezANOVA (data = datos ,
                        dv = Postwt ,
                        between = Treat ,
                        wid = instancia ,
                        type = 3,
                        return_aov = TRUE )
print( pruebaAnova )

# $ANOVA
# Effect  DFn   DFd        F            p p<.05       ges
#   2    Treat   2   69 8.650628     0.0004443275     * 0.2004751

# $`Levene's Test for Homogeneity of Variance`
# DFn DFd      SSn        SSd          F        p p<.05
#  1   2  69 86.99993   1698.513   1.767133    0.1784648 

# El test de Levene nos indica que las varianzas son aproximadamente similares,
# ya que entrega un valor de p = 0.1784648, superior al nivel de significación
# con el que este se evalúa (a = 0.05).

# Igualmente, el test de ANOVA nos indica que se rechaza la hipótesis nula, pues 
# con el se obtuvo un valor de p = 0.0004443275, por mucho inferior al nivel
# de significación del mismo (a = 0.05), permitiendo concluir que 
# los tratamientos poseen diferencias significativas entre ellos


# Prueba POST-HOC

# Para determinar que tratamientos son los que poseen diferencias significativas
# se realiza la prueba de HSD Turkey.
# Como se presentaban algunos valores atípicos, se utilizará un nivel de 
# significación inferior a 0.05

alfa = 0.025

# Prueba HSD de Tukey
anova <- aov( Postwt ~ Treat , data = datos )

post_hoc <- TukeyHSD ( anova ,"Treat",
                       ordered = TRUE ,
                       conf.level = 1 - alfa )
print ( post_hoc )

#resultado TukeyHSD
#              diff        lwr       upr     p adj
# CBT-Cont 4.588859 -0.6756141  9.853333 0.0581141
# FT-Cont  9.386425  3.3067223 15.466128 0.0002930
# FT-CBT   4.797566 -1.1565149 10.751647 0.0864030


# Conclusión:

# Como fue mencionado en el test de ANOVA, se rechaza la hipótesis nula, debido 
# a que si existen diferencias significativas con al menos uno de los 
# tratamientos. 
# En concreto, gracias al test de HSD Turkey, podemos notar que 
# los tratamientos que difieren corresponden al FT y Cont, donde se obtuvo
# un valor p = 0.0002930, inferior al nivel de significación (a = 0.025).
# Esto nos permite asegurar con un nivel de confianza de 97.5% que si existen
# diferencias significativas entre los tratamientos, siendo el tratamiento Cont
# el que nos permite afirmar esto al presentar mayores cambios en el peso final
# de las mujeres en comparación con el tratamiento FT.


