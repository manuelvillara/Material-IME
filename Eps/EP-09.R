library(tidyverse)
library(ggpubr)
library(ez)


texto <- ("
Instancia Optimo R R2 R3 G
'brock400_2' 26 15.6 16.3 17.2 19
'brock400_4' 30 13.8 16.3 17.4 19
'C2000.9' 77 54.2 56.6 59.4 63
'c-fat500-10' 123 122 122 122 123
'hamming10-2' 509 340.2 416.1 419.4 509
'johnson32-2-4' 13 13 13 13 13
'keller6' 56 31.5 40 42.5 45.2
'MANN_a81' 1097 1079.2 1079.2 1079.2 1092.9
'p-hat1500-1' 9 3.9 5.1 5.9 7
'p-hat1500-3' 91 72.8 74.7 81.6 83
'san1000' 12 4.6 4.6 4.7 7
'san400_0.7_1' 37 16.6 17.5 17.5 18
'san400_0.9_1' 97 41.1 51.4 53.4 89
'frb100-40' 97 63.4 73.7 77.5 79
'frb59-26-1' 56 36.2 42.9 45.3 45
'1et.2048' 313 229.4 265.4 277.9 289.4
'1zc.4096' 376 270.8 290.2 304.4 325.5
'2dc.2048' 21 12.6 15.7 16.9 18
")
datos <- read.table(textConnection(texto), header = TRUE)
#-------------------------------------------------------------------------------
# Pregunta 1
#-------------------------------------------------------------------------------
# ¿Hay algoritmos mejores que otrs?
#Hipótesis:

#H0: el tiempo promedio que tarda cada algoritmo en resolver el problema es igual
#H1: el tiempo promedio que tarda cada algoritmo en resolver el problema es distinto

instancia <- factor (1:18)
datos [["Instancia"]] <- factor(1:nrow ( datos ) )
datos <- datos %>% pivot_longer (c("R", "R2", "R3","G") ,
                                  names_to = "algoritmo", values_to = "tiempo")


datos [["algoritmo"]] <- factor ( datos [["algoritmo"]])


# Comprobci ón de normalidad .
g <- ggqqplot ( datos , x = "tiempo", y = "algoritmo", color = "algoritmo")
g <- g + facet_wrap (~ algoritmo )
g <- g + rremove ("x.ticks") + rremove ("x.text")
g <- g + rremove ("y.ticks") + rremove ("y.text")
g <- g + rremove ("axis.title")
print ( g )



#Cada grupo presenta varios valores atipicos


#prueba <- aov ( tiempo~algoritmo + Error ( Instancia /( algoritmo ) ) ,data = datos )


prueba2 <- ezANOVA ( data = datos , dv = tiempo , within = algoritmo ,
                     wid = Instancia , return_aov = TRUE )

print( summary ( prueba2$aov) )
print(prueba2)

#P de anova = 0.0016 < 0.05 -> se acepta la hipotesis alternativa, se rechaza la hipotesis nula

#------------------------------------------------------------------------------
#pREGUNTA 2
#------------------------------------------------------------------------------

