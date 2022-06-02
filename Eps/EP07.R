library(dplyr)
library(ggplot2)
set.seed(685)

#Integrantes:
#Wladimir Duran 
#Rodrigo Hernandez
#Manuel Villar

# 1. En su desquiciada investigaci?n para acabar con los vampiros, Van Helsing ha descubierto que sus enemigos
# tienen predilecci?n por la sangre humana tipo AII+. El cazador sospecha que estos monstruos tienen
# preferencia por la sangre de los adultos, pero a?n no est? convencido. Por esta raz?n, mediante artima?as,
# ha encerrado a 14 ni?os y 20 adultos con este tipo de sangre en un reducto de vampiros. Tras 3 noches, 3 de
# los ni?os y 13 de los adultos fueron atacados. ?Existe relaci?n entre el ataque de vampiros y la edad de la
# v?ctima?
  
#Se pide determinar si existe relaci?n entre la edad de las v?ctimas de los vampiros y el ataque que estos realizan.

# H0: la edad no tiene relaci?n con el ataque de los vampiros
# HA: la edad est? realcionada con el ataque de los vampiros

#Se definen la cantidad de ni?os y adultos antes y despu?s del ataque.
ninos <- c(14, 3)
adultos <- c(20, 13)

#Se crea la tabla
tabla <- as.table( rbind ( ninos , adultos ) )
dimnames ( tabla ) <- list ( persona = c(" ninos ", " adultos ") ,
                   ataque = c("antes", "después") )
print(tabla)


# Aplicar prueba exacta de Fisher .
alfa <- 0.05
prueba <- fisher.test (tabla,1 - alfa)
print(prueba)

print("Por la prueba de Fisher se acepta H0, por lo que la edad de las victimas no esta relacionada con el ataque de los vampiros.")


# 2. Una Universidad ha detectado que muchos de sus nuevos estudiantes ingresan con elevados niveles de 
# ansiedad. Para ello, han decidido evaluar un nuevo programa de bienvenida que busca facilitar la adaptación 
# a la vida universitaria. Para ello, han reclutado a un grupo de 15 voluntarios a quienes se les midió el nivel de 
# ansiedad (alto o bajo) antes y después de participar en el programa de bienvenida:
# . 4 estudiantes no presentaron ansiedad ni antes ni después.
# . 5 estudiantes inicialmente ansiosos dejaron de estarlo.
# . 4 estudiantes mantuvieron un elevado nivel de ansiedad.
# . Los 2 estudiantes restantes desarrollaron síntomas de ansiedad tras participar en el programa.
# ¿Qué se puede concluir acerca del nuevo programa de bienvenida?

# Debemos determinar si el programa de bienvenida de la universidad est? siendo exitoso en t?rminos de lograr bajar la ansiedad de los estudiantes 
# que lo toman.

#H0: el programa no cambia los niveles de ansiedad en la Universidad
#Ha: el programa s? cambia los niveles de ansiedad en la Universidad


alumnos <- seq(1:15)
antes <- c(rep(" No Ansioso ", 4) , rep(" Ansioso ", 5) , rep(" Ansioso ", 4), rep(" No Ansioso ", 2))
despues <- c(rep(" No Ansioso ", 4) , rep(" No Ansioso ", 5) , rep(" Ansioso ", 4), rep(" Ansioso ", 2))
datos <- data.frame(alumnos, antes, despues)
tablaMcNemar <- table(antes,despues)
print(tablaMcNemar)

pruebaMcNemar <- mcnemar.test(tabla)
print(pruebaMcNemar)

print("Por prueba de mcNemar se rechaza H0 en favor de Ha, por lo que el programa estaria cambiando los niveles de ansiedad en la Universidad.")

# 3. En noviembre de 2019, se realizó un estudio acerca de la aprobación al presidente Sebastián Piñera entre 440 
# profesores y estudiantes de una prestigiosa universidad, obteniéndose los resultados que se muestran en la 
# tabla. ¿Son similares las opiniones de ambos segmentos de la comunidad universitaria?

#Se busca mostrar si las opinones entre los profesores y los estudiantes son similares en la Universidad.

#H0: Los profesores y estudiantes tienen preferencias iguales en cuanto a la aprobaci?n de Sebasti?n Pi?era
#Ha: Los profesores y los estudiantes tienen distintas opiniones en cuanto a la aprobaci?n de Sebasti?n Pi?era

#chi cuadrado homogeneidad

estudiantes <- c(35, 208, 17)
profesores <- c(20, 151, 9)

tabla <- as.table( rbind ( estudiantes , profesores ) )

dimnames ( tabla ) <- list ( persona = c("Estudiantes", "Profesores") ,
                             voto = c("Aprueba", "Desaprueba", "Ninguno") )
print(tabla)

pruebaChi <- chisq.test( tabla )
print(pruebaChi)

print("Por prueba de chi cuadrado de homogeneidad, se acepta H0, por lo que los estudiantes y los profesores tendrian iguales preferencias en cuanto a la aprobacion de
Sebastian Pignera.")


# 4. La Facultad de Ingeniería desea saber si existe diferencia significativa en el desempeño de los estudiantes en 
# asignaturas críticas de primer semestre. Para ello, le ha entregado un archivo de datos que, para 3 asignaturas, 
# indica si una muestra de 50 estudiantes aprobó o reprobó. ¿Qué puede concluir la Facultad? Indicación:
#   obtenga la muestra a partir del archivo EP07 Datos.csv, usando la semilla 685. Considere un nivel de 
# significación α=0,05.

#Queremos saber si es que existe relaci?n entre las aprobaciones y reprobaciones de las materias de c?lculo, algebra y f?sica.

#prueba chi cuadrado independencia

# 
# datosTabla <- read.csv2("C:\\Users\\rodri\\OneDrive\\Escritorio\\USACH\\1-2022\\IME\\Lecturas\\EP's\\EP07 Datos.csv")
# 
# print(datosTabla)
# 
# datosMuestra <- sample(datosTabla, 50)




