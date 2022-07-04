library(dplyr)
library(ggpubr)
library(leaps)
library(caret)
library(car)
options(max.print=1000000)

# Wladimir Dur�n
# Rodrigo Hernandez
# Manuel Villar


# Para esta actividad usaremos los datos de medidas anatómicas recolectados por Heinz et al. (2003) que ya 
# conocimos en el ejercicio práctico anterior. Como este ejercicio requiere de una variable dicotómica, vamos a 
# realizar lo siguiente:
# - Crear la variable IMC (índice de masa corporal) como el peso de una persona (en kilogramos) dividida por el 
# cuadrado de su estatura (en metros).
# - Si bien esta variable se usa para clasificar a las personas en varias clases de estado nutricional 
#   (bajo peso, normal, sobrepeso, obesidad, obesidad mórbida), para efectos de este ejercicio, usaremos dos clases: 
#   sobrepeso (IMC ≥ 25,0) y no sobrepeso (IMC < 25,0).
# - Crear la variable dicotómica EN (estado nutricional) de acuerdo al valor de IMC de cada persona.

# Se leen los datos.
datos <- read.csv2("D:\\EP13 Datos.csv")

# 1. Definir la semilla a utilizar, que corresponde a los primeros cinco dígitos del RUN del integrante de mayor edad
# del equipo.

set.seed(19920)

# 2. Seleccionar una muestra de 100 personas, asegurando que la mitad tenga estado nutricional “sobrepeso” y la
# otra mitad “no sobrepeso”.

# Variables ep anterior
# columnas <- colnames(datos)
# columnasAleatorias <- sample(columnas,8,replace = FALSE)

datos$Height <- datos$Height/100
datos$IMC <- datos$Weight / ((datos$Height)^2)

# Recomendación de la profe poner 25
datos$EN <- ifelse(datos$IMC > 25.0, "sobrepeso","no sobrepeso")
datos$EN <- factor(datos$EN)

sobrepeso <- filter(datos, EN == "sobrepeso")
noSobrepeso <- filter(datos, EN == "no sobrepeso")

# Muestra de 100 personas mitad sobrepeso y otra mitad no sobrepeso
muestraSobrepeso <- sample_n(sobrepeso, size = 50)
muestraNoSobrepeso <- sample_n(noSobrepeso, size = 50)
#print(muestraSobrepeso)
#print(muestraNoSobrepeso)
muestraConjunta <- rbind(muestraSobrepeso,muestraNoSobrepeso)

# 3. Usando las herramientas del paquete leaps, realizar una búsqueda exhaustiva para seleccionar entre dos y
# ocho predictores que ayuden a estimar la variable Peso (Weight), obviamente sin considerar las nuevas
# variables IMC ni EN, y luego utilizar las funciones del paquete caret para construir un modelo de regresión
# lineal múltiple con los predictores escogidos y evaluarlo usando bootstrapping.

datosExh <- muestraConjunta %>% select(!c("EN","IMC"))

subsets <- regsubsets(Weight ~., data = datosExh, method = "exhaustive",nbest=1, nvmax=8)
print(plot(subsets))

modelo <- lm( Weight ~ Chest.Girth + Waist.Girth + Thigh.Girth + Height , data = datosExh)
print(summary(modelo))

# Ahora para evaluar con bootstraping, hay que usar la función train() del paquete caret
# y hay que cambiar un parametro (te mandé la pagina por disc)
nEntrenamiento <- floor(0.7 * 100)
muestra <- sample.int(n = 100, size = nEntrenamiento, replace = FALSE)
entrenamiento <- datosExh[muestra, ]
prueba <- datosExh[-muestra, ]

modeloEv <- train(Weight ~ Chest.Girth + Waist.Girth + Thigh.Girth + Height , data = entrenamiento , method = "lm",
                  trControl = trainControl(method = "boot", number = 5))

print(modeloEv)

# 4. Haciendo un poco de investigaci?n sobre el paquete caret, en particular c?mo hacer Recursive Feature
# Elimination (RFE), construir un modelo de regresi?n lineal m?ltiple para predecir la variable IMC que incluya
# entre 10 y 20 predictores, seleccionando el conjunto de variables que maximice R2 y que use cinco
# repeticiones de validaci?n cruzada de cinco pliegues para evitar el sobreajuste (obviamente no se debe
# considerar las variables Peso, Estatura ni estado nutricional -Weight, Height, EN respectivamente).

x <- datosExh %>% select(!c("Weight", "Height"))
y <- muestraConjunta$IMC

xEntrenamiento <- x[muestra, ]
xPrueba <- x[-muestra, ]

yEntrenamiento <- y[muestra ]
yPrueba <- y[-muestra ]



modeloRfeLineal <- rfe(x = xEntrenamiento,
             y = yEntrenamiento,
             sizes = c(10:20),
             rfeControl = rfeControl(functions = lmFuncs,
                                     method = "repeatedcv",
                                     repeats = 5,
                                     number = 5))

print(modeloRfeLineal)

# 5. Usando RFE, construir un modelo de regresión logística múltiple para la variable EN que incluya el conjunto,
# de entre dos y seis, predictores que entregue la mejor curva ROC y que utilice validación cruzada dejando uno
# fuera para evitar el sobreajuste (obviamente no se debe considerar las variables Peso, Estatura –Weight y
#                                   Height respectivamente– ni IMC).

x1 <- muestraConjunta %>% select(!c("EN", "IMC", "Weight", "Height"))
y1 <- muestraConjunta$EN
 
x1Entrenamiento <- x1[muestra, ]
x1Prueba <- x1[-muestra, ]
 
y1Entrenamiento <- y1[muestra]
y1Prueba <- y1[-muestra]

modeloRfeLogistico <- rfe(x = x1Entrenamiento,
                 y = y1Entrenamiento,
                 sizes = c(2:6),
                 rfeControl = rfeControl(functions = lrFuncs,
                                         method = "repeatedcv",
                                         repeats = 5,
                                         number = 5))

print(modeloRfeLogistico)


# 6. Pronunciarse sobre la confiabilidad y el poder predictivo de los modelos.

# Verificamos la multicolinealidad del modelo

vifs <- vif(modeloRfeLineal)
cat("\nVerificar la multicolinealidad pasra el modelo Lineal :\n")
cat("- VIFs :\n")
print(vifs)
cat("- Tolerancias :\n")
print(1/vifs)
cat("- VIF medio :", mean(vifs), "\n")

vifs <- vif(modeloRfeLogistico)
cat("\nVerificar la multicolinealidad pasra el modelo Log�stico :\n")
cat("- VIFs :\n")
print(vifs)
cat("- Tolerancias :\n")
print(1/vifs)
cat("- VIF medio :", mean(vifs), "\n")

# Se verifica que la multicolinealidad no es excesivamente alta o baja, por lo que el modelo se puede considerar confiable y por consiguiente,
# tener un gran poder estad�stico

# (Al haber actualizado R studio, vif da un problema con los modelos que le entregamos el cual no pudimos solucionar :c ).



