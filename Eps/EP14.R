library(dplyr)
library(ggpubr)
library(plyr)
library(pROC)


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
datos <- read.csv2("C:\\Users\\villa\\OneDrive\\Escritorio\\EP14\\EP13 Datos.csv")

# 1. Definir la semilla a utilizar, que corresponde a los últimos cuatro dígitos del RUN (sin considerar el dígito 
# verificador) del integrante de menor edad del equipo.

set.seed(0828)

# 2. Seleccionar una muestra de 120 mujeres (si la semilla es un número par) o 120 hombres (si la semilla es impar), 
# asegurando que la mitad tenga estado nutricional “sobrepeso” y la otra mitad “no sobrepeso”. Dividir 
# esta muestra en dos conjuntos: los datos de 80 personas (40 con EN “sobrepeso”) para utilizar en la 
# construcción de los modelos y 40 personas (20 con EN “sobrepeso”) para poder evaluarlos.

# Variables EP anterior
columnas <- colnames(datos)
columnasAleatorias <- sample(columnas,8,replace = FALSE)

# Se filtran los datos para tener las columnas aleatorias del EP anterior en nuestros datos a trabajar.
datosBind <- datos %>% select(columnasAleatorias,Gender)
datosBind <- filter(datosBind, Gender == 0)

datosSeleccionados <- datos %>% select(Weight,Height,Gender)

datosFiltrados <- filter(datosSeleccionados, Gender == 0)

# Se pasa la estatura a metros (estaba en centímetros).
datosFiltrados$Height <- datosFiltrados$Height/100

# Se crea la columna del IMC con la fórmula dada.
datosFiltrados$IMC <- datosFiltrados$Weight / ((datosFiltrados$Height)^2)

datosCombinados <- cbind(datosBind,datosFiltrados %>% select(!"Gender"))

# Recomendación de la profesora poner 23 para obtener la cantidad de mujeres necesarias.
datosCombinados$EN <- ifelse(datosCombinados$IMC > 23.0, "sobrepeso","no sobrepeso")
datosCombinados$EN <- factor(datosCombinados$EN)

sobrepeso <- filter(datosCombinados, EN == "sobrepeso")
noSobrepeso <- filter(datosCombinados, EN == "no sobrepeso")

# Se obtienen las muestras de las mujeres con y sin sobrepeso.
muestraSobrepeso <- sample_n(sobrepeso,size=60)
muestraNoSobrepeso <- sample_n(noSobrepeso,size=60)

# Se unen dichas muestras para garantizar cierta cantidad de mujeres en las muestras siguientes (por enunciado).
muestraConjunta <- rbind(muestraSobrepeso,muestraNoSobrepeso)

muestraSobrepeso1 <- sample_n(muestraSobrepeso,size=40)
muestraNoSobrepeso1 <- sample_n(muestraNoSobrepeso,size=40)

muestraSobrepeso2 <- sample_n(muestraSobrepeso,size=20)
muestraNoSobrepeso2 <- sample_n(muestraNoSobrepeso,size=20)

muestraEvaluada <- rbind(muestraSobrepeso1, muestraNoSobrepeso1)
muestraEvaluadora <- rbind(muestraSobrepeso2, muestraNoSobrepeso2)

# 4. Seleccionar, de las otras variables, una que el equipo considere que podría ser útil para predecir la clase EN, 
# justificando bien esta selección.

muestraEvaluada[["EN"]] <- factor(muestraEvaluada[["EN"]])
IMC <- muestraEvaluada[["IMC"]]
datosCor <- muestraEvaluada %>% select(!c("EN","Gender","IMC"))

matriz <- cor(IMC,datosCor)
print(matriz)

# A partir de la función cor, se da que "Weight" es el que tiene el mayor valor de correlación, por lo cual
# es candidato a ser un buen predictor para el estudio del EN.

# 5. Usando el entorno R y paquetes estándares, construir un modelo de regresión logística con el predictor 
# seleccionado en el paso anterior y utilizando de la muestra obtenida.

EN <- muestraEvaluada[["EN"]]
modelo <- glm(EN ~ Weight, family = binomial(link = "logit"), data = muestraEvaluada)
print(summary(modelo))

# 6. Usando herramientas estándares para la exploración de modelos del entorno R, buscar entre dos y cinco 
# predictores de entre las variables seleccionadas al azar, recordadas en el punto 3, para agregar al modelo 
# obtenido en el paso 5.

# En datos combinados están las columnas que necesitamos para obtener los otros predictores
# Ajustar modelo nulo.
nulo <- lm(IMC ~ 1, data = datosCombinados)

# Ajustar modelo completo.
completo <- lm(IMC ~ ., data = datosCombinados)
# print (add1(nulo, scope = completo ) )

# Ajustar modelo con regresión escalonada.
escalonado <- step(nulo , scope = list (lower = nulo , upper = completo) ,
                   direction = "both", trace = 0)

print(summary (escalonado))

# 7. Evaluar la confiabilidad de los modelos (i.e. que tengan un buen nivel de ajuste y son generalizables) y 
# “arreglarlos” en caso de que tengan algún problema.

probs <- predict(escalonado, muestraEvaluadora, type = "response")
ROC <- roc(muestraEvaluadora[["EN"]], probs)
plot(ROC)

# Verificación de multicolinealidad .
cat (" Verificaci ón de colinealidad \n")
cat (" - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - - -- - - - - -\n")
cat ("\ nVIF :\n")
vifs <- vif(escalonado)
print ( vifs )

# Independencia de los residuos .
cat (" Verificación de independencia de los residuos \n")
cat (" - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - - -- - - - - -\n")
print(durbinWatsonTest(escalonado, max.lag = 5) )
# Por el valor p obtenido, no se puede decir verificar la hipótesis nula por lo que no se puede decir que verifican independencia.

cat("\nPrueba de normalidad para los residuos :\ n ")
print(shapiro.test(escalonado$residuals))
# Dado que el p value fue de 9.484e^-12, presentan normalidad.

# Comprobar homocedasticidad de los residuos .
cat("Prueba de homocedasticidad para los residuos :\ n ")
print(ncvTest(escalonado))
# Por el valor p obtenido, cumple homocedasticidad.


# 8. Usando código estándar, evaluar el poder predictivo de los modelos con los datos de las 40 personas que no 
# se incluyeron en su construcción en términos de sensibilidad y especificidad.

# n <- nrow(datosCombinados)
# n_entrenamiento <- floor(0.7 * n)

# muestraPoder <- sample.int(n = n , size = n_entrenamiento, replace = FALSE)

# entrenamiento <- datosCombinados[muestraPoder,]
# prueba <- datosCombinados[-muestraPoder,]

# modeloPoder <- lm(EN ~ Weight, data = muestraEvaluada)
# print(summary(modeloPoder))

# # Calcular error cuadrado promedio para el conjunto de entrenamiento .
# mse_entrenamiento <- mean(modeloPoder$residuals**2)
# cat("MSE para el conjunto de entrenamiento : " , mse_entrenamiento, "\n")
# 
# # Hacer predicciones para el conjunto de prueba .
# predicciones <- predict(modelo, prueba)
# 
# # Calcular error cuadrado promedio para el conjunto de prueba .
# error <- prueba [["Weight"]] - predicciones
# mse_prueba <- mean ( error ** 2)
# cat ("MSE para el conjunto de prueba : ", mse_prueba)





