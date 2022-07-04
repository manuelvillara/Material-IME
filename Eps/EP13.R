library(dplyr)
library(ggpubr)
library(car)

# Un estudio recolectó medidas anatómicas de 247 hombres y 260 mujeres (Heinz et al., 2003). El estudio incluyó 
# nueve mediciones del esqueleto (ocho diámetros y una profundidad de hueso a hueso) y doce mediciones de 
# grosor (circunferencias) que incluyen el tejido.

# Se pide construir un modelo de regresión lineal múltiple para predecir la variable Peso, de acuerdo con las 
# siguientes instrucciones:

# Se leen los datos.
datos <- read.csv2("C:\\Users\\rodri\\OneDrive\\Escritorio\\USACH\\1-2022\\IME\\Lecturas\\EP's\\EP13\\EP13 Datos.csv")

# 1. Definir la semilla a utilizar, que corresponde a los últimos cuatro dígitos del RUN (sin considerar el dígito 
# verificador) del integrante de menor edad del equipo.

set.seed(0828)

# 2. Seleccionar una muestra de 50 mujeres (si la semilla es un número par) o 50 hombres (si la semilla es impar).

datos1 <- filter(datos, Gender == 0)
datos1[["Gender"]] <- NULL
muestraMujeres <- sample_n(datos1,size=50)

# 3. Seleccionar de forma aleatoria ocho posibles variables predictoras.

columnas <- colnames(datos1)
columnasAleatorias <- sample(columnas,8,replace = FALSE)
muestraVarPredictoras <- muestraMujeres %>% select(columnasAleatorias)

# 4. Seleccionar, de las otras variables, una que el equipo considere que podría ser útil para predecir la variable 
# Peso, justificando bien esta selección.

# Para elegir correctamente una variable que pueda servir para predecir la variable peso, se calculará la correlación del peso, y se 
# comparará con las correlaciones de las variables que no están incluidas en la selección anterior.

# Ocupamos la función cor(x,y) para determinar dicha correlación entre peso y las demás variables.

muestrasRestantes <- muestraMujeres %>% select(!columnasAleatorias & !"Weight")
peso <- muestraMujeres %>% select("Weight")

matriz <- cor(peso,muestrasRestantes)

# La función entrega que la variable Chest.Girth tiene un valor de 0.8894733 (el mayor valor) por tanto se elige esta variable como la que
# permite predecir la variable Peso.

# 5. Usando el entorno R, construir un modelo de regresión lineal simple con el predictor seleccionado en el 
# paso anterior.

# Realizamos un modelo mediante mínimos cuadrados, utilizando la muestra de mujeres, con las variables Peso y Ancho de pecho.

modelo <- lm(Weight ~ Chest.Girth, data = muestraMujeres)
print(summary(modelo))

# 6. Usando herramientas para la exploración de modelos del entorno R, buscar entre dos y cinco predictores de 
# entre las variables seleccionadas al azar en el punto 3, para agregar al modelo de regresión lineal simple 
# obtenido en el paso 5.

columnasVarPred <- append(columnasAleatorias,c("Weight","Chest.Girth"))
columnasParaRegresion <- muestraMujeres %>% select(columnasVarPred)

# Ajustar modelo nulo.
nulo <- lm(Weight ~ 1, data = columnasParaRegresion)

# Ajustar modelo completo.
completo <- lm(Weight ~ ., data = columnasParaRegresion)
# print (add1(nulo, scope = completo ) )

# Ajustar modelo con regresi ón escalonada .
escalonado <- step(nulo , scope = list (lower = nulo , upper = completo) ,
                        direction = "both", trace = 0)

cat (" === Modelo con regresión escalonada ===\ n")
print (summary (escalonado))
cat (" AIC =", AIC(escalonado), "\n\n")



# 7. Evaluar los modelos y “arreglarlos” en caso de que tengan algún problema con las condiciones que deben 
# cumplir.

# Comprobar independencia de los residuos .
cat ("Prueba de Durbin - Watson para autocorrelaciones")
cat ("entre errores :\ n ")
print (durbinWatsonTest(escalonado))

# Comprobar normalidad de los residuos .
cat("\nPrueba de normalidad para los residuos :\ n ")
print(shapiro.test(escalonado$residuals))
# Comprobar homocedasticidad de los residuos .
cat("Prueba de homocedasticidad para los residuos :\ n ")
print(ncvTest(escalonado))
# Comprobar la multicolinealidad .
vifs <- vif(escalonado)
cat (" \nVerificar la multicolinealidad:\n")
cat ("-VIFs:\n ")
print (vifs)
cat ("-Tolerancias:\n")
print (1 / vifs )
cat ("-VIF medio:" , mean(vifs) , "\n")

# 8. 

n <- nrow(muestraVarPredictoras)
n_entrenamiento <- floor(0.7 * n)

muestra <- sample.int(n = n , size = n_entrenamiento, replace = FALSE)

entrenamiento <- columnasParaRegresion[muestra,]
prueba <- columnasParaRegresion[-muestra,]

modelo <- lm(Weight ~ Chest.Girth, data = entrenamiento)
print(summary(modelo))

# Calcular error cuadrado promedio para el conjunto de entrenamiento .
mse_entrenamiento <- mean(modelo$residuals**2)
cat("MSE para el conjunto de entrenamiento : " , mse_entrenamiento, "\n")

# Hacer predicciones para el conjunto de prueba .
predicciones <- predict(modelo, prueba)

# Calcular error cuadrado promedio para el conjunto de prueba .
error <- prueba [["Weight"]] - predicciones
mse_prueba <- mean ( error ** 2)
cat ("MSE para el conjunto de prueba : ", mse_prueba)