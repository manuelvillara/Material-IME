# Se importan los paquetes necesarios
if (!require(dplyr)){
  install.packages("dplyr", dependencies = TRUE )
  require (dplyr)
}

if (!require(ggpubr)){
  install.packages("ggpubr", dependencies = TRUE )
  require (ggpubr)
}

# Se lee el archivo de entrada
datos <- read.csv(file.choose(), stringsAsFactors = FALSE, encoding = "UTF-8")

# 1. Definan su propia semilla y obtengan 5.000 casos para una distribución de 
#    ingresos aproximadamente normal.

tamaño <- nrow(datos)
ingreso <- as.numeric(datos[["ytot"]])
poda <- 0.2
q20 <- quantile(ingreso, poda)
q80 <- quantile(ingreso, 1 - poda)
ingreso.podado <- ingreso[ingreso > q20 & ingreso < q80]
tamaño.podado <- length(ingreso.podado)
media.ingreso <- mean(ingreso.podado)
sd.ingreso <- sqrt(sum((ingreso.podado - media.ingreso)^2) / tamaño.podado)
set.seed(20)
ingreso.normal <- rnorm(5000, mean = media.ingreso, sd = sd.ingreso)

# Grafica para pregunta 1
datos_normal <- data.frame(ingreso.normal)

# Grafico histograma con densidad 
g0 <- ggplot(data = datos_normal) + 
  aes(x = ingreso.normal, y = ..density..) +
  geom_histogram() +
  geom_density()
print(g0)

# Histograma
g1 <- gghistogram(datos_normal[["ingreso.normal"]],
                  bins = 10,
                  add = "mean",
                  xlab = "ingresos totales",
                  ylab = "Frecuencia",
                  color = "blue",
                  fill = "blue")
print(g1)


# 2. A partir de la distribución conseguida, y sin usar nuevamente la función 
#    rnorm(), generen la correspondiente distribución Z.
distribucion_Z <- (ingreso.normal - media.ingreso) / sd.ingreso

# Grafica densidad
plot(density(distribucion_Z), main = "Distribucion Z")

# 3. Con la distribución Z obtenida en el punto anterior, y sin utilizar funciones
#    como rchisq(), construyan dos distribuciones ??2, cada una con más de 3 y menos
#    de 15 grados de libertad.

grados1 <- 7
chi.cuadrado1 <- c(1:5000)
for (i in 1:5000) {
  chi.cuadrado1[i] <- distribucion_Z[i]^2 + sum((distribucion_Z[1:grados1]^2), na.rm = TRUE)
}
plot(density(chi.cuadrado1), main = "Distribucion chi-cuadrado con 7 grados de libertad")


grados2 <- 14
chi.cuadrado2 <- c(1:5000)
for (i in 1:5000) {
  chi.cuadrado2[i] <- distribucion_Z[i]^2 + sum((distribucion_Z[1:grados2]^2), na.rm = TRUE)
}
plot(density(chi.cuadrado2), main = "Distribucion chi-cuadrado con 14 grados de libertad")


# 4. Usando las dos distribuciones ??2 generadas en el punto anterior, construyan
#    una distribución F.
distribucion_F <- (((chi.cuadrado1) / grados1) / ((chi.cuadrado2) / grados2))
plot(density(distribucion_F), main = "Distribucion F")


#############################################################################################
# Parte 2

# 1. Definan su propia semilla y número de repeticiones para el ensayo.
set.seed(20) 
n.repeticiones <- 15

ensayo <- function(x)
  ifelse(sample(datos[["sexo"]], 1) == "Mujer", 1, 0)
veinte.repeticiones <- sapply(1:n.repeticiones, ensayo)


# 2. Generen, sin utilizar funciones como rbinom(), una distribución binomial.
exitos <- sum(veinte.repeticiones)
prob_Exito <- exitos / n.repeticiones
prob_Fracaso <- 1 - prob_Exito 

distribucion_B <- 1:n.repeticiones
for (i in 1:n.repeticiones) {
  combinatoria <- factorial(n.repeticiones) / (factorial(i) * factorial(n.repeticiones - i))
  distribucion_B[i] <- (combinatoria*(prob_Exito)^i)*((prob_Fracaso)^(n.repeticiones - i))
}
names(distribucion_B) <- 1:n.repeticiones
barplot(distribucion_B, main = "Distribucion binomial")


# 3. Similarmente, construyan una distribución geométrica.

distribucion_G <- 1:n.repeticiones
for (i in 1:n.repeticiones) {
  distribucion_G[i] <- ((prob_Fracaso)^(i-1))*prob_Exito
}
names(distribucion_G) <- 1:n.repeticiones
barplot(distribucion_G, main = "Distribucion geometrica")


# 4. Análogamente, generen una distribución binomial negativa.

distribucion_BN <- 1:n.repeticiones
for (i in 1:n.repeticiones) {
  combinatoriaN <- factorial(n.repeticiones - 1) / (factorial(i-1) * factorial(n.repeticiones - i))
  distribucion_BN[i] <- (combinatoriaN*(prob_Exito)^i)*((prob_Fracaso)^(1 - i))
}
names(distribucion_BN) <- 1:n.repeticiones
barplot(distribucion_BN, main = "Distribucion binomial negativa")









