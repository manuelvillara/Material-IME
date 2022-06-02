library(dplyr)
library(ggpubr)


#1. Definan su propia semilla y obtengan 5.000 casos para una distribución de ingresos aproximadamente normal.

poblacion <- read.csv2("EP03 Datos Casen 2017.csv")

tamano <- nrow(poblacion)
ingreso <- as.numeric(poblacion[["ytot"]])
poda <- 0.2
q20 <- quantile(ingreso, poda)
q80 <- quantile(ingreso, 1 - poda)
ingreso.podado <- ingreso[ingreso > q20 & ingreso < q80]
tamano.podado <- length(ingreso.podado)
media.ingreso <- mean(ingreso.podado)
sd.ingreso <- sqrt (sum((ingreso.podado - media.ingreso)^2) / tamano.podado )
set.seed(69420)
ingreso.normal <- rnorm(5000, mean = media.ingreso, sd = sd.ingreso)

normalizados <- data.frame(ingreso.normal)

g <- ggplot(data = normalizados) + 
  aes(x = ingreso.normal, y = ..density..) +
  geom_histogram() +
  geom_density()
print(g)

#2. A partir de la distribución conseguida, y sin usar nuevamente la función rnorm(), generen la 
#correspondiente distribución Z.

#Distribución Z
z <- (ingreso.normal - media.ingreso) / sd.ingreso
plot(density(z), main = "Distribucion Z")


#3. Con la distribución Z obtenida en el punto anterior, y sin utilizar funciones como rchisq(), construyan dos 
#distribuciones χ2, cada una con más de 3 y menos de 15 grados de libertad.
# Se elijen distribuidos de 1:300 para la mejor visualización del gráfico.

#Distribución chi^2
chi1 <- 1:300
for(i in chi1){
  zsample <- sample(z,4,replace=TRUE)
  chi1[i] <- sum(zsample^2)
}
plot(density(chi1), main = "Distribucion chi1")

#Distribución chi^2
chi2 <- 1:300
for(i in chi2){
  zsample <- sample(z,13,replace=TRUE)
  chi2[i] <- sum(zsample^2)
}
plot(density(chi2), main = "Distribucion chi2")

#4. Usando las dos distribuciones χ2 generadas en el punto anterior, construyan una distribución F.

distribucionF <- (chi1/4)/(chi2/13)
plot(density(distribucionF), main = "Distribucion F")


#Actividad parte 2

#1. Definan su propia semilla y número de repeticiones para el ensayo.

set.seed(24)
repeticiones <- 9

ensayo <- function(x)
  ifelse(sample(poblacion[["sexo"]], 1) == "Mujer", 1, 0)
repeticionesT <- sapply(1:repeticiones, ensayo)

#2. Generen, sin utilizar funciones como rbinom(), una distribución binomial.

casosexitosos <- sum(repeticionesT)
probexito <- casosexitosos/repeticiones
probfracaso <- 1 - probexito

n.k <- (factorial(repeticiones) / (factorial(repeticionesT) * factorial(repeticiones - repeticionesT)))
distBinomial <- n.k * probexito^repeticionesT * probfracaso^(repeticiones-repeticionesT)
plot(density(distBinomial))

#3. Similarmente, construyan una distribución geométrica.

geometrica <- probexito*(1-probexito)^(repeticionesT-1)
plot(density(geometrica))

#4. Análogamente, generen una distribución binomial negativa.

n.kmenosuno <- (factorial(repeticiones-1) / (factorial(repeticionesT-1) * factorial(repeticiones - repeticionesT - 2)))
binomialnegativa <- na.omit(n.kmenosuno * probexito^repeticionesT * probfracaso^(repeticiones-repeticionesT))

plot(density(binomialnegativa))