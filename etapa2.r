---
title: "Etapa 2"
author: "Luis Antonio Bolaina Dominguez"
date: "2024-03-08"
output: word_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
## Leer la base de datos
```{r}
M3 = read.csv("datosEq7.csv")
head(M3, n=10)
```

## Dividir la base de datos por región
```{r}
R1 = subset(M3, M3$region == "R1")[-1]
R2 = subset(M3, M3$region == "R2")[-1]
str(R1)
```

```{r}
x1_1 = R1$electrd_fosiles
m1 = mean(x1_1)
ds1 = sd(x1_1)
m1
ds1
```

# Gráficas de distribución de densidad empírica y la distribución normal teórica
## Región 1
```{r}
# acceso_electrd, acceso_combust_limpios, electrd_fosiles, electrd_de_f_bajas_carb
par(mfrow=c(2,2))
hist(R1$acceso_electrd, freq=FALSE, col="pink3", main="Región África del Norte", xlab="Acceso a electricidad (%)", ylab="Frecuencia") # Histograma de densidad empírica con área bajo la curva 1.
lines(density(R1$acceso_electrd), col="pink4", lwd=2) # density genera la curva de densidad empírica (línea ajustada)
x = seq(mean(R1$acceso_electrd)-4*sd(R1$acceso_electrd), mean(R1$acceso_electrd)+4*sd(R1$acceso_electrd), 0.1) # crea una secuencia de datos que estén 4 desviaciones estándar alrededor de la media y que vayan de 0.1 en 0.1
curve(dnorm(x, mean(R1$acceso_electrd), sd(R1$acceso_electrd)), add=TRUE, col="red", lwd=2) #grafica la línea de densidad normal con media m1 y desviación estándar ds1
hist(R1$acceso_combust_limpios, freq=FALSE, col="pink3", main="Región África del Norte", xlab="Acceso a combustibles limpios (%)", ylab="Frecuencia") # Histograma de densidad empírica con área bajo la curva 1.
lines(density(R1$acceso_combust_limpios), col="pink4", lwd=2) # density genera la curva de densidad empírica (línea ajustada)
x = seq(mean(R1$acceso_combust_limpios)-4*sd(R1$acceso_combust_limpios), mean(R1$acceso_combust_limpios)+4*sd(R1$acceso_combust_limpios), 0.1) # crea una secuencia de datos que estén 4 desviaciones estándar alrededor de la media y que vayan de 0.1 en 0.1
curve(dnorm(x, mean(R1$acceso_combust_limpios), sd(R1$acceso_combust_limpios)), add=TRUE, col="red", lwd=2) #grafica la línea de densidad normal con media m1 y desviación estándar ds1
hist(R1$electrd_de_f_bajas_carb, freq=FALSE, col="pink3", main="Región África del Norte", xlab="Electricidad de fuentes bajas en carbono(kWh)", ylab="Frecuencia") # Histograma de densidad empírica con área bajo la curva 1.
lines(density(R1$electrd_de_f_bajas_carb), col="pink4", lwd=2) # density genera la curva de densidad empírica (línea ajustada)
x = seq(mean(R1$electrd_de_f_bajas_carb)-4*sd(R1$electrd_de_f_bajas_carb), mean(R1$electrd_de_f_bajas_carb)+4*sd(R1$electrd_de_f_bajas_carb), 0.1) # crea una secuencia de datos que estén 4 desviaciones estándar alrededor de la media y que vayan de 0.1 en 0.1
curve(dnorm(x, mean(R1$electrd_de_f_bajas_carb), sd(R1$electrd_de_f_bajas_carb)), add=TRUE, col="red", lwd=2) #grafica la línea de densidad normal con media m1 y desviación estándar ds1
hist(R1$electrd_fosiles, freq=FALSE, col="pink3", main="Región África del Norte", xlab="Electricidad de origen fósil(kWh)", ylab="Frecuencia") # Histograma de densidad empírica con área bajo la curva 1.
lines(density(R1$electrd_fosiles), col="pink4", lwd=2) # density genera la curva de densidad empírica (línea ajustada)
x = seq(mean(R1$electrd_fosiles)-4*sd(R1$electrd_fosiles), mean(R1$electrd_fosiles)+4*sd(R1$electrd_fosiles), 0.1) # crea una secuencia de datos que estén 4 desviaciones estándar alrededor de la media y que vayan de 0.1 en 0.1
curve(dnorm(x, mean(R1$electrd_fosiles), sd(R1$electrd_fosiles)), add=TRUE, col="red", lwd=2) #grafica la línea de densidad normal con media m1 y desviación estándar ds1
```

## Región 2
```{r}
par(mfrow=c(2,2))
hist(R2$acceso_electrd, freq=FALSE, col="pink3", main="Región Asia Occidental", xlab="Acceso a electricidad (%)", ylab="Frecuencia") # Histograma de densidad empírica con área bajo la curva 1.
lines(density(R2$acceso_electrd), col="pink4", lwd=2) # density genera la curva de densidad empírica (línea ajustada)
x = seq(mean(R2$acceso_electrd)-4*sd(R2$acceso_electrd), mean(R2$acceso_electrd)+4*sd(R2$acceso_electrd), 0.1) # crea una secuencia de datos que estén 4 desviaciones estándar alrededor de la media y que vayan de 0.1 en 0.1
curve(dnorm(x, mean(R2$acceso_electrd), sd(R2$acceso_electrd)), add=TRUE, col="red", lwd=2) #grafica la línea de densidad normal con media m1 y desviación estándar ds1
hist(R2$acceso_combust_limpios, freq=FALSE, col="pink3", main="Región Asia Occidental", xlab="Acceso a combustibles limpios (%)", ylab="Frecuencia") # Histograma de densidad empírica con área bajo la curva 1.
lines(density(R2$acceso_combust_limpios), col="pink4", lwd=2) # density genera la curva de densidad empírica (línea ajustada)
x = seq(mean(R2$acceso_combust_limpios)-4*sd(R2$acceso_combust_limpios), mean(R2$acceso_combust_limpios)+4*sd(R2$acceso_combust_limpios), 0.1) # crea una secuencia de datos que estén 4 desviaciones estándar alrededor de la media y que vayan de 0.1 en 0.1
curve(dnorm(x, mean(R2$acceso_combust_limpios), sd(R2$acceso_combust_limpios)), add=TRUE, col="red", lwd=2) #grafica la línea de densidad normal con media m1 y desviación estándar ds1
hist(R2$electrd_de_f_bajas_carb, freq=FALSE, col="pink3", main="Región Asia Occidental", xlab="Electricidad de fuentes bajas en carbono(kWh)", ylab="Frecuencia") # Histograma de densidad empírica con área bajo la curva 1.
lines(density(R2$electrd_de_f_bajas_carb), col="pink4", lwd=2) # density genera la curva de densidad empírica (línea ajustada)
x = seq(mean(R2$electrd_de_f_bajas_carb)-4*sd(R2$electrd_de_f_bajas_carb), mean(R2$electrd_de_f_bajas_carb)+4*sd(R2$electrd_de_f_bajas_carb), 0.1) # crea una secuencia de datos que estén 4 desviaciones estándar alrededor de la media y que vayan de 0.1 en 0.1
curve(dnorm(x, mean(R2$electrd_de_f_bajas_carb), sd(R2$electrd_de_f_bajas_carb)), add=TRUE, col="red", lwd=2) #grafica la línea de densidad normal con media m1 y desviación estándar ds1
hist(R2$electrd_fosiles, freq=FALSE, col="pink3", main="Región Asia Occidental", xlab="Electricidad de origen fósil(kWh)", ylab="Frecuencia") # Histograma de densidad empírica con área bajo la curva 1.
lines(density(R2$electrd_fosiles), col="pink4", lwd=2) # density genera la curva de densidad empírica (línea ajustada)
x = seq(mean(R2$electrd_fosiles)-4*sd(R2$electrd_fosiles), mean(R2$electrd_fosiles)+4*sd(R2$electrd_fosiles), 0.1) # crea una secuencia de datos que estén 4 desviaciones estándar alrededor de la media y que vayan de 0.1 en 0.1
curve(dnorm(x, mean(R2$electrd_fosiles), sd(R2$electrd_fosiles)), add=TRUE, col="red", lwd=2) #grafica la línea de densidad normal con media m1 y desviación estándar ds1
```

# Tabla de las principales medidas de centralización
## Región 1
```{r}
media_R1 = apply(R1[-1], 2, mean)
mediana_R1 = apply(R1[-1], 2, median)
Rm = function(x)(max(x)+min(x))/2
Rm_R1 = apply(R1[-1], 2, Rm)
Centro_R1 = data.frame(media_R1, mediana_R1, Rm_R1)
```

## Región 2
```{r}
media_R2 = apply(R2[-1], 2, mean)
mediana_R2 = apply(R2[-1], 2, median)
Rm_R2 = apply(R2[-1], 2, Rm)
Centro_R2 = data.frame(media_R2, mediana_R2, Rm_R2)
```


# Graficar el QQplot por variable
## Es necesario hacer qqplot por variable (como son 5 por región y hay dos regiones, se deben hacer 10 qqplot)
## Región 1
```{r}
par(mfrow=c(2,2))
qqnorm(R1$acceso_electrd, main="QQplot acceso a electricidad")
qqline(R1$acceso_electrd)
qqnorm(R1$acceso_combust_limpios, main="QQplot acceso a combustibles limpios")
qqline(R1$acceso_combust_limpios)
qqnorm(R1$electrd_de_f_bajas_carb, main="QQplot electricidad de fuentes bajas en carbono")
qqline(R1$electrd_de_f_bajas_carb)
qqnorm(R1$electrd_fosiles, main="QQplot electricidad de origen fósil")
qqline(R1$electrd_fosiles)
```

## Región 2
```{r}
par(mfrow=c(2,2))
qqnorm(R2$acceso_electrd, main="QQplot acceso a electricidad")
qqline(R2$acceso_electrd)
qqnorm(R2$acceso_combust_limpios, main="QQplot acceso a combustibles limpios")
qqline(R2$acceso_combust_limpios)
qqnorm(R2$electrd_de_f_bajas_carb, main="QQplot electricidad de fuentes bajas en carbono")
qqline(R2$electrd_de_f_bajas_carb)
qqnorm(R2$electrd_fosiles, main="QQplot electricidad de origen fósil")
qqline(R2$electrd_fosiles)
```
