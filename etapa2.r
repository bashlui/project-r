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

## Graficar la distribución de densidad empírica de cada variable numérica elegida por el equipo (acceso_electrd, acceso_combust_limpios, electrd_fosiles, electrd_de_f_bajas_carb)
```{r}
#REGIÓN 1: ÁFRICA DEL NORTE

x1_1 = R1$acceso_electrd  # Variable: acceso_electrd
m1_1 = mean(x1_1)  # Promedio
ds1_1 = sd(x1_1)  # Desviación estandar             

x2_1 = R1$acceso_combust_limpios  # Variable: acceso_combust_limpios
m2_1 = mean(x2_1)  # Promedio
ds2_1 = sd(x2_1)  # Desviación estandar             

x3_1 = R1$electrd_fosiles  # Variable: electrd_fosiles
m3_1 = mean(x3_1)  # Promedio
ds3_1 = sd(x3_1)  # Desviación estandar             

x4_1 = R1$electrd_de_f_bajas_carb  # Variable: electrd_de_f_bajas_carb
m4_1 = mean(x4_1)  # Promedio
ds4_1 = sd(x4_1)  # Desviación estandar      

x5_1 = R1$PIB_per_cap  # Variable: PIB_per_CAP
m5_1 = mean(x5_1)  # Promedio
ds5_1 = sd(x5_1)  # Desviación estandar

#REGIÓN 2: ASIA OCCIDENTAL

x1_2 = R2$acceso_electrd  # Variable: acceso_electrd
m1_2 = mean(x1_2)  # Promedio
ds1_2 = sd(x1_2)  # Desviación estandar

x2_2 = R2$acceso_combust_limpios  # Variable: acceso_combust_limpios
m2_2 = mean(x2_2)  # Promedio
ds2_2 = sd(x2_2)  # Desviación estandar             

x3_2 = R2$electrd_fosiles  # Variable: electrd_fosiles
m3_2 = mean(x3_2)  # Promedio
ds3_2 = sd(x3_2)  # Desviación estandar             

x4_2 = R2$electrd_de_f_bajas_carb  # Variable: electrd_de_f_bajas_carb
m4_2 = mean(x4_2)  # Promedio
ds4_2 = sd(x4_2)  # Desviación estandar

x5_2 = R2$PIB_per_cap  # Variable: PIB_per_CAP
m5_2 = mean(x5_2)  # Promedio
ds5_2 = sd(x5_2)  # Desviación estandar
```


# Tabla de las principales medidas de centralización

### REGIÓN 1: ÁFRICA DEL NORTE
```{r}
media_R1 = apply(R1[-1], 2, mean)  # Calcular de la media para las variables (el 2 indica que el cálculo será por columna) 
mediana_R1 = apply(R1[-1], 2, median)  # Calcular de la mediana para las variables (el 2 indica que el cálculo será por columna) 
Rm = function(x)(max(x)+min(x))/2  # Función que calcula el rango medio 
Rm_R1 = apply(R1[-1], 2, Rm)  # Calcular del rango medio para las variables
Centro_R1 = data.frame(media_R1, mediana_R1, Rm_R1)  # Elaborar la tabla de todas las variables
```

### REGIÓN 2: ASIA OCCIDENTAL
```{r}
media_R2 = apply(R2[-1], 2, mean)  # Calcular de la media para las variables (el 2 indica que el cálculo será por columna) 
mediana_R2 = apply(R2[-1], 2, median)  # Calcular de la mediana para las variables (el 2 indica que el cálculo será por columna) 
Rm = function(x)(max(x)+min(x))/2  # Función que calcula el rango medio 
Rm_R2 = apply(R2[-1], 2, Rm)  # Calcular del rango medio para las variables
Centro_R2 = data.frame(media_R2, mediana_R2, Rm_R2)  # Elaborar la tabla de todas las variables
```

## Graficar la función de densidad empírica
### REGIÓN 1: ÁFRICA DEL NORTE
```{r}
par(mfrow=c(3,2))

# VARIABLE: acceso_electrd
# Graficar la función de densidad empírica
hist(x1_1, freq=FALSE, col="cornsilk4", sub="Líneas= Rojo:Normal - Azul:fdp empírica", main="Región 1: África del Norte", xlab="% de población con acceso a la electr.", ylab="Frecuencia", ) # Histograma de densidad empírica con área bajo la curva 1.
lines(density(x1_1), col="blue", lwd=2) # "density" genera la curva de densidad empírica (línea ajustada)

#Gráfica de la distribución normal si los datos se distribuyeran como una normal:
x = seq(m1_1-4*ds1_1, m1_1+4*ds1_1, 0.1) # Crear secuencia de datos que estén 4 desviaciones estándar alrededor de la media y que vayan de 0.1 en 0.1
curve(dnorm(x, m1_1, ds1_1), add=TRUE, col="red", lwd=2) # Graficar la línea de densidad normal con media m1_1 y desviación estándar ds1_1


# VARIABLE: acceso_combust_limpios
# Graficar la función de densidad empírica
hist(x2_1, freq=FALSE, col="bisque2",  sub="Líneas= Rojo:Normal - Azul:fdp empírica", main="Región 1: África del Norte", xlab="% de población que depende\nprincipalmente de combustibles limpios", ylab="Frecuencia") # Histograma de densidad empírica con área bajo la curva 1.
lines(density(x2_1), col="blue", lwd=2) # "density" genera la curva de densidad empírica (línea ajustada)

#Gráfica de la distribución normal si los datos se distribuyeran como una normal:
x = seq(m2_1-4*ds2_1, m2_1+4*ds2_1, 0.1) # Crear secuencia de datos que estén 4 desviaciones estándar alrededor de la media y que vayan de 0.1 en 0.1
curve(dnorm(x, m2_1, ds2_1), add=TRUE, col="red", lwd=2) # Graficar la línea de densidad normal con media m1_1 y desviación estándar ds1_1


# VARIABLE: electrd_fosiles
# Graficar la función de densidad empírica
hist(x3_1, freq=FALSE, col="darkgoldenrod2",  sub="Líneas= Rojo:Normal - Azul:fdp empírica", main="Región 1: África del Norte", xlab="Electr. generada por combustibles fósiles (TWh).", ylab="Frecuencia") # Histograma de densidad empírica con área bajo la curva 1.
lines(density(x3_1), col="blue", lwd=2) # "density" genera la curva de densidad empírica (línea ajustada)

#Gráfica de la distribución normal si los datos se distribuyeran como una normal:
x = seq(m3_1-4*ds3_1, m3_1*ds3_1, 0.1) # Crear secuencia de datos que estén 4 desviaciones estándar alrededor de la media y que vayan de 0.1 en 0.1
curve(dnorm(x, m3_1, ds3_1), add=TRUE, col="red", lwd=2) # Graficar la línea de densidad normal con media m1_1 y desviación estándar ds1_1


# VARIABLE: electrd_de_f_bajas_carb
# Graficar la función de densidad empírica
hist(x4_1, freq=FALSE, col="darkolivegreen3",  sub="Líneas= Rojo:Normal - Azul:fdp empírica", main="Región 1: África del Norte", xlab="% de electricidad procedente\nde fuentes bajas en carbono", ylab="Frecuencia") # Histograma de densidad empírica con área bajo la curva 1.
lines(density(x4_1), col="blue", lwd=2) # "density" genera la curva de densidad empírica (línea ajustada)

#Gráfica de la distribución normal si los datos se distribuyeran como una normal:
x = seq(m4_1-4*ds4_1, m4_1*ds4_1, 0.1) # Crear secuencia de datos que estén 4 desviaciones estándar alrededor de la media y que vayan de 0.1 en 0.1
curve(dnorm(x, m4_1, ds4_1), add=TRUE, col="red", lwd=2) # Graficar la línea de densidad normal con media m1_1 y desviación estándar ds1_1

# VARIABLE: PIB_per_cap
# Graficar la función de densidad empírica  
hist(x5_1, freq=FALSE, col="darksalmon",  sub="Líneas= Rojo:Normal - Azul:fdp empírica", main="Región 1: África del Norte", xlab="PIB per cápita (USD)", ylab="Frecuencia") # Histograma de densidad empírica con área bajo la curva 1.
lines(density(x5_1), col="blue", lwd=2) # "density" genera la curva de densidad empírica (línea ajustada)

# Gráfica de la distribución normal si los datos se distribuyeran como una normal:
x = seq(m5_1-4*ds5_1, m5_1*ds5_1, 0.1) # Crear secuencia de datos que estén 4 desviaciones estándar alrededor de la media y que vayan de 0.1 en 0.1
curve(dnorm(x, m5_1, ds5_1), add=TRUE, col="red", lwd=2) # Graficar la línea de densidad normal con media m1_1 y desviación estándar ds1_1
```

### REGIÓN 2: ASIA OCCIDENTAL
```{r}
par(mfrow=c(3,2))

# VARIABLE: acceso_electrd
# Graficar la función de densidad empírica
hist(x1_2, freq=FALSE, col="cornsilk4", sub="Líneas= Rojo:Normal - Azul:fdp empírica", main="Región 2: Asia Occidental", xlab="% de población con acceso a la electr.", ylab="Frecuencia", ) # Histograma de densidad empírica con área bajo la curva 1.
lines(density(x1_2), col="blue", lwd=2) # "density" genera la curva de densidad empírica (línea ajustada)

# Gráfica de la distribución normal si los datos se distribuyeran como una normal:
x = seq(m1_2-4*ds1_2, m1_2+4*ds1_2, 0.1) # Crear secuencia de datos que estén 4 desviaciones estándar alrededor de la media y que vayan de 0.1 en 0.1
curve(dnorm(x, m1_2, ds1_2), add=TRUE, col="red", lwd=2) # Graficar la línea de densidad normal con media m1_2 y desviación estándar ds1_2

# VARIABLE: acceso_combust_limpios
# Graficar la función de densidad empírica
hist(x2_2, freq=FALSE, col="bisque2",  sub="Líneas= Rojo:Normal - Azul:fdp empírica", main="Región 2: Asia Occidental", xlab="% de población que depende\nprincipalmente de combustibles limpios", ylab="Frecuencia") # Histograma de densidad empírica con área bajo la curva 1.
lines(density(x2_2), col="blue", lwd=2) # "density" genera la curva de densidad empírica (línea ajustada)

# Gráfica de la distribución normal si los datos se distribuyeran como una normal:
x = seq(m2_2-4*ds2_2, m2_2+4*ds2_2, 0.1) # Crear secuencia de datos que estén 4 desviaciones estándar alrededor de la media y que vayan de 0.1 en 0.1
curve(dnorm(x, m2_2, ds2_2), add=TRUE, col="red", lwd=2) # Graficar la línea de densidad normal con media m1_2 y desviación estándar ds1_2

# VARIABLE: electrd_fosiles
# Graficar la función de densidad empírica
hist(x3_2, freq=FALSE, col="darkgoldenrod2",  sub="Líneas= Rojo:Normal - Azul:fdp empírica", main="Región 2: Asia Occidental", xlab="Electr. generada por combustibles fósiles (TWh).", ylab="Frecuencia") # Histograma de densidad empírica con área bajo la curva 1.
lines(density(x3_2), col="blue", lwd=2) # "density" genera la curva de densidad empírica (línea ajustada)

# Gráfica de la distribución normal si los datos se distribuyeran como una normal:
x = seq(m3_2-4*ds3_2, m3_2*ds3_2, 0.1) # Crear secuencia de datos que estén 4 desviaciones estándar alrededor de la media y que vayan de 0.1 en 0.1
curve(dnorm(x, m3_2, ds3_2), add=TRUE, col="red", lwd=2) # Graficar la línea de densidad normal con media m1_2 y desviación estándar ds1_2

# VARIABLE: electrd_de_f_bajas_carb
# Graficar la función de densidad empírica
hist(x4_2, freq=FALSE, col="darkolivegreen3",  sub="Líneas= Rojo:Normal - Azul:fdp empírica", main="Región 2: Asia Occidental", xlab="% de electricidad procedente\nde fuentes bajas en carbono", ylab="Frecuencia") # Histograma de densidad empírica con área bajo la curva 1.
lines(density(x4_2), col="blue", lwd=2) # "density" genera la curva de densidad empírica (línea ajustada)

# Gráfica de la distribución normal si los datos se distribuyeran como una normal:
x = seq(m4_2-4*ds4_2, m4_2*ds4_2, 0.1) # Crear secuencia de datos que estén 4 desviaciones estándar alrededor de la media y que vayan de 0.1 en 0.1
curve(dnorm(x, m4_2, ds4_2), add=TRUE, col="red", lwd=2) # Graficar la línea de densidad normal con media m1_2 y desviación estándar ds1_2

# VARIABLE: PIB_per_cap
# Graficar la función de densidad empírica
hist(x5_2, freq=FALSE, col="darksalmon",  sub="Líneas= Rojo:Normal - Azul:fdp empírica", main="Región 2: Asia Occidental", xlab="PIB per cápita (USD)", ylab="Frecuencia") # Histograma de densidad empírica con área bajo la curva 1.
lines(density(x5_2), col="blue", lwd=2) # "density" genera la curva de densidad empírica (línea ajustada)

# Gráfica de la distribución normal si los datos se distribuyeran como una normal:
x = seq(m5_2-4*ds5_2, m5_2*ds5_2, by = 1) # Crear secuencia de datos que estén 4 desviaciones estándar alrededor de la media y que vayan de 0.1 en 0.1
curve(dnorm(x, m5_2, ds5_2), add=TRUE, col="red", lwd=2) # Graficar la línea de densidad normal con media m1_2 y desviación estándar ds1_2
```

## Graficar el QQplot por variable
### REGIÓN 1: ÁFRICA DEL NORTE
```{r}
par(mfrow=c(3,2))

qqnorm(x1_1, col="deepskyblue2", pch=20, main="Región 1: África del Norte", sub="% de población con acceso a la electr.")
qqline(x1_1, col="deeppink3")

qqnorm(x2_1, col="deepskyblue2", pch=20, main="Región 1: África del Norte", sub="% de población que depende principalmente de combustibles limpios")
qqline(x2_1, col="deeppink3")

qqnorm(x3_1, col="deepskyblue2", pch=20, main="Región 1: África del Norte", sub="Electr. generada por combustibles fósiles (TWh).")
qqline(x3_1, col="deeppink3")

qqnorm(x4_1, col="deepskyblue2", pch=20, main="Región 1: África del Norte", sub="% de electricidad procedente de fuentes bajas en carbono")
qqline(x4_1, col="deeppink3")

qqnorm(x5_1, col="deepskyblue2", pch=20, main="Región 1: África del Norte", sub="PIB per Cápita")
qqline(x5_1, col="deeppink3")
```


### REGIÓN 2: ÁSIA OCCIDENTAL
```{r}
par(mfrow=c(3,2))

qqnorm(x1_2, col="blue", pch=20, main="Región 2: Asia Occidental", sub="% de población con acceso a la electr.")
qqline(x1_2, col="red")

qqnorm(x2_2, col="blue", pch=20, main="Región 2: Asia Occidental", sub="% de población que depende principalmente de combustibles limpios")
qqline(x2_2, col="red")

qqnorm(x3_2, col="blue", pch=20, main="Región 2: Asia Occidental", sub="Electr. generada por combustibles fósiles (TWh).")
qqline(x3_2, col="red")

qqnorm(x4_2, col="blue", pch=20, main="Región 2: Asia Occidental", sub="% de electricidad procedente de fuentes bajas en carbono")
qqline(x4_2, col="red")

qqnorm(x5_2, col="blue", pch=20, main="Región 2: Asia Occidental", sub="PIB per cápita")
qqline(x5_2, col="red")
```
