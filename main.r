---
title: 'Etapa 1: Análisis Exploratorio de lo datos'
author: "Luis Antonio Bolaina Dominguez"
date: "2024-02-19"
output: word_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE)
```

# Introducción

En la actualidad, la dependencia global de fuentes de energía, ya sean eléctricas o renovables, es un factor fundamental en la configuración de la dinámica socioeconómica y ambiental a nivel mundial. De acuerdo con la Organización Mundial de la Salud (OMS), cerca del 99 % de las personas del planeta respiran un aire que no llega a los límites de calidad adecuados y esto pone en peligro su salud. Asimismo, cada año se registran más de 13 millones de muertes en todo el mundo provocadas por entornos medioambientales evitables, como la contaminación del aire. El cambio a fuentes de energías limpias, como la solar o la eólica, ayuda indudablemente, no solo a luchar contra el cambio climático, sino también, a evitar la contaminación del aire en favor de la salud [2]. En este contexto, la energía renovable se destaca por su contribución significativa, aprovechando recursos prácticamente inagotables debido a su inmensa cantidad o a su capacidad de regeneración natural [1]. Esta realidad subraya la importancia de comprender y analizar el uso de la energía a lo largo del tiempo. En el marco de esta situación, la presente investigación se enfoca en un análisis detallado de las variables pertinentes, centrándose en el período comprendido entre 2000 y 2020. Este análisis tiene como objetivo primordial profundizar en la naturaleza de estas variables y detectar posibles tendencias y patrones que hayan surgido durante estas dos décadas cruciales. Además, se buscará evaluar la variabilidad existente entre los países asignados a cada equipo, reconociendo las diversidades en términos de desarrollo energético y sostenibilidad.

## Leer la base de datos
En este paso, leemos la base de datos y le asignamos una variable M.

```{r}
M = read.csv("datosG5Eq07.csv")
str(M[-1])
```

## Checar los estadísticos y los datos faltantes

```{r}
names(M)
summary(M[-1])
str(sum(is.na(M)))
```

## 3.- Creación de una base de datos de trabajo del equipo.
a) Crea una columna de la región a la que pertenecen los paises de tu base de datos. Elige un nombre sencillo y corto para cada zona
```{r}
R1=c("Cameroon", "Central African Republic", "Ghana")
R2=c("Jordan", "United Arab Emirates", "Iraq")
region = ifelse(M$entidad %in% R1, "R1", "R2")
M1 = cbind(M, region)
```

#### Crea un subconjunto de datos de las variables elegidas, quita la entidad y las filas con datos faltantes
Al utilizar la librería dplyr, seleccionamos las variables que nos interesan y quitamos la entidad y las filas con datos faltantes para así obtener una base de datos sin datos faltantes y tenga un impacto positivo a nuestro análisis.

```{r}
library(dplyr)
M1=select(M1, "anio", "region", "acceso_electrd", "acceso_combust_limpios", "energ_renov", "consumo_energ_prim", "electrd_fosiles", "emisiones_CO2", "crecimiento_PIB", "electrd_de_f_bajas_carb")
M1 = na.omit(M1)
str(M1)
```

## Guardar la base de datos
```{r}
write.csv(M1, "datosEq7.csv", row.names = FALSE)  # cambia # por el número de tu equipo.
# row.names=FALSE lo que hace es evitar que se añada una columna de conteo de filas.
# la nueva base de datos datosEq#.csv se guardará en el directorio de trabajo de R Studio (donde guardaste el documento de trabajo)
```
## Separar la base de datos
```{r}
R1 = subset(M1, M1$region == "R1")
R2 = subset(M1, M1$region == "R2")
str(R1)
```

## Calcular las medidas de cada región
## Medidas de la región 1
```{r}
cat("Región 1", "\n")
summary(R1)  # Calcula las principales medidas por variable para la Región 1
cat("\n", "Desviación estándar", "\n")
apply(R1,2, sd)  # Calcula la desviación estándar de las variables de la Región 1
cat("\n", "Rango Medio", "\n")
Rm = function(x)((max(x)+min(x))/2)
R1M = R1[c(-1, -2)]
apply(R1M, 2, Rm)  #Calcula el rango medio para las variables de la Región 1
```

## Medidas de la región 2

```{r}
cat("Región 2", "\n")
summary(R2)  # Calcula las principales medidas por variable para la Región 2
cat("\n", "Desviación estándar", "\n")
apply(R2,2, sd)  # Calcula la desviación estándar de las variables de la Región 2
cat("\n", "Rango Medio", "\n")
Rm = function(x)((max(x)+min(x))/2)
R2M = R2[c(-1, -2)]
apply(R2M, 2, Rm)  #Calcula el rango medio para las variables de la Región 2
```


## Histogramas
Histograma del consumo de energía generada por combustibles fósiles (carbón, petróleo y gas natural) en cada región.

```{r}
par(mfrow=c(1,2))
hist(R1$electrd_fosiles, col = "burlywood1", main="Región 1", xlab="Consumo de energía (TWh)", ylab = "Frecuencia")

hist(R2$electrd_fosiles, col = "burlywood3", main="Región 2", xlab="Consumo de energía (TWh)", ylab = "Frecuencia")
```

Histograma del consumo de energía generada por fuentes renovables.
```{r}
par(mfrow=c(1,2))
hist(R1$energ_renov, col = "aquamarine1", main="Región 1", xlab="Consumo en teravatios por hora (TWh)", ylab = "Frecuencia")

hist(R2$energ_renov, col = "aquamarine4", main="Región 2", xlab="Consumo en teravatios por hora (TWh)", ylab = "Frecuencia")
```

Histograma del consumo de energía generada por fuentes de energía bajas en carbono.
```{r}
par(mfrow=c(1,2))
hist(R1$electrd_de_f_bajas_carb, col = "darkseagreen1", main="Región 1", xlab="Consumo en teravatios por hora (TWh)", ylab = "Frecuencia")

hist(R2$electrd_de_f_bajas_carb, col = "darkseagreen3", main="Región 2", xlab="Consumo en teravatios por hora (TWh)", ylab = "Frecuencia")

```


## Gráficos de caja y bigote

Gráfica de caja y bigote del acceso a la electricidad por región.
```{r}
boxplot(acceso_electrd ~ region, data = M, col = "coral4", main = "Acceso a la electricidad por región", xlab = "Región", ylab = "Porcentaje de acceso a la electricidad")
```
Estadísticos del acceso a la electricidad por región.
```{r}
cat("Región 1", "\n")
summary(R1$acceso_electrd)
cat("----------------------------------", "\n")
cat("Región 2", "\n")
summary(R2$acceso_electrd)
```

Gráfica de caja y bigote del acceso a combustibles limpios por región.
```{r}
boxplot(acceso_combust_limpios ~ region, data = M, col = "cadetblue1", main = "Acceso a combustibles limpios por región", xlab = "Región", ylab = "(%) de acceso a combustibles limpios")
```  

Estadísticos del acceso a combustibles limpios por región.
```{r}
cat("Región 1", "\n")
summary(R1$acceso_combust_limpios)
cat("----------------------------------", "\n")
cat("Región 2", "\n")
summary(R2$acceso_combust_limpios)
```

Gráfica de caja y bigote del consumo de energía primaria por región.
```{r}
boxplot(consumo_energ_prim ~ region, data = M, col = "chartreuse4", main = "Consumo de energía primaria por región", xlab = "Región", ylab = "Consumo de energía primaria (TWh)")
```

Estadísticos del consumo de energía primaria por región.
```{r}
cat("Región 1", "\n")
summary(R1$consumo_energ_prim)
cat("----------------------------------", "\n")
cat("Región 2", "\n")
summary(R2$consumo_energ_prim)
```

Gráfica de caja y bigote del emisiones de CO2 por región.
```{r}
boxplot(emisiones_CO2 ~ region, data = M, col = "azure4", main = "Emisiones de CO2 por región", xlab = "Región", ylab = "Emisiones de CO2 (MtCO2)")
```

Estadísticos del emisiones de CO2 por región.
```{r}
cat("Región 1", "\n")
summary(R1$emisiones_CO2)
cat("----------------------------------", "\n")
cat("Región 2", "\n")
summary(R2$emisiones_CO2)
```


## Coeficiente de correlación
```{r}
correl = cor(R1[c(-1, -2)])
round(correl, 3)

correl2 = cor(R2[c(-1, -2)])
round(correl2, 3)
```

## Gráficos de dispersión
```{r}
plot(R1, col = "blue")  # Todos los gráficos
plot(R1$electrd_fosiles, R1$electrd_de_energ_renov, col = "red", pch = 20, main = "Electridad de origen fósil vs de origen renovable") # Gráficos de dos en dos
```
