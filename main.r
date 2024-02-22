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

## Crea un subconjunto de datos de las variables elegidas, quita la entidad y las filas con datos faltantes
```{r}
library(dplyr)
M1=select(M1, "entidad", "region", "electrd_fosiles", "electrd_nuclear", "electrd_de_energ_renov", "electrd_de_f_bajas_carb", "finan_paises_desarr", "crecimiento_PIB", "PIB_per_cap", "densidad_pobl_Km2", "superficie", "latitud", "longitud")
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
```{r}
par(mfrow=c(1,2))
hist(R1$electrd_fosiles, col = "cadetblue", main="Región 1", xlab="Electricidad de origen fósil(kWh)", ylab = "Frecuencia")

hist(R2$electrd_fosiles, col = "darkkhaki", main="Región 2", xlab="Electricidad de origen fósil(kWh)", ylab = "Frecuencia")
```

## Gráficos de caja y bigote
```{r}
boxplot(electrd_fosiles ~ region, data = M, col = "coral2", main = "Electricidad de origen fósil", xlab = "Región", ylab = "Electricidad de origen fósil(kWh)")
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
