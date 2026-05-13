---
title: "Preprocesamiento"
output: github_document
date: "2024-04-09"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
rm(list = ls())
graphics.off()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
cat("\014")
options(scipen = 999)  
options(digits = 3)    
library(pacman)
p_load(DataExplorer, VIM, MASS, mice, naniar, dlookr, dplyr, 
       caret, ggplot2, cowplot, fastDummies, tictoc, 
       data.table, DMwR2, plotly, performance, see, flextable, 
       visdat, arules)
datos <- read.csv("loan_prediction-II.csv", stringsAsFactors = T, sep = ";", na.strings = "")
str(datos)
datos$Loan_ID <- NULL
# Convierte la columna 'Credit_History' del data frame 'datos' en un factor
datos$Credit_History <- as.factor(datos$Credit_History)
# Muestra los niveles del factor 'Credit_History'
levels(datos$Credit_History)
# Cambia los niveles del factor 'Credit_History' a 'Malo' y 'Bueno'
levels(datos$Credit_History) <- c("Malo", "Bueno")

# Evaluando la variable target: Loan_Status

# Crea una tabla de frecuencias de la variable 'Loan_Status'
table(datos$Loan_Status)
# Calcula las proporciones de la variable 'Loan_Status' y las multiplica por 100 para obtener porcentajes
prop.table(table(datos$Loan_Status)) * 100
str(datos)






```

# I. CASO DREAM HOUSING FINANCE 

##  1. Descripción del caso  



```{r}
str(datos)
```

## 2. Detección de datos perdidos o missing value 



```{r pressure, echo=FALSE}
# Carga la biblioteca 'DataExplorer'
library(DataExplorer)
# Grafica los datos perdidos en el data frame 'datos' usando un tema clásico
plot_missing(datos, theme = theme_classic())
# Carga la biblioteca 'visdat'
library(visdat)
# Grafica los datos perdidos en el data frame 'datos'
vis_miss(datos)
```
## 2. Detección de datos perdidos o missing value (procentajes) 

```{r}

# Encuentra los índices de las filas que tienen al menos un dato perdido
rmiss <- which(rowSums(is.na(datos)) != 0, arr.ind = T)
# Muestra el número de filas con al menos un dato perdido
length(rmiss)
# Calcula el porcentaje de filas con al menos un dato perdido
length(rmiss) * 100 / dim(datos)[1]
# Calcula el número total de datos perdidos en el data frame 'datos'
sum(is.na(datos)) 
# Número de registros con datos perdidos por variable
# Calcula el número de datos perdidos en cada columna del data frame 'datos'
colSums(is.na(datos))
```
```{r, echo=FALSE}
# Graficar la cantidad de valores perdidos
# Carga la biblioteca 'VIM'
library(VIM)
# Grafica la cantidad de valores perdidos en cada variable del data frame 'datos'
graf_perdidos <- aggr(datos, prop = F, 
                      numbers  = TRUE,
                      sortVars = TRUE,
                      combine  = FALSE,
                      cex.axis = 0.5)
```
## Exploración de datos perdidos con naniar

```{r}
# Exploración de datos perdidos con naniar --------------------
# Carga la biblioteca 'naniar'
library(naniar) 

# Calcula el número total de datos perdidos en el data frame 'datos'
n_miss(datos) 
# Calcula el número total de datos completos en el data frame 'datos'
n_complete(datos)

# Calcula la proporción de datos perdidos en el data frame 'datos'
prop_miss(datos)     # 2831/(11500*13), % celdas con datos perdidos
# Calcula la proporción de datos completos en el data frame 'datos'
1 - prop_miss(datos) # % de celdas con datos completos

# Datos perdidos por variable
# Muestra un resumen de los datos perdidos por variable en el data frame 'datos'
miss_var_summary(datos)
# Grafica los datos perdidos por variable en el data frame 'datos'
gg_miss_var(datos)

# Calcula el número de datos perdidos en la variable 'Gender' del data frame 'datos'
n_miss(datos$Gender)
# Calcula el número de datos perdidos en la variable 'Loan_Status' del data frame 'datos'
n_miss(datos$Loan_Status)

# Grafica los datos perdidos por variable en el data frame 'datos', mostrando el porcentaje de datos perdidos y facetando por 'Loan_Status'
datos %>% gg_miss_var(show_pct = T, facet = Loan_Status)
```

## Visualizando los datos perdidos por casos y variables

```{r,echo=FALSE}
# Visualizando los datos perdidos por casos y variables
# Carga la biblioteca 'visdat'
library(visdat)
# Grafica los datos perdidos en el data frame 'datos', agrupando los casos con patrones similares de datos perdidos
vis_miss(datos, cluster = TRUE)
# Relacionando datos no perdidos (!NA) y perdidos (NA) de 
# Credit_History con la variable target: Loan Status
# Carga la biblioteca 'VIM'
library(VIM)
# Carga la biblioteca 'dplyr'
library(dplyr)

# Selecciona las variables 'Loan_Status' y 'Credit_History' del data frame 'datos'
# y luego grafica la relación entre los datos perdidos y no perdidos de 'Credit_History' con 'Loan_Status'
datos %>% 
  dplyr::select(Loan_Status, Credit_History) %>% # (target, v predict)
  spineMiss()
# Selecciona las variables 'Loan_Status' y 'LoanAmount' del data frame 'datos'
# y luego grafica la relación entre los datos perdidos y no perdidos de 'LoanAmount' con 'Loan_Status'
datos %>% 
  dplyr::select(Loan_Status, LoanAmount) %>%
  spineMiss()
```

## Consideraciones con las variables con datos missing

```{r}
# Consideraciones con las variables con datos missing ---------
# Crea una tabla de frecuencias de la variable 'Gender' en el data frame 'datos'
table(datos$Gender)
# Crea una tabla de frecuencias de la variable 'Gender' en el data frame 'datos' y agrega los márgenes
addmargins(table(datos$Gender))
# Calcula las proporciones de la variable 'Gender' en el data frame 'datos'
prop.table(table(datos$Gender))

# Crea una tabla de frecuencias de la variable 'Gender' en el data frame 'datos', incluyendo los datos perdidos
table(datos$Gender, useNA = "always")
# Calcula las proporciones de la variable 'Gender' en el data frame 'datos', incluyendo los datos perdidos
prop.table(table(datos$Gender, useNA = "always"))
# Crea una tabla de frecuencias de la variable 'Gender' en el data frame 'datos', incluyendo los datos perdidos, y agrega los márgenes
addmargins(table(datos$Gender, useNA = "always"))

# Grafica un histograma de la variable 'Gender' en el data frame 'datos', incluyendo los datos perdidos
ggplot(datos) + aes(Gender) + geom_bar() # Toma en cuenta el NA

# Grafica un histograma de la variable 'Gender' en el data frame 'datos', excluyendo los datos perdidos
ggplot(datos) + aes(Gender) + geom_bar() +
  scale_x_discrete(na.translate = FALSE)

# Calcula la media de la variable 'LoanAmount' en el data frame 'datos', incluyendo los datos perdidos
mean(datos$LoanAmount)              # NA = Not available
# Calcula la media de la variable 'LoanAmount' en el data frame 'datos', excluyendo los datos perdidos
mean(datos$LoanAmount, na.rm = T)   # ¿Remove NA?
```

# 3. Pre-procesamiento de datos

```{r}
# Imputando los valores perdidos usando k-nn ------------------
# Cargamos la biblioteca VIM para el manejo de datos faltantes
library(VIM)

# Usamos la función kNN de la biblioteca VIM para imputar los datos faltantes
# en las variables especificadas, usando el método de los k vecinos más cercanos (kNN)
datos_transformado1 <- VIM::kNN(datos, k = 5,
                                variable = c("Gender", "Married", "Dependents",
                                             "Self_Employed", "LoanAmount",
                                             "Loan_Amount_Term", "Credit_History"),
                                imp_var = FALSE)

# Visualizamos los datos faltantes en nuestro conjunto de datos
plot_missing(datos_transformado1)

# Cargamos la biblioteca caret para el preprocesamiento de datos
library(caret)

# Identificamos las variables con varianza cercana a cero
nearZeroVar(datos_transformado1, saveMetrics = TRUE)

# Contamos la frecuencia de cada valor en la variable 'Nacionality'
table(datos_transformado1$Nacionality)

# Calculamos el porcentaje de cada valor en la variable 'Nacionality'
prop.table(table(datos_transformado1$Nacionality))*100

# Eliminamos la variable 'Nacionality' de nuestro conjunto de datos
datos_transformado1$Nacionality <- NULL

# Contamos la frecuencia de cada valor en la variable 'Dependents'
table(datos_transformado1$Dependents)

# Verificamos la estructura de nuestro conjunto de datos
str(datos_transformado1)

# Estandarizamos las variables numéricas de nuestro conjunto de datos
preProcValues <- preProcess(datos_transformado1, 
                            method = c("center", "scale"))

# Verificamos los valores de preprocesamiento
preProcValues

# Aplicamos los valores de preprocesamiento a nuestro conjunto de datos
datos_transformado2 <- predict(preProcValues, 
                               datos_transformado1)
```


```{r include=FALSE}
# Cargamos la biblioteca ggplot2 para la visualización de datos
library(ggplot2)

# Creamos histogramas para las variables numéricas sin transformar
# 'ApplicantIncome', 'CoapplicantIncome', 'LoanAmount', 'Loan_Amount_Term'
ggplot(datos_transformado1) + aes(ApplicantIncome) + 
  geom_histogram(color = "black", fill = "white") -> g1 ; g1

ggplot(datos_transformado1) + aes(CoapplicantIncome) + 
  geom_histogram(color = "black", fill = "white") -> g2 ; g2

ggplot(datos_transformado1) + aes(LoanAmount) + 
  geom_histogram(color = "black", fill = "white") -> g3 ; g3

ggplot(datos_transformado1) + aes(Loan_Amount_Term) + 
  geom_histogram(color = "black", fill = "white") -> g4 ; g4

# Creamos histogramas para las variables numéricas transformadas
# 'ApplicantIncome', 'CoapplicantIncome', 'LoanAmount', 'Loan_Amount_Term'
ggplot(datos_transformado2, aes(ApplicantIncome)) + 
  geom_histogram(color = "black", fill = "darksalmon") -> g9 ; g9

ggplot(datos_transformado2) + aes(CoapplicantIncome) + 
  geom_histogram(color= "black", fill = "darksalmon") -> g10 ; g10

ggplot(datos_transformado2) + aes(LoanAmount) + 
  geom_histogram(color= "black", fill = "darksalmon") -> g11 ; g11

ggplot(datos_transformado2) + aes(Loan_Amount_Term) + 
  geom_histogram(color= "black", fill = "darksalmon") -> g12 ; g12

```


```{r}
# Cargamos la biblioteca cowplot para la organización de gráficos
library(cowplot)

# Organizamos los gráficos en una cuadrícula para una mejor comparación
plot_grid(g1, g9, g2, g10, g3, g11, g4, g12, ncol = 2)
#Este código genera histogramas para las variables numéricas tanto sin 
#transformar como transformadas en tu conjunto de datos. Luego, organiza estos
#histogramas en una cuadrícula para una fácil comparación visual. Los histogramas
#sin transformar están en blanco, mientras que los histogramas transformados 
#están en darksalmon.


```


## Eliminar variables altamente correlacionadas

```{r}
# Cargamos la biblioteca caret para el preprocesamiento de datos
library(caret)

# Normalizamos las variables numéricas de nuestro conjunto de datos al rango [0,1]
preProcValues <- preProcess(datos_transformado1, 
                            method = c("range"))

# Verificamos los valores de preprocesamiento
preProcValues

# Aplicamos los valores de preprocesamiento a nuestro conjunto de datos
datos_transformado2 <- predict(preProcValues, 
                               datos_transformado1)

# Cargamos la biblioteca fastDummies para la creación de variables dummies
library(fastDummies)

# Creamos variables dummies para las variables categóricas en nuestro conjunto de datos
datos_transformado3 <- dummy_cols(datos_transformado2, 
                                  select_columns = c("Gender",
                                                     "Married","Dependents",
                                                     "Education",
                                                     "Self_Employed",
                                                     "Credit_History",
                                                     "Property_Area"),
                                  remove_first_dummy = TRUE,
                                  remove_selected_columns = TRUE)                                        

# Verificamos la estructura de nuestro conjunto de datos
str(datos_transformado3)

# Calculamos la correlación entre las primeras 4 variables de nuestro conjunto de datos
descrCor1 <- cor(datos_transformado3[, c(1, 2, 3, 4)])

# Resumimos la correlación
summary(descrCor1[upper.tri(descrCor1)])

# Identificamos las variables altamente correlacionadas
altaCorr <- findCorrelation(descrCor1, 
                            cutoff = 0.40, 
                            names = TRUE,
                            verbose = TRUE,
                            exact = F)

# Verificamos las variables altamente correlacionadas
altaCorr

# Calculamos la correlación entre las primeras 2 variables y la cuarta variable de nuestro conjunto de datos
descrCor2 <- cor(datos_transformado3[, c(1, 2, 4)])

# Resumimos la correlación
summary(descrCor2[upper.tri(descrCor2)])

# Identificamos las variables altamente correlacionadas
altaCorr2 <- findCorrelation(descrCor2, cutoff = 0.40, 
                             names = TRUE)

# Verificamos las variables altamente correlacionadas
altaCorr2

# Eliminamos la variable 'LoanAmount' de nuestro conjunto de datos

datos_transformado3$LoanAmount <- NULL

#Este código normaliza las variables numéricas, crea variables dummies para 
#las variables categóricas, identifica las variables altamente correlacionadas 
#y elimina la variable ‘LoanAmount’ de tu conjunto de datos


```

# II. CASO CENSUS
```{r include=FALSE}
# El objetivo es poder predecir el salario de una persona de 
# manera categórica: <= 50K o > 50K
# Eliminamos todas las variables del entorno de R
rm(list = ls())
# Cargamos los datos con la función read.csv()
census.csv <- read.csv("censusn.csv", sep = ";")
# Cargamos la biblioteca data.table para el manejo eficiente de datos
library(data.table)
# Cargamos los datos con la función fread() de data.table
censusn <- fread("censusn.csv", 
                 header = T, 
                 verbose = FALSE, 
                 stringsAsFactors = TRUE,
                 showProgress = TRUE)
# Verificamos la estructura de nuestro conjunto de datos
str(censusn)

# Eliminamos la variable 'id' de nuestro conjunto de datos
censusn$id <- NULL

```

```{r}
str(censusn)
```

# TRATAMIENTO DE DATOS PERDIDOS O MISSING VALUES
## 1. Detección de valores perdidos

```{r}
# Tratamiento de datos perdidos o missing values --------------
# 1. Detección de valores perdidos ----------------------------

# Detección de valores perdidos con el paquete DataExplorer
# Cargamos la biblioteca DataExplorer para la exploración de datos
library(DataExplorer)
# Visualizamos los datos faltantes en nuestro conjunto de datos
plot_missing(censusn) 

# Cargamos la biblioteca naniar para el manejo de datos faltantes
library(naniar)

# Resumimos los datos faltantes en nuestro conjunto de datos
miss_var_summary(censusn)

# Contamos el número de datos faltantes en cada columna de nuestro conjunto de datos
colSums(is.na(censusn))
```

## 2. Eliminación de datos perdidos

```{r}
# 2. Eliminación de datos perdidos ----------------------------
# Eliminamos las filas con datos faltantes de nuestro conjunto de datos
census.cl <- na.omit(censusn)

# Visualizamos los datos faltantes en nuestro conjunto de datos después de la eliminación
plot_missing(census.cl)
```

## 3. Imputación con el paquete DMwR2

```{r}
# 3. Imputación con el paquete DMwR2 --------------------------
# Cargamos la biblioteca DMwR2 para el manejo de datos faltantes
library(DMwR2)  # Data Mining with R, Luis Torgo

# Función centralImputation()
# Si la variable es numérica (numeric o integer) reemplaza los
# valores faltantes con la mediana.
# Si la variable es categórica (factor) reemplaza los valores 
# faltantes con la moda. 

# Contamos la frecuencia de cada valor en la variable 'employment'
table(censusn$employment)

# Verificamos el valor de 'employment' para la fila 28 en nuestro conjunto de datos
censusn[28, "employment"]

# Imputamos los datos faltantes con la función centralImputation()
census.ci <- centralImputation(censusn)

# Visualizamos los datos faltantes en nuestro conjunto de datos después de la imputación
plot_missing(census.ci) 

# Verificamos el valor de 'employment' para la fila 28 en nuestro conjunto de datos después de la imputación
census.ci[28,"employment"]

#Este código cuenta la frecuencia de cada valor en la variable ‘employment’,
#verifica el valor de ‘employment’ para la fila 28, imputa los datos faltantes,
#visualiza los datos faltantes después de la imputación y verifica el valor de 
#‘employment’ para la fila 28 después de la imputación.

```

#  DETECCIÓN DE OUTLIERS

### Para la variable age

```{r}
# 1. Detección de outliers univariados ------------------------

# Para la variable age
# Cargamos la biblioteca ggplot2 para la visualización de datos
library(ggplot2)

# Creamos un diagrama de caja para la variable 'age' en nuestro conjunto de datos
ggplot(censusn) + aes(x = "", age) + 
  geom_boxplot(fill = "peru") + labs(x = "")

# Creamos un diagrama de caja para la variable 'age' en nuestro conjunto de datos con la función boxplot()
boxplot(censusn$age, col = "peru")

# Calculamos las estadísticas del diagrama de caja para la variable 'age'
boxplot.stats(censusn$age)

# Resumimos la variable 'age' en nuestro conjunto de datos
summary(censusn$age)

# Identificamos los valores atípicos en la variable 'age'
outliers1 <- boxplot(censusn$age, col = "peru")$out

# Resumimos los valores atípicos
summary(outliers1)

# Cargamos la biblioteca dplyr para el manejo de datos
library(dplyr)

# Filtramos nuestro conjunto de datos para eliminar los valores atípicos en la variable 'age'
censusn.out <- filter(censusn, age < 79)  

# Verificamos las dimensiones de nuestro conjunto de datos después de la eliminación de valores atípicos
dim(censusn.out)

# Verificamos las dimensiones de nuestro conjunto de datos original
dim(censusn)

# Creamos un diagrama de caja para la variable 'age' en nuestro conjunto de datos después de la eliminación de valores atípicos
ggplot(censusn.out) + aes(x = "", age) + 
  geom_boxplot(fill = "peru") + labs(x = "")

# Identificamos los valores atípicos en la variable 'age' después de la eliminación de valores atípicos
outliers2 <- boxplot(censusn.out$age, col = "cadetblue3")$out

# Resumimos los valores atípicos después de la eliminación de valores atípicos
summary(outliers2)

"Este código crea diagramas de caja para la variable ‘age’, identifica y resume
los valores atípicos, filtra el conjunto de datos para eliminar los valores
atípicos, verifica las dimensiones del conjunto de datos antes y después de la
eliminación de valores atípicos, y finalmente identifica y resume los valores 
atípicos después de la eliminación de valores atípicos."
```

### Para la variable hours.per.week

```{r}
# Para la variable hours.per.week

# Cargamos la biblioteca ggplot2 para la visualización de datos
library(ggplot2)

# Creamos un diagrama de caja para la variable 'hours.per.week' en nuestro conjunto de datos
ggplot(censusn) + aes(x = "", hours.per.week) + 
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Boxplot de Hours.per.week",
       x = "") +
  theme_minimal()

# Identificamos los valores atípicos en la variable 'hours.per.week'
outliers3 <- boxplot(censusn$hours.per.week)$out

# Resumimos los valores atípicos
summary(outliers3)

# Cargamos la biblioteca dlookr para la detección de valores atípicos
library(dlookr)

# Cargamos la biblioteca flextable para la visualización de tablas
library(flextable)

# Diagnosticamos los valores atípicos en nuestro conjunto de datos
diagnose_outlier(censusn) %>% flextable()

# Creamos un gráfico de diagnóstico de valores atípicos para todas las variables numéricas en nuestro conjunto de datos
plot_outlier(censusn)

# Cargamos la biblioteca rstatix para la detección de valores atípicos
library(rstatix)

# Identificamos los valores atípicos en la variable 'age' en nuestro conjunto de datos
identify_outliers(censusn, age)


# Values above Q3 + 1.5*IQR or below Q1 - 1.5*IQR are 
# considered as outliers. 
# Values above Q3 + 3*IQR or below Q1 - 3*IQR are 
# considered as extreme points (or extreme outliers).

"Este código crea un diagrama de caja para la variable ‘hours.per.week’, 
identifica y resume los valores atípicos en ‘hours.per.week’, diagnostica 
los valores atípicos en todo el conjunto de datos, crea un gráfico de 
diagnóstico de valores atípicos para todas las variables numéricas, e 
identifica los valores atípicos en la variable ‘age’"


```


# IMPUTACIÓN DE OUTLIERS
### imputacion Para la variable hours.per.week

```{r}
# Diagnóstico de outliers -------------------------------------
# Cargamos la biblioteca dlookr para la detección de valores atípicos
library(dlookr)

# Diagnosticamos los valores atípicos en nuestro conjunto de datos
diagnose_outlier(censusn) %>% flextable()

# Creamos un gráfico de diagnóstico de valores atípicos para todas las variables numéricas en nuestro conjunto de datos
plot_outlier(censusn)

# Imputamos los valores atípicos en la variable 'hours.per.week' con el método de capping
hpw.imp.out <- imputate_outlier(censusn, 
                                hours.per.week, 
                                method = "capping")
"Este código diagnostica los valores atípicos en todo el conjunto de datos, 
crea un gráfico de diagnóstico de valores atípicos para todas las variables 
numéricas, e imputa los valores atípicos en la variable ‘hours.per.week’ 
utilizando el método de capping."


# method : method of missing value imputation.
#   predictor is numerical variable
#       "mean"    : arithmetic mean
#       "median"  : median
#       "mode"    : mode
#       "capping" : Impute the upper outliers with 95 percentile,
#                   and Impute the bottom outliers with 5 percentile.

# Calculamos los percentiles 5 y 95 de la variable 'hours.per.week'
quantile(censusn$hours.per.week, c(0.05,0.95))

# Creamos un gráfico de los datos imputados para la variable 'hours.per.week'
plot(hpw.imp.out)

# Creamos un diagrama de caja para los datos imputados de la variable 'hours.per.week'
ggplot() + aes(x = "", hpw.imp.out) + 
  geom_boxplot(fill = "cadetblue") + labs(x = "")

# Identificamos los valores atípicos en los datos imputados de la variable 'hours.per.week'
boxplot(hpw.imp.out)$out

# Identificamos los valores atípicos en la variable 'hours.per.week' antes de la imputación
outliers3 <- boxplot(censusn$hours.per.week)$out

# Resumimos los valores atípicos antes de la imputación
summary(outliers3)

# Identificamos los valores atípicos en la variable 'hours.per.week' después de la imputación
outliers.hpw <- boxplot(hpw.imp.out)$out

# Resumimos los valores atípicos después de la imputación
summary(outliers.hpw)
"Este código calcula los percentiles 5 y 95 de la variable ‘hours.per.week’, 
crea un gráfico y un diagrama de caja para los datos imputados de 
‘hours.per.week’, identifica y resume los valores atípicos en ‘hours.per.week’ 
antes y después de la imputación."

```

#  DISCRETIZACIÓN
### discretizacion Para la variable hours.per.week
```{r}
# Discretización ----------------------------------------------
# 1. Discretización usando BoxPlot y la función cut() ---------
#    Basada únicamente en la misma variable con outliers
# Asignamos el conjunto de datos 'censusn' a 'censusd1'
censusd1 <- censusn

# Resumimos la variable 'hours.per.week' en nuestro conjunto de datos
summary(censusd1$hours.per.week)

# Cargamos la biblioteca ggplot2 para la visualización de datos
library(ggplot2)

# Creamos un diagrama de caja para la variable 'hours.per.week' en nuestro conjunto de datos
ggplot(censusd1) + aes(x = " ", y = hours.per.week) + 
  geom_boxplot(fill = "peru") +
  scale_y_continuous(breaks = seq(0, 100, 5)) + 
  labs(title = "Box Plot de horas de trabajo a la semana",
       xlab = " ", ylab = "Horas de trabajo a la semana") +
  theme_minimal()

# Resumimos nuevamente la variable 'hours.per.week' en nuestro conjunto de datos
summary(censusd1$hours.per.week)

# Calculamos los cuantiles de la variable 'hours.per.week' en nuestro conjunto de datos
quantile(censusd1$hours.per.week)
"Este código asigna el conjunto de datos ‘censusn’ a ‘censusd1’, resume la 
variable ‘hours.per.week’, crea un diagrama de caja para ‘hours.per.week’, 
resume nuevamente ‘hours.per.week’ y calcula los cuantiles de ‘hours.per.week’."

# Menos de 40          [-Inf a 40>
# De 40 a menos de 45  [40 a 45>
# De 45 a más          [45 a Inf>

# "[  ]"  "[  >"  "<  ]"

# Creamos una nueva variable 'hpw_cat1' en nuestro conjunto de datos que categoriza la variable 'hours.per.week' en tres grupos
censusd1$hpw_cat1 <- cut(censusd1$hours.per.week, 
                         breaks = c(-Inf, 40, 45, Inf),
                         labels = c("Menos de 40", 
                                    "De 40 a menos de 45",
                                    "De 45 a más"),
                         right  = FALSE)  # [   >

# Contamos la frecuencia de cada grupo en la variable 'hpw_cat1'
table(censusd1$hpw_cat1)  

# Creamos un gráfico de barras para la variable 'hpw_cat1'
ggplot(censusd1) + aes(hpw_cat1) + 
  geom_bar(color = "black", fill = "darkgreen") + 
  theme_light() + 
  labs(title = "Gráfico de Barras", 
       x = "Horas de trabajo a la semana", 
       y = "Frecuencia") 

# Creamos un gráfico de barras apilado para las variables 'hpw_cat1' y 'salary'
ggplot(censusd1) + aes(x = hpw_cat1, fill = salary) +
  geom_bar(position = position_fill()) +
  theme_bw() +
  labs(title ="Salario según las horas de trabajo a la semana",
       subtitle = "Usando cuartiles",
       x = "Horas de trabajo", 
       y = "Frecuencia") + 
  scale_fill_manual(values = c("darkolivegreen3", "firebrick2")) +
  theme(legend.position = "bottom") -> g1 ; g1

# Convertimos el gráfico de ggplot a un gráfico interactivo con la función ggplotly()
ggplotly()

"Este código crea una nueva variable ‘hpw_cat1’ que categoriza la variable
‘hours.per.week’ en tres grupos, cuenta la frecuencia de cada grupo, crea un
gráfico de barras para ‘hpw_cat1’, crea un gráfico de barras apilado para
‘hpw_cat1’ y ‘salary’, y finalmente convierte el gráfico de ggplot a un gráfico
interactivo con la función ggplotly()"
```

## 3. Discretización usando clusters y la función discretize()
### discretizacion usando clusters y la función discretize() Para la variable hours.per.week
```{r}
# 3. Discretización usando clusters y la función discretize() ----
#    Basado en un cluster de partición k-means
# Asignamos el conjunto de datos 'censusn' a 'censusd3'
censusd3 <- censusn

# Resumimos la variable 'hours.per.week' en nuestro conjunto de datos
summary(censusd3$hours.per.week)

# Cargamos la biblioteca arules para la discretización de datos
library(arules)

# Establecemos una semilla para la reproducibilidad
set.seed(2022)

# Creamos una nueva variable 'hpw_cat3' en nuestro conjunto de datos que 
#categoriza la variable 'hours.per.week' en tres grupos utilizando el método de clustering
censusd3$hpw_cat3 <- discretize(censusd3$hours.per.week, 
                                method = "cluster", 
                                breaks = 3)
"Este código asigna el conjunto de datos ‘censusn’ a ‘censusd3’, 
resume la variable ‘hours.per.week’, y crea una nueva variable ‘hpw_cat3’ 
que categoriza la variable ‘hours.per.week’ en tres grupos utilizando el 
método de clustering."

# method: discretization method. Available are: 
# - "interval" (equal interval width), 
# - "frequency" (equal frequency), 
# - "cluster" (k-means clustering) and 
# - "fixed" (categories specifies interval boundaries). 

# Contamos la frecuencia de cada grupo en la variable 'hpw_cat3'
table(censusd3$hpw_cat3)  

# Creamos un gráfico de barras para la variable 'hpw_cat3'
ggplot(censusd3) + aes(hpw_cat3) + 
  geom_bar(color = "black", fill = "darkgreen") + 
  theme_light() + 
  labs(title = "Gráfico de Barras", 
       x = "Horas de trabajo a la semana", 
       y = "Frecuencia") 

# Creamos un gráfico de barras apilado para las variables 'hpw_cat3' y 'salary'
ggplot(censusd3) + aes(x = hpw_cat3, fill = salary) +
  geom_bar(position = position_fill()) +
  theme_bw() +
  labs(title ="Salario según las horas de trabajo a la semana",
       subtitle = "Usando cluster k-means",
       x = "Horas de trabajo", 
       y = "Frecuencia") + 
  scale_fill_manual(values = c("darkolivegreen3", "firebrick2")) +
  theme(legend.position = "bottom") -> g3 ; g3

# Cargamos la biblioteca cowplot para la organización de gráficos
library(cowplot)

# Organizamos los gráficos en una cuadrícula para una fácil comparación
plot_grid(g1, g3)
"Este código cuenta la frecuencia de cada grupo en la variable ‘hpw_cat3’, 
crea un gráfico de barras para ‘hpw_cat3’, crea un gráfico de barras apilado
para ‘hpw_cat3’ y ‘salary’, y organiza los gráficos en una cuadrícula para 
una fácil comparación."
```


