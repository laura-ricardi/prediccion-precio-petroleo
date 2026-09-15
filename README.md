# Predicción del precio del petróleo Brent

## Descripción

Proyecto desarrollado como trabajo final de la Diplomatura en Ciencia de Datos.

El objetivo es analizar la relación entre el sentimiento de noticias internacionales y la evolución del precio del petróleo Brent, integrando información proveniente de distintas fuentes financieras y de noticias.

## Objetivo

Evaluar si indicadores de sentimiento derivados de noticias, junto con variables financieras, pueden aportar información para explicar y predecir el precio futuro del petróleo Brent a distintos horizontes temporales.

Se analizaron predicciones a:

- 1 día
- 3 días
- 5 días
- 7 días

## Datos

Se integraron datos provenientes de:

- **The Guardian API:** noticias internacionales relacionadas con petróleo, Brent, OPEC y mercado petrolero.
- **Yahoo Finance:** precio del petróleo Brent, índice VIX, índice del dólar y precio del oro.

El período analizado comprende diciembre de 2025 a agosto de 2026.

## Metodología

El análisis se desarrolló en R siguiendo las siguientes etapas:

1. Extracción de noticias mediante API.
2. Limpieza y filtrado de las noticias.
3. Análisis de sentimiento de los titulares utilizando los léxicos AFINN y NRC.
4. Agregación diaria de los indicadores de sentimiento.
5. Integración con variables financieras.
6. Construcción de variables derivadas, incluyendo un indicador suavizado de miedo.
7. Construcción de variables objetivo para distintos horizontes de predicción.
8. División cronológica de los datos en conjuntos de entrenamiento (80%) y prueba (20%).
9. Ajuste de modelos de regresión lineal múltiple.
10. Comparación de modelos con y sin el precio actual del Brent y con y sin información de sentimiento.
11. Evaluación mediante R², RMSE, MAE y correlación entre valores observados y predichos.

## Herramientas

- R
- tidyverse
- httr
- jsonlite
- syuzhet
- quantmod
- zoo
- ggplot2

## Reproducibilidad

Para ejecutar el análisis es necesario contar con una API key de The Guardian.

La clave **no se encuentra incluida en este repositorio**. Debe configurarse localmente mediante la variable de entorno:

`GUARDIAN_API_KEY`

Luego se puede ejecutar el archivo:

`analisis_sentimiento.R`

## Estructura del repositorio

```text
analisis-sentimiento-petroleo/
│
├── README.md
├── analisis_sentimiento.R
├── .gitignore
└── figures/

```
## Resultados

Los modelos fueron evaluados para distintos horizontes temporales mediante R², RMSE, MAE y correlación entre los valores observados y predichos.

### Predicción a 1 día

![Predicción Brent a 1 día](figures/figura 1.jpg)

### Predicción a 3 días

![Predicción Brent a 3 días](figures/figura 2.jpg)

### Predicción a 5 días

![Predicción Brent a 5 días](figures/figura 3.jpg)

### Predicción a 7 días

![Predicción Brent a 7 días](figures/figura 4.jpg)
