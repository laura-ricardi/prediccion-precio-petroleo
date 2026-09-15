# ==========================================================
# ANÁLISIS DE SENTIMIENTO Y PRECIO DEL PETRÓLEO BRENT
# ==========================================================
# Proyecto final - Diplomatura en Ciencia de Datos
#
# Flujo de trabajo:
# 1. Extracción de noticias mediante The Guardian API
# 2. Análisis de sentimiento de titulares
# 3. Descarga de variables financieras
# 4. Integración y feature engineering
# 5. Modelos de regresión lineal
# 6. Evaluación temporal train/test
# 7. Comparación de modelos y visualización
# ==========================================================


# ==========================================================
# 1. LIBRERÍAS
# ==========================================================

# Instalar previamente si es necesario:
# install.packages(c(
#   "httr",
#   "jsonlite",
#   "tidyverse",
#   "syuzhet",
#   "quantmod",
#   "zoo"
# ))

library(httr)
library(jsonlite)
library(tidyverse)
library(syuzhet)
library(quantmod)
library(zoo)


# ==========================================================
# 2. CONFIGURACIÓN
# ==========================================================

fecha_inicio <- "2025-12-01"
fecha_fin <- "2026-08-08"

# La API key no se almacena en el repositorio.
# Debe configurarse como variable de entorno:
#
# GUARDIAN_API_KEY
#
# Por ejemplo, en R:
# Sys.setenv(GUARDIAN_API_KEY = "SU_API_KEY")

api_key <- Sys.getenv("GUARDIAN_API_KEY")

if (api_key == "") {
  stop(
    "No se encontró la variable de entorno GUARDIAN_API_KEY. ",
    "Configure su API key antes de ejecutar el script."
  )
}


# Filtro temático para seleccionar noticias relacionadas
# con petróleo y mercado energético.

query_noticias <- paste(
  '("crude oil" OR "Brent crude" OR "oil price"',
  'OR "oil supply" OR "OPEC" OR "oil market")'
)


# ==========================================================
# 3. EXTRACCIÓN DE NOTICIAS - THE GUARDIAN API
# ==========================================================

obtener_noticias_completo <- function(
    fecha_desde,
    fecha_hasta,
    api_key,
    query
) {
  
  message(
    paste(
      "Iniciando descarga desde",
      fecha_desde,
      "hasta",
      fecha_hasta
    )
  )
  
  noticias_acumuladas <- data.frame()
  
  pagina <- 1
  continuar <- TRUE
  
  while (continuar) {
    
    message(
      paste(
        "Descargando página",
        pagina,
        "..."
      )
    )
    
    respuesta <- GET(
      "https://content.guardianapis.com/search",
      
      query = list(
        q = query,
        `from-date` = fecha_desde,
        `to-date` = fecha_hasta,
        page = pagina,
        `page-size` = 200,
        `show-fields` = "bodyText",
        `api-key` = api_key
      ),
      
      timeout(60)
    )
    
    
    # Manejo de errores de conexión o límites de API
    
    if (status_code(respuesta) != 200) {
      
      message(
        "La API devolvió un código distinto de 200. ",
        "Se detiene la descarga."
      )
      
      break
    }
    
    
    contenido <- content(
      respuesta,
      as = "text",
      encoding = "UTF-8"
    )
    
    
    datos <- fromJSON(
      contenido,
      flatten = TRUE
    )
    
    
    results <- datos$response$results
    
    
    if (
      is.null(results) ||
      length(results) == 0
    ) {
      
      continuar <- FALSE
      
    } else {
      
      # Filtrado de secciones no relacionadas
      # con el objetivo del análisis.
      
      pagina_limpia <- results %>%
        filter(
          !grepl(
            "Football|Sport|Australia news|Art and design|Books|Music|Life and style|The Filter US|Travel|Technology",
            sectionName,
            ignore.case = TRUE
          )
        ) %>%
        select(
          fecha = webPublicationDate,
          titulo = webTitle,
          cuerpo = fields.bodyText
        )
      
      
      noticias_acumuladas <- bind_rows(
        noticias_acumuladas,
        pagina_limpia
      )
      
      
      # Detener cuando se alcanza la última página.
      
      if (
        pagina >= datos$response$pages
      ) {
        
        continuar <- FALSE
        
      } else {
        
        pagina <- pagina + 1
      }
    }
  }
  
  
  # Conversión de fecha.
  
  if (
    nrow(noticias_acumuladas) > 0
  ) {
    
    noticias_acumuladas <- noticias_acumuladas %>%
      mutate(
        fecha = as.Date(
          as.POSIXct(
            fecha,
            format = "%Y-%m-%dT%H:%M:%SZ",
            tz = "UTC"
          )
        )
      )
  }
  
  
  return(noticias_acumuladas)
}


# Ejecución de la descarga.

petroleo_completo <- obtener_noticias_completo(
  fecha_inicio,
  fecha_fin,
  api_key,
  query_noticias
)


# Guardar datos descargados localmente.
#
# Este archivo contiene información obtenida mediante
# la API y no se incluye en el repositorio público.

write.csv(
  petroleo_completo,
  "petroleo_historico_hasta_actualidad.csv",
  row.names = FALSE
)


# ==========================================================
# 4. ANÁLISIS DE SENTIMIENTO
# ==========================================================

message(
  "Calculando métricas de sentimiento..."
)


# Análisis de sentimiento mediante NRC.

emociones_nrc <- get_nrc_sentiment(
  petroleo_completo$titulo
)


petroleo_completo <- petroleo_completo %>%
  mutate(
    
    sentimiento_affin =
      get_sentiment(
        titulo,
        method = "afinn"
      ),
    
    fear =
      emociones_nrc$fear,
    
    anger =
      emociones_nrc$anger,
    
    negative =
      emociones_nrc$negative
  )


# Consolidación diaria de las métricas
# de sentimiento.

tabla_sentimiento_diario <- petroleo_completo %>%
  group_by(fecha) %>%
  summarise(
    
    sentimiento_affin =
      mean(
        sentimiento_affin,
        na.rm = TRUE
      ),
    
    fear =
      mean(
        fear,
        na.rm = TRUE
      ),
    
    anger =
      mean(
        anger,
        na.rm = TRUE
      ),
    
    negative =
      mean(
        negative,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  )


# ==========================================================
# 5. DESCARGA DE VARIABLES FINANCIERAS
# ==========================================================

message(
  "Descargando datos financieros de Yahoo Finance..."
)


obtener_activo_financiero <- function(
    ticker,
    nombre_col,
    desde,
    hasta
) {
  
  datos <- getSymbols(
    ticker,
    src = "yahoo",
    from = desde,
    to = hasta,
    auto.assign = FALSE
  )
  
  
  df <- datos %>%
    
    as.data.frame() %>%
    
    rownames_to_column(
      var = "fecha"
    ) %>%
    
    mutate(
      fecha = as.Date(fecha)
    )
  
  
  # Fecha + precio de cierre.
  
  df <- df[
    ,
    c(1, 5)
  ]
  
  
  colnames(df) <- c(
    "fecha",
    nombre_col
  )
  
  
  return(df)
}


# Precio del petróleo Brent.

brent_final <- obtener_activo_financiero(
  "BZ=F",
  "brent",
  fecha_inicio,
  fecha_fin
)


# Índice de volatilidad VIX.

vix_final <- obtener_activo_financiero(
  "^VIX",
  "vix",
  fecha_inicio,
  fecha_fin
)


# Índice del dólar.

dolar_final <- obtener_activo_financiero(
  "DX-Y.NYB",
  "dolar",
  fecha_inicio,
  fecha_fin
)


# Precio del oro.

oro_final <- obtener_activo_financiero(
  "GC=F",
  "oro",
  fecha_inicio,
  fecha_fin
)


# ==========================================================
# 6. INTEGRACIÓN Y FEATURE ENGINEERING
# ==========================================================

message(
  "Integrando variables..."
)


# Calendario diario completo.

calendario_completo <- data.frame(
  
  fecha = seq(
    as.Date(fecha_inicio),
    as.Date(fecha_fin),
    by = "day"
  )
)


# Lista de datasets a integrar.

lista_df <- list(
  
  calendario_completo,
  
  tabla_sentimiento_diario,
  
  brent_final,
  
  vix_final,
  
  oro_final,
  
  dolar_final
)


# Integración de las diferentes fuentes.

tabla_resultado <- lista_df %>%
  
  reduce(
    full_join,
    by = "fecha"
  ) %>%
  
  arrange(fecha) %>%
  
  
  # Rellenar valores financieros faltantes
  # mediante arrastre del último valor disponible.
  #
  # En los días sin noticias se asigna cero
  # a las variables de sentimiento.
  
  mutate(
    
    across(
      c(
        brent,
        vix,
        oro,
        dolar
      ),
      ~ na.locf(
        .x,
        na.rm = FALSE
      )
    ),
    
    across(
      c(
        sentimiento_affin,
        fear,
        anger,
        negative
      ),
      ~ replace_na(
        .x,
        0
      )
    )
  ) %>%
  
  
  # Suavizado de la variable fear
  # mediante una media móvil de 5 días.
  
  mutate(
    
    fear_suavizado = rollmean(
      fear,
      k = 5,
      fill = 0,
      align = "right"
    )
  ) %>%
  
  
  # ========================================================
# VARIABLES OBJETIVO
# ========================================================

mutate(
  
  brent_t1 =
    lead(brent, 1),
  
  brent_t3 =
    lead(brent, 3),
  
  brent_t5 =
    lead(brent, 5),
  
  brent_t7 =
    lead(brent, 7),
  
  
  fecha_t1 =
    lead(fecha, 1),
  
  fecha_t3 =
    lead(fecha, 3),
  
  fecha_t5 =
    lead(fecha, 5),
  
  fecha_t7 =
    lead(fecha, 7)
) %>%
  
  
  # Las últimas observaciones no tienen
  # un valor futuro disponible.
  
  na.omit()


# Guardar tabla integrada.

write.csv(
  tabla_resultado,
  "tabla_completa_horizontes.csv",
  row.names = FALSE
)


# ==========================================================
# 7. PARTICIÓN TEMPORAL TRAIN / TEST
# ==========================================================

n <- nrow(
  tabla_resultado
)


# 80 % inicial para entrenamiento.

train_idx <- 1:round(
  0.8 * n
)


train_data <- tabla_resultado[
  train_idx,
]


# 20 % final para evaluación.

test_data <- tabla_resultado[
  -train_idx,
]


# ==========================================================
# 8. FUNCIÓN PARA CALCULAR MÉTRICAS
# ==========================================================

metricas_horizonte <- function(
    modelo,
    train,
    test,
    variable_objetivo
) {
  
  
  # ------------------------------------------
  # Predicciones
  # ------------------------------------------
  
  pred_train <- predict(
    modelo,
    newdata = train
  )
  
  
  pred_test <- predict(
    modelo,
    newdata = test
  )
  
  
  # ------------------------------------------
  # Valores reales
  # ------------------------------------------
  
  real_train <- train[
    [variable_objetivo]
  ]
  
  real_test <- test[
    [variable_objetivo]
  ]
  
  
  # ------------------------------------------
  # R² en entrenamiento
  # ------------------------------------------
  
  R2_train <- 1 -
    
    sum(
      (real_train - pred_train)^2
    ) /
    
    sum(
      (
        real_train -
          mean(real_train)
      )^2
    )
  
  
  # ------------------------------------------
  # R² en test
  # ------------------------------------------
  
  R2_test <- 1 -
    
    sum(
      (real_test - pred_test)^2
    ) /
    
    sum(
      (
        real_test -
          mean(real_test)
      )^2
    )
  
  
  # ------------------------------------------
  # RMSE
  # ------------------------------------------
  
  RMSE <- sqrt(
    mean(
      (
        real_test -
          pred_test
      )^2
    )
  )
  
  
  # ------------------------------------------
  # MAE
  # ------------------------------------------
  
  MAE <- mean(
    abs(
      real_test -
        pred_test
    )
  )
  
  
  # ------------------------------------------
  # Correlación observado-predicho
  # ------------------------------------------
  
  Correlacion <- cor(
    real_test,
    pred_test
  )
  
  
  data.frame(
    
    R2_train = R2_train,
    
    R2_test = R2_test,
    
    RMSE = RMSE,
    
    MAE = MAE,
    
    Correlacion = Correlacion
  )
}


# ==========================================================
# 9. MODELOS CON PRECIO ACTUAL DEL BRENT
# ==========================================================

modelo_t1_con_brent <- lm(
  
  brent_t1 ~
    brent +
    vix +
    oro +
    dolar +
    fear_suavizado,
  
  data = train_data
)


modelo_t3_con_brent <- lm(
  
  brent_t3 ~
    brent +
    vix +
    oro +
    dolar +
    fear_suavizado,
  
  data = train_data
)


modelo_t5_con_brent <- lm(
  
  brent_t5 ~
    brent +
    vix +
    oro +
    dolar +
    fear_suavizado,
  
  data = train_data
)


modelo_t7_con_brent <- lm(
  
  brent_t7 ~
    brent +
    vix +
    oro +
    dolar +
    fear_suavizado,
  
  data = train_data
)


# ==========================================================
# 10. MODELOS SIN FEAR
# ==========================================================

modelo_t1_sin_fear <- lm(
  
  brent_t1 ~
    brent +
    vix +
    oro +
    dolar,
  
  data = train_data
)


modelo_t3_sin_fear <- lm(
  
  brent_t3 ~
    brent +
    vix +
    oro +
    dolar,
  
  data = train_data
)


modelo_t5_sin_fear <- lm(
  
  brent_t5 ~
    brent +
    vix +
    oro +
    dolar,
  
  data = train_data
)


modelo_t7_sin_fear <- lm(
  
  brent_t7 ~
    brent +
    vix +
    oro +
    dolar,
  
  data = train_data
)


# ==========================================================
# 11. MODELOS SIN PRECIO ACTUAL DEL BRENT
# ==========================================================

modelo_t1_sin_brent <- lm(
  
  brent_t1 ~
    vix +
    oro +
    dolar +
    fear_suavizado,
  
  data = train_data
)


modelo_t3_sin_brent <- lm(
  
  brent_t3 ~
    vix +
    oro +
    dolar +
    fear_suavizado,
  
  data = train_data
)


modelo_t5_sin_brent <- lm(
  
  brent_t5 ~
    vix +
    oro +
    dolar +
    fear_suavizado,
  
  data = train_data
)


modelo_t7_sin_brent <- lm(
  
  brent_t7 ~
    vix +
    oro +
    dolar +
    fear_suavizado,
  
  data = train_data
)


# ==========================================================
# 12. CÁLCULO DE MÉTRICAS
# ==========================================================


# ------------------------------------------
# Modelos con Brent actual
# ------------------------------------------

met_t1_con <- metricas_horizonte(
  modelo_t1_con_brent,
  train_data,
  test_data,
  "brent_t1"
)


met_t3_con <- metricas_horizonte(
  modelo_t3_con_brent,
  train_data,
  test_data,
  "brent_t3"
)


met_t5_con <- metricas_horizonte(
  modelo_t5_con_brent,
  train_data,
  test_data,
  "brent_t5"
)


met_t7_con <- metricas_horizonte(
  modelo_t7_con_brent,
  train_data,
  test_data,
  "brent_t7"
)


# ------------------------------------------
# Modelos sin fear
# ------------------------------------------

met_t1_sin_fear <- metricas_horizonte(
  modelo_t1_sin_fear,
  train_data,
  test_data,
  "brent_t1"
)


met_t3_sin_fear <- metricas_horizonte(
  modelo_t3_sin_fear,
  train_data,
  test_data,
  "brent_t3"
)


met_t5_sin_fear <- metricas_horizonte(
  modelo_t5_sin_fear,
  train_data,
  test_data,
  "brent_t5"
)


met_t7_sin_fear <- metricas_horizonte(
  modelo_t7_sin_fear,
  train_data,
  test_data,
  "brent_t7"
)


# ------------------------------------------
# Modelos sin Brent actual
# ------------------------------------------

met_t1_sin <- metricas_horizonte(
  modelo_t1_sin_brent,
  train_data,
  test_data,
  "brent_t1"
)


met_t3_sin <- metricas_horizonte(
  modelo_t3_sin_brent,
  train_data,
  test_data,
  "brent_t3"
)


met_t5_sin <- metricas_horizonte(
  modelo_t5_sin_brent,
  train_data,
  test_data,
  "brent_t5"
)


met_t7_sin <- metricas_horizonte(
  modelo_t7_sin_brent,
  train_data,
  test_data,
  "brent_t7"
)


# ==========================================================
# 13. TABLA COMPARATIVA FINAL
# ==========================================================

tabla_horizontes <- bind_rows(
  
  cbind(
    Horizonte = 1,
    Modelo = "Con Brent actual",
    met_t1_con
  ),
  
  cbind(
    Horizonte = 3,
    Modelo = "Con Brent actual",
    met_t3_con
  ),
  
  cbind(
    Horizonte = 5,
    Modelo = "Con Brent actual",
    met_t5_con
  ),
  
  cbind(
    Horizonte = 7,
    Modelo = "Con Brent actual",
    met_t7_con
  ),
  
  cbind(
    Horizonte = 1,
    Modelo = "Sin Brent actual",
    met_t1_sin
  ),
  
  cbind(
    Horizonte = 3,
    Modelo = "Sin Brent actual",
    met_t3_sin
  ),
  
  cbind(
    Horizonte = 5,
    Modelo = "Sin Brent actual",
    met_t5_sin
  ),
  
  cbind(
    Horizonte = 7,
    Modelo = "Sin Brent actual",
    met_t7_sin
  )
)


print(
  tabla_horizontes
)


write.csv(
  tabla_horizontes,
  "metricas_horizontes_regresion.csv",
  row.names = FALSE
)


# ==========================================================
# 14. COMPARACIÓN DEL APORTE DE FEAR
# ==========================================================

tabla_fear <- bind_rows(
  
  cbind(
    Horizonte = 1,
    Fear = "Con fear",
    met_t1_con
  ),
  
  cbind(
    Horizonte = 1,
    Fear = "Sin fear",
    met_t1_sin_fear
  ),
  
  cbind(
    Horizonte = 3,
    Fear = "Con fear",
    met_t3_con
  ),
  
  cbind(
    Horizonte = 3,
    Fear = "Sin fear",
    met_t3_sin_fear
  ),
  
  cbind(
    Horizonte = 5,
    Fear = "Con fear",
    met_t5_con
  ),
  
  cbind(
    Horizonte = 5,
    Fear = "Sin fear",
    met_t5_sin_fear
  ),
  
  cbind(
    Horizonte = 7,
    Fear = "Con fear",
    met_t7_con
  ),
  
  cbind(
    Horizonte = 7,
    Fear = "Sin fear",
    met_t7_sin_fear
  )
)


print(
  tabla_fear
)


write.csv(
  tabla_fear,
  "metricas_aporte_fear.csv",
  row.names = FALSE
)


# ==========================================================
# 15. COEFICIENTE Y SIGNIFICANCIA DE FEAR
# ==========================================================

extraer_fear <- function(
    modelo,
    horizonte
) {
  
  coeficientes <- summary(
    modelo
  )$coefficients
  
  
  data.frame(
    
    Horizonte = horizonte,
    
    Coeficiente_fear =
      coeficientes[
        "fear_suavizado",
        "Estimate"
      ],
    
    Error_estandar =
      coeficientes[
        "fear_suavizado",
        "Std. Error"
      ],
    
    t_value =
      coeficientes[
        "fear_suavizado",
        "t value"
      ],
    
    p_value =
      coeficientes[
        "fear_suavizado",
        "Pr(>|t|)"
      ]
  )
}


tabla_coeficientes_fear <- bind_rows(
  
  extraer_fear(
    modelo_t1_con_brent,
    1
  ),
  
  extraer_fear(
    modelo_t3_con_brent,
    3
  ),
  
  extraer_fear(
    modelo_t5_con_brent,
    5
  ),
  
  extraer_fear(
    modelo_t7_con_brent,
    7
  )
)


print(
  tabla_coeficientes_fear
)


write.csv(
  tabla_coeficientes_fear,
  "coeficientes_fear.csv",
  row.names = FALSE
)


# ==========================================================
# 16. COMPARACIÓN DE R² SEGÚN HORIZONTE
# ==========================================================

ggplot(
  tabla_horizontes,
  aes(
    x = Horizonte,
    y = R2_test,
    group = Modelo,
    color = Modelo
  )
) +
  
  geom_line(
    linewidth = 1
  ) +
  
  geom_point(
    size = 3
  ) +
  
  scale_x_continuous(
    breaks = c(
      1,
      3,
      5,
      7
    )
  ) +
  
  labs(
    
    title =
      "Capacidad predictiva según horizonte temporal",
    
    subtitle =
      "Regresión lineal múltiple",
    
    x =
      "Horizonte de predicción (días)",
    
    y =
      "R² en datos de test",
    
    color =
      "Modelo"
  ) +
  
  theme_minimal()


# ==========================================================
# 17. PRECIO REAL VS. PREDICHO
# ==========================================================
# Modelos con Brent actual y sin fear.


pred_t1 <- predict(
  modelo_t1_sin_fear,
  newdata = test_data
)


pred_t3 <- predict(
  modelo_t3_sin_fear,
  newdata = test_data
)


pred_t5 <- predict(
  modelo_t5_sin_fear,
  newdata = test_data
)


pred_t7 <- predict(
  modelo_t7_sin_fear,
  newdata = test_data
)


# ==========================================================
# HORIZONTE T + 1
# ==========================================================

plot(
  
  test_data$fecha_t1,
  
  test_data$brent_t1,
  
  type = "l",
  
  lwd = 2,
  
  col = "black",
  
  xlab = "Fecha",
  
  ylab = "Precio Brent",
  
  main =
    "Predicción del Brent - t + 1"
)


lines(
  
  test_data$fecha_t1,
  
  pred_t1,
  
  col = "orange",
  
  lwd = 2
)


legend(
  
  "topleft",
  
  legend = c(
    "Observado",
    "Predicho"
  ),
  
  col = c(
    "black",
    "orange"
  ),
  
  lwd = 2,
  
  bty = "n"
)


# ==========================================================
# HORIZONTE T + 3
# ==========================================================

plot(
  
  test_data$fecha_t3,
  
  test_data$brent_t3,
  
  type = "l",
  
  lwd = 2,
  
  col = "black",
  
  xlab = "Fecha",
  
  ylab = "Precio Brent",
  
  main =
    "Predicción del Brent - t + 3"
)


lines(
  
  test_data$fecha_t3,
  
  pred_t3,
  
  col = "green",
  
  lwd = 2
)


legend(
  
  "topleft",
  
  legend = c(
    "Observado",
    "Predicho"
  ),
  
  col = c(
    "black",
    "green"
  ),
  
  lwd = 2,
  
  bty = "n"
)


# ==========================================================
# HORIZONTE T + 5
# ==========================================================

plot(
  
  test_data$fecha_t5,
  
  test_data$brent_t5,
  
  type = "l",
  
  lwd = 2,
  
  col = "black",
  
  xlab = "Fecha",
  
  ylab = "Precio Brent",
  
  main =
    "Predicción del Brent - t + 5"
)


lines(
  
  test_data$fecha_t5,
  
  pred_t5,
  
  col = "pink",
  
  lwd = 2
)


legend(
  
  "topleft",
  
  legend = c(
    "Observado",
    "Predicho"
  ),
  
  col = c(
    "black",
    "pink"
  ),
  
  lwd = 2,
  
  bty = "n"
)


# ==========================================================
# HORIZONTE T + 7
# ==========================================================

plot(
  
  test_data$fecha_t7,
  
  test_data$brent_t7,
  
  type = "l",
  
  lwd = 2,
  
  col = "black",
  
  xlab = "Fecha",
  
  ylab = "Precio Brent",
  
  main =
    "Predicción del Brent - t + 7"
)


lines(
  
  test_data$fecha_t7,
  
  pred_t7,
  
  col = "blue",
  
  lwd = 2
)


legend(
  
  "topleft",
  
  legend = c(
    "Observado",
    "Predicho"
  ),
  
  col = c(
    "black",
    "blue"
  ),
  
  lwd = 2,
  
  bty = "n"
)


# ==========================================================
# FIN DEL ANÁLISIS
# ==========================================================