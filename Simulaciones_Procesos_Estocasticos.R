# Cargamos las librerías necesarias
library(ggplot2)
library(tidyr)

simular_rw <- function(T_periods = 720, 
                       N_series = 3, 
                       phi_0 = 0, 
                       y_0 = 0,      # <--- Este es el valor inicial
                       sigma = 4.130375,
                       titulo = "Simulación de Random Walks",
                       eje_x = "Tiempo (t)",
                       eje_y = expression(y[t])) {
  
  # 1. Simulación de los shocks (errores epsilon_t con distribución Normal)
  shocks <- matrix(rnorm(T_periods * N_series, mean = 0, sd = sigma), 
                   nrow = T_periods, ncol = N_series)
  
  # 2. Construcción de los Random Walks
  cambios <- shocks + phi_0
  rw_paths <- apply(cambios, 2, cumsum)
  rw_paths <- rw_paths + y_0
  rw_paths <- rbind(rep(y_0, N_series), rw_paths)
  
  # 3. Suavizado (Interpolación para líneas curvas)
  t_vec <- 0:T_periods
  # Creamos un vector de tiempo con 200 puntos para que la curva fluya sin cortes
  t_suave <- seq(0, T_periods, length.out = 200) 
  
  # Interpolamos las series simuladas usando splines (pasan exactamente por tus puntos)
  rw_paths_suave <- apply(rw_paths, 2, function(col) spline(t_vec, col, xout = t_suave)$y)
  
  # Calculamos la media y los intervalos exactos sobre el tiempo continuo
  df_ci <- data.frame(
    t = t_suave,
    media = y_0 + phi_0 * t_suave,
    sup = (y_0 + phi_0 * t_suave) + 1.96 * sigma * sqrt(t_suave),
    inf = (y_0 + phi_0 * t_suave) - 1.96 * sigma * sqrt(t_suave)
  )
  
  # 4. Preparación de datos para graficar
  df_paths <- as.data.frame(rw_paths_suave)
  df_paths$t <- t_suave
  df_largo <- pivot_longer(df_paths, cols = -t, names_to = "serie", values_to = "y")
  
  # Definimos el color base para todo el gráfico
  color_azul <- "steelblue"
  
  # 5. Gráfico
  g <- ggplot() +
    # Trayectorias simuladas suavizadas (alta transparencia)
    geom_line(data = df_largo, aes(x = t, y = y, group = serie), 
              color = color_azul, alpha = 0.2) +
    # Media teórica suavizada (continua, opaca y un poco más gruesa)
    geom_line(data = df_ci, aes(x = t, y = media), 
              color = color_azul, alpha = 1, linewidth = 0.8) +
    # Límites del Intervalo de Confianza 95% suavizados (continuas y opacas)
    geom_line(data = df_ci, aes(x = t, y = sup), 
              color = color_azul, alpha = 1, linewidth = 0.8) +
    geom_line(data = df_ci, aes(x = t, y = inf), 
              color = color_azul, alpha = 1, linewidth = 0.8) +
    theme_minimal() +
    labs(title = titulo, x = eje_x, y = eje_y)
  
  return(g)
}

# === EJECUCIÓN DE LOS 3 ESCENARIOS CAMBIANDO EL VALOR INICIAL ===

# Escenario 1: Sin Drift (Empezando en un precio de 50)
grafico_sin_drift <- simular_rw(phi_0 = 0, 
                                y_0 = 100,    # Nuevo valor inicial
                                titulo = "Random Walk Puro",
                                eje_x = "Meses", 
                                eje_y = "Variable")
print(grafico_sin_drift)

# Escenario 2: Con Drift Positivo (Empezando en un índice base de 100)
grafico_drift_pos <- simular_rw(phi_0 = 0.34667, 
                                y_0 = 50,   # Nuevo valor inicial
                                titulo = "Random Walk con tendencia positiva",
                                eje_x = "Meses", 
                                eje_y = "Variable")
print(grafico_drift_pos)

# Escenario 3: Con Drift Negativo (Empezando en 1000 reservas)
grafico_drift_neg <- simular_rw(phi_0 = -0.5, 
                                y_0 = 100,  # Nuevo valor inicial
                                titulo = "Random Walk con tendencia negativa",
                                eje_x = "Meses", 
                                eje_y = "Variable")
print(grafico_drift_neg)

library(quantmod)
library(forecast)
library(lmtest)

acciones <- c("AAPL", "MSFT", "NVDA")

getSymbols(acciones)

NVDA <- NVDA$NVDA.Adjusted["2024/"]

summary(NVDA)

data <- data.frame(NVDA = NVDA)

model_arima <- Arima(data$NVDA, order = c(0,1,0), include.drift = TRUE)
model_arima

coeftest(model_arima)
  
drift_estimado <- coef(model_arima)
sigma_estimado <- (model_arima$sigma2)^(1/2)


library(ggplot2)
library(tidyr)
library(quantmod)

simular_rw <- function(T_periods = NULL, 
                       N_series = 3, 
                       phi_0 = 0, 
                       y_0 = 0,      
                       sigma = 1,
                       fechas = NULL,             # NUEVO: Vector de fechas
                       serie_real = NULL,         # NUEVO: Datos reales para superponer
                       mostrar_ci = TRUE,         # NUEVO: Toggle para el Intervalo
                       mostrar_tendencia = TRUE,  # NUEVO: Toggle para la tendencia
                       titulo = "Simulación de Random Walks",
                       eje_x = "Tiempo",
                       eje_y = expression(y[t])) {
  
  # Si se entregan fechas, calculamos los periodos automáticamente
  if (!is.null(fechas)) {
    T_periods <- length(fechas) - 1
  } else if (is.null(T_periods)) {
    T_periods <- 720
    fechas <- 0:T_periods # Vector numérico por defecto si no hay fechas
  }
  
  # 1. Simulación de los shocks (errores epsilon_t con distribución Normal)
  shocks <- matrix(rnorm(T_periods * N_series, mean = 0, sd = sigma), 
                   nrow = T_periods, ncol = N_series)
  
  # 2. Construcción de los Random Walks
  cambios <- shocks + phi_0
  rw_paths <- apply(cambios, 2, cumsum)
  rw_paths <- rw_paths + y_0
  rw_paths <- rbind(rep(y_0, N_series), rw_paths)
  
  # Creamos un vector de tiempo numérico para la fórmula matemática del CI
  t_num <- 0:T_periods 
  
  # 3. Datos para el Intervalo de Confianza y Tendencia
  # CORRECCIÓN AQUÍ: Se genera una secuencia lineal espaciada de fechas 
  # para evitar las ondulaciones causadas por los fines de semana.
  df_ci <- data.frame(
    t = seq(from = fechas[1], to = fechas[length(fechas)], length.out = length(fechas)),
    media = y_0 + phi_0 * t_num,
    sup = (y_0 + phi_0 * t_num) + 1.96 * sigma * sqrt(t_num),
    inf = (y_0 + phi_0 * t_num) - 1.96 * sigma * sqrt(t_num)
  )
  
  # 4. Preparación de datos simulados para graficar
  df_paths <- as.data.frame(rw_paths)
  df_paths$t <- fechas
  df_largo <- pivot_longer(df_paths, cols = -t, names_to = "serie", values_to = "y")
  
  color_azul <- "#2984D1"
  
  # 5. Construcción del Gráfico
  g <- ggplot() +
    # Trayectorias simuladas (alta transparencia)
    geom_line(data = df_largo, aes(x = t, y = y, group = serie), 
              color = color_azul, alpha = 0.25)
  
  # Capa opcional: Tendencia Teórica
  if (mostrar_tendencia) {
    g <- g + geom_line(data = df_ci, aes(x = t, y = media), 
                       color = "#2984D1", alpha = 1, linewidth = 0.8)
  }
  
  # Capa opcional: Intervalos de Confianza (95%)
  if (mostrar_ci) {
    g <- g + 
      geom_line(data = df_ci, aes(x = t, y = sup), 
                color = "#2984D1", alpha = 1, linewidth = 0.8) +
      geom_line(data = df_ci, aes(x = t, y = inf), 
                color = "#2984D1", alpha = 1, linewidth = 0.8)
  }
  
  # Capa opcional: Serie Real destacada
  if (!is.null(serie_real)) {
    # Nos aseguramos de que coincidan los largos
    df_real <- data.frame(t = fechas, y = as.numeric(serie_real))
    g <- g + geom_line(data = df_real, aes(x = t, y = y), 
                       color = "#2984D1", alpha = 1, linewidth = 0.9)
  }
  
  g <- g + theme_minimal() + labs(title = titulo, x = eje_x, y = eje_y)
  
  return(g)
}

# (Asumiendo que ya tienes tu objeto NVDA cargado previamente con quantmod)
# Extraemos la columna ajustada
nvda_adj <- NVDA$NVDA.Adjusted

# 2. Extraemos las fechas y los valores reales como vectores para la función
fechas_reales <- index(nvda_adj)
valores_reales <- as.numeric(nvda_adj)

# 3. ESTIMACIÓN DE COEFICIENTES (Random Walk con Drift)
# Calculamos la diferencia diaria del precio
diferencias_diarias <- diff(valores_reales)[-1] # [-1] quita el primer NA

# Estimamos parámetros basándonos en los datos reales:
precio_inicial <- valores_reales[1]         # Empezamos la simulación donde empezó el año

hist(log(valores_reales))

set.seed(123)
# 4. EJECUTAMOS LA SIMULACIÓN
simulacion_nvda <- simular_rw(
  N_series = 10, 
  phi_0 = 0,
  y_0 = precio_inicial,  
  sigma = sigma_estimado,
  fechas = fechas_reales,        
  serie_real = valores_reales,   
  mostrar_ci = TRUE,             
  mostrar_tendencia = TRUE,     
  titulo = "NVDA con Drift Igual a Cero",
  eje_x = "Fechas", 
  eje_y = "Precio"
)

# Imprimir gráfico
print(simulacion_nvda)

