# Cargamos las librerías necesarias
library(ggplot2)
library(tidyr)
library(usethis)

simular_rw <- function(T_periods = 24, 
                       N_series = 100, 
                       phi_0 = 0, 
                       y_0 = 0,      # <--- Este es el valor inicial
                       sigma = 1,
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
                                y_0 = 50,    # Nuevo valor inicial
                                titulo = "Random Walk Puro (Valor Inicial = 50)",
                                eje_x = "Meses", 
                                eje_y = "Precio")
print(grafico_sin_drift)

# Escenario 2: Con Drift Positivo (Empezando en un índice base de 100)
grafico_drift_pos <- simular_rw(phi_0 = 0.5, 
                                y_0 = 100,   # Nuevo valor inicial
                                titulo = "Random Walk Alcista (Valor Inicial = 100)",
                                eje_x = "Semanas", 
                                eje_y = "Índice de Precios")
print(grafico_drift_pos)

# Escenario 3: Con Drift Negativo (Empezando en 1000 reservas)
grafico_drift_neg <- simular_rw(phi_0 = -0.5, 
                                y_0 = 1000,  # Nuevo valor inicial
                                titulo = "Random Walk Bajista (Valor Inicial = 1000)",
                                eje_x = "Años", 
                                eje_y = "Nivel de Reservas")
print(grafico_drift_neg)

#### Modelo Exponencial

simular_exponencial <- function(T_periods = 24, 
                                N_series = 100, 
                                r = 0.05,      # Tasa de crecimiento por período (ej. 5%)
                                y_0 = 100,     # Valor inicial
                                sigma = 0.17,  # Volatilidad del shock
                                titulo = "Simulación: Tendencia Determinística con Shock Multiplicativo",
                                eje_x = "Tiempo (t)",
                                eje_y = expression(y[t])) {
  
  # 1. Vector de tiempo
  t_vec <- 0:T_periods
  
  # 2. Simulación de los shocks normales (epsilon_t)
  # A diferencia del random walk, aquí los shocks NO se acumulan (no hay cumsum).
  # Para t=0, forzamos el shock a 0 para que todas las series partan exactamente de y_0.
  shocks <- matrix(rnorm(T_periods * N_series, mean = 0, sd = sigma), 
                   nrow = T_periods, ncol = N_series)
  shocks <- rbind(rep(0, N_series), shocks) 
  
  # 3. Construcción del proceso estocástico: y_t = y_0 * (1+r)^t * exp(epsilon_t)
  tendencia <- y_0 * (1 + r)^t_vec
  # Replicamos la tendencia para multiplicarla matricialmente con los shocks
  tendencia_matriz <- matrix(rep(tendencia, N_series), ncol = N_series, byrow = FALSE)
  
  y_paths <- tendencia_matriz * exp(shocks)
  
  # 4. Intervalos de Confianza (95%) y Línea Central (Mediana)
  # Utilizamos la fórmula exacta deducida anteriormente
  limite_sup <- tendencia * exp(1.96 * sigma)
  limite_inf <- tendencia * exp(-1.96 * sigma)
  
  # 5. Preparación de datos para graficar
  df_paths <- as.data.frame(y_paths)
  df_paths$t <- t_vec
  df_largo <- pivot_longer(df_paths, cols = -t, names_to = "serie", values_to = "y")
  
  df_ci <- data.frame(
    t = t_vec, 
    tendencia = tendencia, 
    sup = limite_sup, 
    inf = limite_inf
  )
  
  # Definimos el color base a Verde
  color_verde <- "seagreen"
  
  # 6. Gráfico
  g <- ggplot() +
    # Trayectorias simuladas (alta transparencia)
    geom_line(data = df_largo, aes(x = t, y = y, group = serie), 
              color = color_verde, alpha = 0.2) +
    # Tendencia central determinística (continua, opaca y más gruesa)
    geom_line(data = df_ci, aes(x = t, y = tendencia), 
              color = color_verde, alpha = 1, linewidth = 0.8) +
    # Límites del Intervalo de Confianza 95% (continuas y opacas)
    geom_line(data = df_ci, aes(x = t, y = sup), 
              color = color_verde, alpha = 1, linewidth = 0.8) +
    geom_line(data = df_ci, aes(x = t, y = inf), 
              color = color_verde, alpha = 1, linewidth = 0.8) +
    theme_minimal() +
    labs(title = titulo, x = eje_x, y = eje_y)
  
  return(g)
}

# === EJECUCIÓN ===

# Simulamos el proceso asumiendo un crecimiento del 5% (r=0.05),
# un valor inicial de 100, y una volatilidad de 0.15
grafico_final <- simular_exponencial(r = 0.05, 
                                     y_0 = 100, 
                                     sigma = 0.17,
                                     titulo = "Proceso Estocástico: y_t = y_0(1+r)^t e^{epsilon_t}",
                                     eje_x = "Meses",
                                     eje_y = "Precio")

print(grafico_final)