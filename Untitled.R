# CASO 3 - Marketing digital y demanda de alquiler de vehículos
# Business Analytics 2026-3 | Pontificia Universidad Javeriana
#Integrantes: Valerie Beltrán, Juanita Heredia, Mariana Ávila


# Paquetes
paquetes <- c("readr", "dplyr", "tidyr", "lubridate", "stringr", "gtrendsR")
nuevos <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(nuevos) > 0) install.packages(nuevos)
invisible(lapply(paquetes, library, character.only = TRUE))

ruta_raw <- file.choose()
dir.create("datos", recursive = TRUE, showWarnings = FALSE)


# PUNTO 1. RECONOCIMIENTO DE LA BASE

# Se importa sin alterar el original: 'raw' nunca se modifica, se trabaja en 'datos'
raw   <- read_csv(ruta_raw, show_col_types = FALSE)
datos <- raw

n_antes <- nrow(datos)


cat("Filas:", nrow(datos), "| Columnas:", ncol(datos), "\n")
glimpse(datos)

# 1.1 Fecha semanal y qué representa una fila
datos$Week <- as.Date(datos$Week)
cat("Rango de fechas:", format(min(datos$Week)), "a", format(max(datos$Week)), "\n")
cat("Día de la semana de 'Week':", unique(weekdays(datos$Week)), "\n")
cat("Diferencias entre semanas consecutivas (días):",
    paste(unique(as.numeric(diff(datos$Week))), collapse = ", "), "\n")
# Una fila = una semana (observación semanal agregada a nivel empresa)


# 1.2 Variables de respuesta y columnas de publicidad
vars_respuesta <- c("Sales", "Orders")
cols_cost <- names(datos)[str_detect(names(datos), "\\.Cost$")]
cols_imp  <- names(datos)[str_detect(names(datos), "\\.Impression$")]
cols_macro <- c("SP500", "WEI")
cat("Columnas de costo:", length(cols_cost), "| de impresiones:", length(cols_imp), "\n")


# 1.3 Calidad de datos: tipos, faltantes, duplicados, rangos
cat("\nTipos de dato:\n"); print(sapply(datos, class))

cat("\nValores faltantes por columna (solo las que tienen):\n")
na_por_col <- colSums(is.na(datos)); print(na_por_col[na_por_col > 0])
cat("Total de NA:", sum(na_por_col), "\n")

cat("\nFilas duplicadas:", sum(duplicated(datos)),
    "| Semanas duplicadas:", sum(duplicated(datos$Week)), "\n")
cat("Semanas faltantes en la secuencia:",
    length(setdiff(as.character(seq(min(datos$Week), max(datos$Week), by = "week")),
                   as.character(datos$Week))), "\n")

cat("\nValores negativos en costos/impresiones/KPIs:",
    sum(datos[, c(vars_respuesta, cols_cost, cols_imp)] < 0, na.rm = TRUE), "\n")
cat("Sales <= 0:", sum(datos$Sales <= 0), "| Orders <= 0:", sum(datos$Orders <= 0), "\n")

cat("\nResumen de las variables de respuesta y controles:\n")
print(summary(datos[, c(vars_respuesta, cols_macro)]))

# Columnas con muchos ceros (campañas que no existían en toda la muestra)
ceros <- sapply(datos[, c(cols_cost, cols_imp)], function(x) sum(x == 0))
cat("\nColumnas con ceros (campañas que arrancan después / pausas):\n")
print(ceros[ceros > 0])

primer_gasto <- sapply(datos[, cols_cost], function(x) {
  i <- which(x > 0)[1]; if (is.na(i)) NA else format(datos$Week[i])
})
cat("\nPrimera semana con costo > 0 por campaña:\n"); print(primer_gasto)

# Hallazgo importante: relación entre Sales y Orders
ratio <- datos$Sales / datos$Orders
cat("\nSales/Orders -> mín:", round(min(ratio), 4), "| máx:", round(max(ratio), 4),
    "| correlación Sales-Orders:", round(cor(datos$Sales, datos$Orders), 6), "\n")
# Si el ratio es constante, Orders es un reescalamiento exacto de Sales
# (misma información). Hay que mencionarlo en el informe: las regresiones de
# Sales y Orders darán los mismos t, p, R2; solo cambia la escala de los coeficientes.

# Outliers (regla IQR) en Sales, solo para describir (NO se eliminan)
q <- quantile(datos$Sales, c(.25, .75)); lim <- q[2] + 1.5 * diff(q)
cat("\nSemanas con Sales > Q3 + 1.5*IQR (picos de temporada alta):\n")
print(datos[datos$Sales > lim, c("Week", "Sales")], n = 30)

# Registro de ajustes: solo la conversión de Week a Date (no se eliminan filas)
cat("\nAjustes realizados: conversión de Week a Date. Filas antes/después:",
    nrow(raw), "/", nrow(datos), "\n")




# PUNTO 2. CANALES AGREGADOS DE AMAZON
# Prefijos exactos: DSP, Search, Sponsored (las columnas Google quedan fuera porque sus nombres empiezan por "Google")

suma_prefijo <- function(df, prefijo, sufijo) {
  cols <- names(df)[str_detect(names(df), paste0("^", prefijo)) &
                      str_detect(names(df), paste0("\\.", sufijo, "$"))]
  list(cols = cols, suma = rowSums(df[, cols], na.rm = FALSE))
}

canales <- c(DSP = "Amazon_DSP", Search = "Amazon_Search", Sponsored = "Amazon_Sponsored")

for (pref in names(canales)) {
  c_cost <- suma_prefijo(datos, pref, "Cost")
  c_imp  <- suma_prefijo(datos, pref, "Impression")
  datos[[paste0(canales[[pref]], "_Cost")]]        <- c_cost$suma
  datos[[paste0(canales[[pref]], "_Impressions")]] <- c_imp$suma
  cat("\n", canales[[pref]], "\n  Costo suma:", paste(c_cost$cols, collapse = " + "),
      "\n  Impresiones suma:", paste(c_imp$cols, collapse = " + "), "\n")
}

# Verificación de que ninguna columna Google entró a los agregados
stopifnot(!any(str_detect(
  c(suma_prefijo(datos, "DSP", "Cost")$cols,
    suma_prefijo(datos, "Search", "Cost")$cols,
    suma_prefijo(datos, "Sponsored", "Cost")$cols), "^Google")))

# Comprobación manual en dos semanas
semanas_check <- as.Date(c("2024-11-24", "2025-12-07"))
for (s in as.character(semanas_check)) {
  f <- datos[datos$Week == as.Date(s), ]
  cat("\n--- Semana", s, "---\n")
  dsp  <- f[["DSP - Evergreen.Cost"]] + f[["DSP - Seasonal.Cost"]]
  srch <- f[["Search - Men - Brand.Cost"]] + f[["Search - Men - Non-Brand.Cost"]] +
    f[["Search - Women - Brand.Cost"]] + f[["Search - Women - Non-Brand.Cost"]]
  spon <- f[["Sponsored Brands-Paid Search.Cost"]] + f[["Sponsored Display-Non-Brand.Cost"]] +
    f[["Sponsored Products-Brand Search.Cost"]] + f[["Sponsored Products-NonBrand Search.Cost"]]
  cat("DSP manual:", round(dsp, 2), "| código:", round(f$Amazon_DSP_Cost, 2), "\n")
  cat("Search manual:", round(srch, 2), "| código:", round(f$Amazon_Search_Cost, 2), "\n")
  cat("Sponsored manual:", round(spon, 2), "| código:", round(f$Amazon_Sponsored_Cost, 2), "\n")
  stopifnot(isTRUE(all.equal(dsp, f$Amazon_DSP_Cost)),
            isTRUE(all.equal(srch, f$Amazon_Search_Cost)),
            isTRUE(all.equal(spon, f$Amazon_Sponsored_Cost)))
}
# Nota: Search arranca en 2024-09-08 (antes es 0)

# Las columnas de Google se conservan en 'datos' (no se eliminan)
write_csv(datos, "datos/base_amazon.csv")




# PUNTO 3. VARIABLES DE CONTROL ADICIONALES (mínimo 4 + SP500 y WEI)
# Plan: 2 series de Google Trends (Brand y Non-Brand) + 3 series FRED
#   - GASREGW : precio de gasolina regular (semanal)  -> costo de viajar/alquilar
#   - UNRATE  : tasa de desempleo (mensual)           -> condiciones del mercado laboral
#   - ICSA    : solicitudes iniciales de desempleo (semanal)


fecha_consulta <- Sys.Date()
inicio <- min(datos$Week)
fin    <- max(datos$Week)


# 3.1 Google Trends
brand_term    <- "Enterprise Rent-A-Car"
nonbrand_term <- "car rental"    


# Configuración documentada: país US, web search, categoría 0 (todas),
# se descargan SEPARADAMENTE (cada serie con su propio índice 0-100).
rango_trends <- paste(format(inicio), format(fin + 6))

bajar_trends <- function(termino) {
  g <- gtrends(keyword = termino, geo = "US", time = rango_trends,
               gprop = "web", category = 0)
  g$interest_over_time |>
    transmute(Week = as.Date(date),
              valor = as.numeric(ifelse(hits == "<1", 0.5, hits)))
}

trends_brand    <- bajar_trends(brand_term)    |> rename(Trends_Brand = valor)
Sys.sleep(5)  # evitar bloqueo por exceso de consultas
trends_nonbrand <- bajar_trends(nonbrand_term) |> rename(Trends_NonBrand = valor)

# Con rangos < 5 años Google Trends devuelve datos semanales (domingo a sábado,
# la fecha es el domingo de inicio), igual que 'Week' en la base.
write_csv(trends_brand,    "datos/trends_brand_raw.csv")
write_csv(trends_nonbrand, "datos/trends_nonbrand_raw.csv")


# 3.2 FRED
bajar_fred <- function(id) {
  url <- paste0("https://fred.stlouisfed.org/graph/fredgraph.csv?id=", id)
  archivo <- file.path("datos", paste0(id, ".csv"))
  if (!file.exists(archivo)) {
    ok <- tryCatch({ download.file(url, archivo, quiet = TRUE); TRUE },
                   error = function(e) FALSE)
    if (!ok) stop("No se pudo descargar ", id, ". Bájenla manualmente de ", url)
  }
  s <- read_csv(archivo, show_col_types = FALSE)
  names(s)[1] <- "fecha_obs"          # la columna de fecha se llama DATE u observation_date
  names(s)[2] <- "valor"
  s |> mutate(fecha_obs = as.Date(fecha_obs), valor = suppressWarnings(as.numeric(valor))) |>
    filter(!is.na(valor))
}

# Fecha desde la cual el dato era conocido públicamente
#   GASREGW: publica el lunes-miércoles siguiente -> +2 días sobre la fecha del dato
#   ICSA   : semana termina sábado, se publica jueves -> +5 días
#   UNRATE : dato del mes M se publica el primer viernes de M+1 -> aprox. fin de mes + 7 días
disponibilidad <- list(
  GASREGW = function(d) d + 2,
  ICSA    = function(d) d + 5,
  UNRATE  = function(d) ceiling_date(d, "month") + 6
)

alinear <- function(base, serie, nombre, fun_disp) {
  s <- serie |> mutate(disponible = fun_disp(fecha_obs)) |> arrange(disponible)
  # Para cada Week, se toma el último dato ya publicado en esa fecha
  out <- base |> select(Week) |>
    left_join(s, by = join_by(closest(Week >= disponible)))
  stopifnot(all(out$disponible <= out$Week, na.rm = TRUE))   # sin información futura
  out |> transmute(Week, !!nombre := valor)
}

fred_ids <- c(GASREGW = "GASREGW", UNRATE = "UNRATE", ICSA = "ICSA")
fred_series <- lapply(names(fred_ids), function(id) {
  alinear(datos, bajar_fred(id), fred_ids[[id]], disponibilidad[[id]])
})


# 3.3 Merge a la base ampliada
base <- datos |>
  left_join(trends_brand,    by = "Week") |>
  left_join(trends_nonbrand, by = "Week")
for (s in fred_series) base <- left_join(base, s, by = "Week")

# Verificaciones: el merge no debe reducir ni duplicar la muestra
stopifnot(nrow(base) == n_antes, !any(duplicated(base$Week)))
nuevas <- c("Trends_Brand", "Trends_NonBrand", unname(fred_ids))
cat("\nNA en variables nuevas tras el merge:\n"); print(colSums(is.na(base[, nuevas])))




if (sum(is.na(base[, nuevas])) > 0)
  warning("Hay NA en las variables nuevas: revisar antes de pasar la base a los puntos 4-6")

write_csv(base, "datos/base_ampliada.csv")




# 3.4 Tabla de trazabilidad de las series nuevas
trazabilidad <- tibble::tribble(
  ~variable, ~concepto, ~termino_o_codigo, ~url, ~entidad, ~cobertura, ~frecuencia_original, ~agregacion_alineacion,
  "Trends_Brand", "Interés de búsqueda de marca (índice relativo 0-100)", brand_term,
  "https://trends.google.com/trends/", "Google", "Estados Unidos", "Semanal (dom-sáb)",
  "Se une por fecha exacta de inicio de semana",
  "Trends_NonBrand", "Interés de búsqueda genérica de alquiler de vehículos", nonbrand_term,
  "https://trends.google.com/trends/", "Google", "Estados Unidos", "Semanal (dom-sáb)",
  "Se une por fecha exacta de inicio de semana",
  "GASREGW", "Precio gasolina regular (USD/galón)", "GASREGW",
  "https://fred.stlouisfed.org/series/GASREGW", "U.S. EIA vía FRED", "Estados Unidos", "Semanal",
  "Último valor publicado a la fecha de Week (rolling join)",
  "UNRATE", "Tasa de desempleo (%)", "UNRATE",
  "https://fred.stlouisfed.org/series/UNRATE", "U.S. BLS vía FRED", "Estados Unidos", "Mensual",
  "Valor del mes ya publicado a la fecha de Week (rezago de publicación)",
  "ICSA", "Solicitudes iniciales de desempleo", "ICSA",
  "https://fred.stlouisfed.org/series/ICSA", "U.S. DOL vía FRED", "Estados Unidos", "Semanal",
  "Último valor publicado a la fecha de Week (rolling join)"
) |> mutate(fecha_consulta = format(fecha_consulta),
            config_trends = ifelse(str_detect(variable, "Trends"),
                                   paste0("geo=US; rango=", rango_trends, "; categoría=0 (todas); tipo=web; descarga separada"),
                                   NA_character_))

print(trazabilidad, width = Inf)
write_csv(trazabilidad, "datos/trazabilidad_variables_externas.csv")


# Puntos 4, 5 y 6 - Juanita Heredia


# Paquetes
paquetes <- c("readr", "dplyr", "tidyr", "ggplot2", "scales")
nuevos <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(nuevos) > 0) install.packages(nuevos)
invisible(lapply(paquetes, library, character.only = TRUE))

options(scipen = 999)
dir.create("resultados/figuras", recursive = TRUE, showWarnings = FALSE)

base <- read_csv("datos/base_ampliada.csv", show_col_types = FALSE) |>
  mutate(Week = as.Date(Week)) |>
  arrange(Week)

stopifnot(!any(duplicated(base$Week)))
cat("Semanas:", nrow(base), "| desde", format(min(base$Week)), "hasta", format(max(base$Week)), "\n")

# Costo total de Google (solo contexto, no entra a los agregados de Amazon)
cols_google_cost <- grep("^Google.*\\.Cost$", names(base), value = TRUE)
base$Google_Cost <- rowSums(base[, cols_google_cost])

canales_cost <- c("Amazon_DSP_Cost", "Amazon_Search_Cost", "Amazon_Sponsored_Cost")
canales_imp  <- c("Amazon_DSP_Impressions", "Amazon_Search_Impressions",
                  "Amazon_Sponsored_Impressions")
controles_externos <- c("Trends_Brand", "Trends_NonBrand", "GASREGW", "UNRATE", "ICSA")

# Noviembre y diciembre, solo para sombrear los gráficos
franjas <- tibble(inicio = as.Date(c("2024-11-01", "2025-11-01")),
                  fin    = as.Date(c("2024-12-31", "2025-12-31")))
base$Nov_Dic <- ifelse(format(base$Week, "%m") %in% c("11", "12"),
                       "Noviembre-diciembre", "Resto del año")




# PUNTO 4. EXPLORACIÓN Y ESTADÍSTICAS DESCRIPTIVAS

# 4.1 Tabla descriptiva
vars_desc <- c("Sales", "Orders", canales_cost, canales_imp,
               "SP500", "WEI", controles_externos)

tabla_desc <- base |>
  select(all_of(vars_desc)) |>
  pivot_longer(everything(), names_to = "Variable", values_to = "valor") |>
  group_by(Variable) |>
  summarise(N        = sum(!is.na(valor)),
            Media    = mean(valor, na.rm = TRUE),
            Desv_Est = sd(valor, na.rm = TRUE),
            Minimo   = min(valor, na.rm = TRUE),
            Maximo   = max(valor, na.rm = TRUE),
            .groups = "drop") |>
  mutate(Variable = factor(Variable, levels = vars_desc)) |>
  arrange(Variable)

print(as.data.frame(mutate(tabla_desc, across(where(is.numeric), ~ round(.x, 2)))))
write_csv(tabla_desc, "resultados/tabla_descriptiva.csv")
cat("Mediana de Sales:", median(base$Sales), "\n")


# 4.2 Series de tiempo de Sales y Orders
graf_kpi <- function(var, titulo, etiqueta_y) {
  ggplot(base, aes(x = Week, y = .data[[var]])) +
    geom_rect(data = franjas, aes(xmin = inicio, xmax = fin, ymin = -Inf, ymax = Inf),
              inherit.aes = FALSE, fill = "grey85", alpha = 0.6) +
    geom_line(color = "#1f4e79", linewidth = 0.8) +
    geom_point(color = "#1f4e79", size = 0.9) +
    scale_y_continuous(labels = label_comma(big.mark = ".", decimal.mark = ",")) +
    scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
    labs(title = titulo, subtitle = "Franjas grises: noviembre y diciembre",
         x = "Semana", y = etiqueta_y) +
    theme_minimal(base_size = 11) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

g_sales  <- graf_kpi("Sales",  "Ventas semanales (Sales)", "Sales")
g_orders <- graf_kpi("Orders", "Órdenes semanales (Orders)", "Órdenes")
print(g_sales); print(g_orders)
ggsave("resultados/figuras/4_serie_sales.png",  g_sales,  width = 9, height = 4.5, dpi = 300)
ggsave("resultados/figuras/4_serie_orders.png", g_orders, width = 9, height = 4.5, dpi = 300)

cat("\nSemanas con mayores ventas:\n")
print(as.data.frame(base |> arrange(desc(Sales)) |> select(Week, Sales, Orders) |> head(8)))
cat("\nSemanas con menores ventas:\n")
print(as.data.frame(base |> arrange(Sales) |> select(Week, Sales) |> head(3)))
cat("\nVentas por año:\n")
print(as.data.frame(base |> group_by(Anio = format(Week, "%Y")) |>
                      summarise(Semanas = n(), Media = mean(Sales), Mediana = median(Sales), Maximo = max(Sales))))
# 2026 solo llega hasta agosto


# 4.3 Costos e impresiones por canal
costos_largo <- base |>
  select(Week, all_of(canales_cost), Google_Cost) |>
  pivot_longer(-Week, names_to = "Canal", values_to = "Costo") |>
  mutate(Canal = factor(Canal,
                        levels = c(canales_cost, "Google_Cost"),
                        labels = c("Amazon DSP", "Amazon Search", "Amazon Sponsored", "Google (total)")))

g_costos <- ggplot(costos_largo, aes(x = Week, y = Costo, color = Canal)) +
  geom_rect(data = franjas, aes(xmin = inicio, xmax = fin, ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE, fill = "grey85", alpha = 0.6) +
  geom_line(linewidth = 0.7) +
  facet_wrap(~ Canal, ncol = 1, scales = "free_y") +
  scale_y_continuous(labels = label_comma(big.mark = ".", decimal.mark = ",")) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  labs(title = "Costo semanal por canal", x = "Semana", y = "Costo") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
print(g_costos)
ggsave("resultados/figuras/4_costos_por_canal.png", g_costos, width = 9, height = 9, dpi = 300)

g_costos_amazon <- costos_largo |>
  filter(Canal != "Google (total)") |>
  ggplot(aes(x = Week, y = Costo, color = Canal)) +
  geom_line(linewidth = 0.7) +
  scale_y_continuous(labels = label_comma(big.mark = ".", decimal.mark = ",")) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  labs(title = "Costo semanal de los canales de Amazon",
       x = "Semana", y = "Costo", color = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))
print(g_costos_amazon)
ggsave("resultados/figuras/4_costos_amazon.png", g_costos_amazon,
       width = 9, height = 4.5, dpi = 300)

g_impresiones <- base |>
  select(Week, all_of(canales_imp)) |>
  pivot_longer(-Week, names_to = "Canal", values_to = "Impresiones") |>
  mutate(Canal = factor(Canal, levels = canales_imp,
                        labels = c("Amazon DSP", "Amazon Search", "Amazon Sponsored"))) |>
  ggplot(aes(x = Week, y = Impresiones, color = Canal)) +
  geom_line(linewidth = 0.7) +
  facet_wrap(~ Canal, ncol = 1, scales = "free_y") +
  scale_y_continuous(labels = label_comma(big.mark = ".", decimal.mark = ",")) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  labs(title = "Impresiones semanales por canal de Amazon", x = "Semana", y = "Impresiones") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
print(g_impresiones)
ggsave("resultados/figuras/4_impresiones_por_canal.png", g_impresiones,
       width = 9, height = 7, dpi = 300)

cat("\nSemana de costo máximo por canal:\n")
for (x in canales_cost) cat(" ", x, ":", format(base$Week[which.max(base[[x]])]),
                            "->", round(max(base[[x]]), 2), "\n")
cat("Primera semana con costo en Search:",
    format(min(base$Week[base$Amazon_Search_Cost > 0])),
    "| en Google:", format(min(base$Week[base$Google_Cost > 0])), "\n")


# 4.4 Gráficos de dispersión
disp_largo <- base |>
  select(Week, Sales, Nov_Dic, all_of(canales_cost)) |>
  pivot_longer(all_of(canales_cost), names_to = "Canal", values_to = "Costo") |>
  mutate(Canal = factor(Canal, levels = canales_cost,
                        labels = c("Amazon DSP", "Amazon Search", "Amazon Sponsored")))

g_disp_canales <- ggplot(disp_largo, aes(x = Costo, y = Sales)) +
  geom_point(aes(color = Nov_Dic), alpha = 0.75) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, color = "black", linewidth = 0.6) +
  facet_wrap(~ Canal, scales = "free_x") +
  scale_x_continuous(labels = label_comma(big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(labels = label_comma(big.mark = ".", decimal.mark = ",")) +
  scale_color_manual(values = c("Noviembre-diciembre" = "#c0392b", "Resto del año" = "#1f4e79")) +
  labs(title = "Sales vs. costo de cada canal de Amazon",
       x = "Costo semanal", y = "Sales", color = NULL) +
  theme_minimal(base_size = 11) + theme(legend.position = "bottom")
print(g_disp_canales)
ggsave("resultados/figuras/4_dispersion_sales_canales.png", g_disp_canales,
       width = 10, height = 4.5, dpi = 300)

disp_controles <- base |>
  select(Sales, Nov_Dic, SP500, WEI, all_of(controles_externos)) |>
  pivot_longer(-c(Sales, Nov_Dic), names_to = "Control", values_to = "Valor")

g_disp_controles <- ggplot(disp_controles, aes(x = Valor, y = Sales)) +
  geom_point(aes(color = Nov_Dic), alpha = 0.75) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black", linewidth = 0.6) +
  facet_wrap(~ Control, scales = "free_x", ncol = 4) +
  scale_y_continuous(labels = label_comma(big.mark = ".", decimal.mark = ",")) +
  scale_color_manual(values = c("Noviembre-diciembre" = "#c0392b", "Resto del año" = "#1f4e79")) +
  labs(title = "Sales vs. variables de control", x = NULL, y = "Sales", color = NULL) +
  theme_minimal(base_size = 11) + theme(legend.position = "bottom")
print(g_disp_controles)
ggsave("resultados/figuras/4_dispersion_sales_controles.png", g_disp_controles,
       width = 11, height = 6, dpi = 300)

# Correlación con Sales sin las semanas de noviembre-diciembre
cat("\nCorrelación con Sales sin noviembre-diciembre:\n")
sin_pico <- base[base$Nov_Dic == "Resto del año", ]
print(round(sapply(canales_cost, function(x) cor(sin_pico[[x]], sin_pico$Sales)), 2))


# 4.5 Matrices de covarianzas y correlaciones
vars_matriz <- c("Sales", "Orders", canales_cost, canales_imp, "SP500", "WEI", controles_externos)
mat_cov <- cov(base[, vars_matriz])
mat_cor <- cor(base[, vars_matriz])

cov_sales <- tibble(Variable = colnames(mat_cov),
                    Covarianza_con_Sales  = mat_cov[, "Sales"],
                    Correlacion_con_Sales = mat_cor[, "Sales"]) |>
  filter(Variable != "Sales")
print(as.data.frame(mutate(cov_sales, across(where(is.numeric), ~ round(.x, 3)))))

write.csv(round(mat_cov, 2), "resultados/matriz_covarianzas.csv")
write.csv(round(mat_cor, 3), "resultados/matriz_correlaciones.csv")
write_csv(cov_sales, "resultados/covarianza_correlacion_con_sales.csv")
print(round(mat_cor, 2))

cor_largo <- as.data.frame(as.table(mat_cor))
names(cor_largo) <- c("Var1", "Var2", "Correlacion")
g_cor <- ggplot(cor_largo, aes(x = Var1, y = Var2, fill = Correlacion)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.2f", Correlacion)), size = 2.6) +
  scale_fill_gradient2(low = "#c0392b", mid = "white", high = "#1f4e79",
                       midpoint = 0, limits = c(-1, 1)) +
  labs(title = "Matriz de correlaciones", x = NULL, y = NULL, fill = "Correlación") +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(g_cor)
ggsave("resultados/figuras/4_matriz_correlaciones.png", g_cor, width = 10, height = 8, dpi = 300)


# PUNTO 5. REGRESIONES SIMPLES
# Sales = b0 + b1 * Costo del canal + e

resumen_lm <- function(m, nombre) {
  s  <- summary(m)
  co <- s$coefficients
  ci <- confint(m, level = 0.95)
  f  <- s$fstatistic
  tibble(Modelo      = nombre,
         Termino     = rownames(co),
         Coeficiente = co[, 1],
         Error_Est   = co[, 2],
         t           = co[, 3],
         p_valor     = co[, 4],
         IC95_inf    = ci[, 1],
         IC95_sup    = ci[, 2],
         R2          = s$r.squared,
         R2_ajustado = s$adj.r.squared,
         F           = unname(f["value"]),
         p_valor_F   = unname(pf(f["value"], f["numdf"], f["dendf"], lower.tail = FALSE)),
         N           = nobs(m))
}

modelos_simples <- lapply(canales_cost, function(x) lm(reformulate(x, response = "Sales"), data = base))
names(modelos_simples) <- canales_cost

for (x in canales_cost) {
  cat("\n--- Sales ~", x, "---\n")
  print(summary(modelos_simples[[x]]))
  print(confint(modelos_simples[[x]]))
}

tabla_simples <- bind_rows(lapply(canales_cost, function(x)
  resumen_lm(modelos_simples[[x]], paste("Sales ~", x))))
write_csv(tabla_simples, "resultados/punto5_regresiones_simples.csv")

print(as.data.frame(tabla_simples |> filter(Termino != "(Intercept)") |>
                      select(Termino, Coeficiente, IC95_inf, IC95_sup, t, p_valor, R2) |>
                      mutate(across(where(is.numeric), ~ round(.x, 4)))))

g_pend_simples <- tabla_simples |>
  filter(Termino != "(Intercept)") |>
  mutate(Canal = factor(Termino, levels = canales_cost,
                        labels = c("Amazon DSP", "Amazon Search", "Amazon Sponsored"))) |>
  ggplot(aes(x = Canal, y = Coeficiente, ymin = IC95_inf, ymax = IC95_sup)) +
  geom_pointrange(color = "#1f4e79") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Pendientes de las regresiones simples (IC 95%)", x = NULL, y = "b1") +
  theme_minimal(base_size = 11)
print(g_pend_simples)
ggsave("resultados/figuras/5_pendientes_simples.png", g_pend_simples, width = 7, height = 4.5, dpi = 300)




# PUNTO 6. REGRESIÓN MÚLTIPLE CON CONTROLES
# KPI = b0 + b1 DSP + b2 Search + b3 Sponsored + controles + e

# 6.1 Revisión de los controles candidatos
controles_cand <- c("Trends_Brand", "Trends_NonBrand", "GASREGW", "UNRATE", "ICSA", "SP500", "WEI")
print(round(cor(base[, controles_cand]), 2))

# VIF = 1 / (1 - R2) de regresar cada variable contra las demás
calc_vif <- function(df) {
  sapply(names(df), function(v) {
    otras <- setdiff(names(df), v)
    r2 <- summary(lm(reformulate(otras, response = v), data = df))$r.squared
    1 / (1 - r2)
  })
}

# Modelo con todos los controles
m_todos <- lm(reformulate(c(canales_cost, controles_cand), response = "Sales"), data = base)
print(summary(m_todos))
print(round(calc_vif(base[, c(canales_cost, controles_cand)]), 2))

# Controles finales:
# Trends_Brand sale (corr. 0.80 con Trends_NonBrand y puede depender de la pauta)
# UNRATE sale (mensual, casi no varía; se deja ICSA que es semanal)
# SP500 sale (se deja WEI; SP500 sube el VIF de Search sin mejorar el R2 ajustado)
controles_finales <- c("Trends_NonBrand", "GASREGW", "ICSA", "WEI")

vif_final <- calc_vif(base[, c(canales_cost, controles_finales)])
print(round(vif_final, 2))
write_csv(tibble(Variable = names(vif_final), VIF = vif_final), "resultados/punto6_vif_modelo_final.csv")


# 6.2 Modelos finales para Sales y Orders
formula_sales  <- reformulate(c(canales_cost, controles_finales), response = "Sales")
formula_orders <- reformulate(c(canales_cost, controles_finales), response = "Orders")

m_sales  <- lm(formula_sales,  data = base)
m_orders <- lm(formula_orders, data = base)

cat("\n--- Modelo Sales ---\n")
print(summary(m_sales))
print(confint(m_sales))

cat("\n--- Modelo Orders ---\n")
print(summary(m_orders))
print(confint(m_orders))

tabla_multiples <- bind_rows(resumen_lm(m_sales,  "Sales"),
                             resumen_lm(m_orders, "Orders"))
write_csv(tabla_multiples, "resultados/punto6_regresiones_multiples.csv")

# Orders por cada 1.000 de costo
print(round(coef(m_orders)[canales_cost] * 1000, 3))

# Simple vs. múltiple
comparacion <- tibble(
  Canal            = canales_cost,
  Simple           = sapply(canales_cost, function(x) coef(modelos_simples[[x]])[x]),
  Multiple         = coef(m_sales)[canales_cost],
  p_valor_Multiple = summary(m_sales)$coefficients[canales_cost, 4])
print(as.data.frame(mutate(comparacion, across(where(is.numeric), ~ round(.x, 4)))))
write_csv(comparacion, "resultados/punto6_comparacion_simple_multiple.csv")


# 6.3 Sensibilidad: agregando el costo de Google
m_sales_google <- lm(reformulate(c(canales_cost, controles_finales, "Google_Cost"),
                                 response = "Sales"), data = base)
print(summary(m_sales_google))

# Mariana Avila
# PUNTO 7. LOGARITMOS Y DUMMIES DE TEMPORADA ----

# 7.1 Preparacion de los datos
base$Week <- as.Date(base$Week)
base <- base[order(base$Week), ]

costos_mariana <- c(
  "Amazon_DSP_Cost",
  "Amazon_Search_Cost",
  "Amazon_Sponsored_Cost"
)

controles_mariana <- c(
  "Trends_NonBrand", "GASREGW", "ICSA", "WEI"
)

variables_mariana <- c(
  "Week", "Sales", "Orders",
  costos_mariana, controles_mariana
)

datos_mariana <- as.data.frame(base[, variables_mariana])

stopifnot(
  !anyNA(datos_mariana),
  !anyDuplicated(datos_mariana$Week),
  all(datos_mariana$Sales > 0),
  all(datos_mariana$Orders > 0)
)

dim(datos_mariana)


# 7.2 Dummies de calendario y division temporal ----

mes_mariana <- as.integer(format(datos_mariana$Week, "%m"))

datos_mariana$Verano <- as.integer(
  mes_mariana %in% c(6, 7, 8)
)

datos_mariana$Fin_Anio <- as.integer(
  mes_mariana %in% c(11, 12)
)

corte_mariana <- floor(0.80 * nrow(datos_mariana))

entrenamiento_mariana <- datos_mariana[
  seq_len(corte_mariana), 
]

evaluacion_mariana <- datos_mariana[
  seq.int(corte_mariana + 1, nrow(datos_mariana)), 
]

c(
  Entrenamiento = nrow(entrenamiento_mariana),
  Evaluacion = nrow(evaluacion_mariana)
)
# 7.3 Camino A: modelos con logaritmo ----

regresores_mariana <- c(costos_mariana, controles_mariana)

modelo_log_sales <- lm(
  reformulate(regresores_mariana, response = "log(Sales)"),
  data = entrenamiento_mariana
)

modelo_log_orders <- lm(
  reformulate(regresores_mariana, response = "log(Orders)"),
  data = entrenamiento_mariana
)

summary(modelo_log_sales)
summary(modelo_log_orders)

# 7.4 Camino B: modelos con dummies ----

regresores_dummy_mariana <- c(
  regresores_mariana, "Verano", "Fin_Anio"
)

modelo_dummy_sales <- lm(
  reformulate(regresores_dummy_mariana, response = "Sales"),
  data = entrenamiento_mariana
)

modelo_dummy_orders <- lm(
  reformulate(regresores_dummy_mariana, response = "Orders"),
  data = entrenamiento_mariana
)

summary(modelo_dummy_sales)
summary(modelo_dummy_orders)

# 7.5 Predicciones en la escala original ----

smearing_sales <- mean(exp(residuals(modelo_log_sales)))
smearing_orders <- mean(exp(residuals(modelo_log_orders)))

pred_log_sales <- exp(
  predict(modelo_log_sales, newdata = evaluacion_mariana)
) * smearing_sales

pred_log_orders <- exp(
  predict(modelo_log_orders, newdata = evaluacion_mariana)
) * smearing_orders

pred_dummy_sales <- predict(
  modelo_dummy_sales, newdata = evaluacion_mariana
)

pred_dummy_orders <- predict(
  modelo_dummy_orders, newdata = evaluacion_mariana
)

head(data.frame(
  Semana = evaluacion_mariana$Week,
  Sales_real = evaluacion_mariana$Sales,
  Sales_log = pred_log_sales,
  Sales_dummies = pred_dummy_sales
))

# 7.6 Comparacion de errores: MAE y RMSE ----

calcular_errores <- function(real, prediccion) {
  c(
    MAE = mean(abs(real - prediccion)),
    RMSE = sqrt(mean((real - prediccion)^2))
  )
}

comparacion_mariana <- data.frame(
  KPI = c("Sales", "Sales", "Orders", "Orders"),
  Modelo = c("Logaritmo", "Dummies", "Logaritmo", "Dummies"),
  rbind(
    calcular_errores(evaluacion_mariana$Sales, pred_log_sales),
    calcular_errores(evaluacion_mariana$Sales, pred_dummy_sales),
    calcular_errores(evaluacion_mariana$Orders, pred_log_orders),
    calcular_errores(evaluacion_mariana$Orders, pred_dummy_orders)
  )
)

rownames(comparacion_mariana) <- NULL
print(comparacion_mariana)

dir.create("resultados/mariana", recursive = TRUE,
           showWarnings = FALSE)

write.csv(
  comparacion_mariana,
  "resultados/mariana/punto7_MAE_RMSE.csv",
  row.names = FALSE
)

# 7.7 Grafica de predicciones de Sales ----

plot(
  evaluacion_mariana$Week,
  evaluacion_mariana$Sales,
  type = "l",
  col = "black",
  lwd = 2,
  ylim = range(
    evaluacion_mariana$Sales,
    pred_log_sales,
    pred_dummy_sales
  ),
  main = "Sales: valores reales y predicciones",
  xlab = "Semana",
  ylab = "Sales"
)

lines(evaluacion_mariana$Week, pred_log_sales,
      col = "blue", lwd = 2)

lines(evaluacion_mariana$Week, pred_dummy_sales,
      col = "red", lwd = 2)

legend(
  "topright",
  legend = c("Real", "Logaritmo", "Dummies"),
  col = c("black", "blue", "red"),
  lty = 1,
  lwd = 2,
  cex = 0.8,
  bty = "n"
)


# 7.8 Grafica de predicciones de Orders ----

plot(
  evaluacion_mariana$Week,
  evaluacion_mariana$Orders,
  type = "l",
  col = "black",
  lwd = 2,
  ylim = range(
    evaluacion_mariana$Orders,
    pred_log_orders,
    pred_dummy_orders
  ),
  main = "Orders: valores reales y predicciones",
  xlab = "Semana",
  ylab = "Orders"
)

lines(evaluacion_mariana$Week, pred_log_orders,
      col = "blue", lwd = 2)

lines(evaluacion_mariana$Week, pred_dummy_orders,
      col = "red", lwd = 2)

legend(
  "topright",
  legend = c("Real", "Logaritmo", "Dummies"),
  col = c("black", "blue", "red"),
  lty = 1,
  lwd = 2,
  cex = 0.8,
  bty = "n"
)


# PUNTO 8. DIAGNOSTICO DE LOS MODELOS ----
# 8.1 Residuos frente a ajustes y tiempo ----

modelos_mariana <- list(
  Sales_log = modelo_log_sales,
  Sales_dummies = modelo_dummy_sales,
  Orders_log = modelo_log_orders,
  Orders_dummies = modelo_dummy_orders
)

graficar_residuos_mariana <- function(modelo, nombre) {
  ajustes <- fitted(modelo)
  residuos <- residuals(modelo)
  
  plot(
    ajustes, residuos,
    main = paste(nombre, "- residuos vs. ajustes"),
    xlab = "Valores ajustados",
    ylab = "Residuos",
    pch = 19, col = "steelblue"
  )
  abline(h = 0, col = "red", lty = 2)
  
  plot(
    entrenamiento_mariana$Week, residuos,
    type = "l",
    main = paste(nombre, "- residuos en el tiempo"),
    xlab = "Semana",
    ylab = "Residuos",
    col = "steelblue"
  )
  abline(h = 0, col = "red", lty = 2)
}

for (nombre in names(modelos_mariana)) {
  graficar_residuos_mariana(
    modelos_mariana[[nombre]], nombre
  )
}



# 8.2 Pruebas de residuos y VIF ----

diagnostico_mariana <- function(modelo, nombre) {
  e <- residuals(modelo)
  X <- model.matrix(modelo)[, -1, drop = FALSE]
  n <- length(e)
  
  auxiliar_bp <- lm(I(e^2) ~ X)
  bp <- n * summary(auxiliar_bp)$r.squared
  
  rezagos <- sapply(1:4, function(j) {
    c(rep(NA_real_, j), head(e, -j))
  })
  
  filas <- 5:n
  auxiliar_bg <- lm(
    e[filas] ~ cbind(
      X[filas, , drop = FALSE],
      rezagos[filas, , drop = FALSE]
    )
  )
  bg <- length(filas) * summary(auxiliar_bg)$r.squared
  
  valores_vif <- sapply(seq_len(ncol(X)), function(j) {
    auxiliar <- lm(X[, j] ~ X[, -j, drop = FALSE])
    1 / (1 - summary(auxiliar)$r.squared)
  })
  
  tabla_vif <- data.frame(
    Modelo = nombre,
    Variable = colnames(X),
    VIF = valores_vif
  )
  
  write.csv(
    tabla_vif,
    paste0("resultados/mariana/", nombre, "_VIF.csv"),
    row.names = FALSE
  )
  
  data.frame(
    Modelo = nombre,
    p_BP = pchisq(bp, ncol(X), lower.tail = FALSE),
    p_BG4 = pchisq(bg, 4, lower.tail = FALSE),
    VIF_maximo = max(valores_vif)
  )
}

diagnosticos_mariana <- do.call(
  rbind,
  lapply(names(modelos_mariana), function(nombre) {
    diagnostico_mariana(modelos_mariana[[nombre]], nombre)
  })
)

print(diagnosticos_mariana)

write.csv(
  diagnosticos_mariana,
  "resultados/mariana/punto8_diagnosticos.csv",
  row.names = FALSE
)

# 8.3 Influencia y patrones temporales ----

for (nombre in names(modelos_mariana)) {
  modelo <- modelos_mariana[[nombre]]
  residuos <- residuals(modelo)
  cook <- cooks.distance(modelo)
  
  influencia <- data.frame(
    Semana = entrenamiento_mariana$Week,
    Cook = cook,
    Revisar = cook > 4 / length(cook)
  )
  
  write.csv(
    influencia,
    paste0("resultados/mariana/", nombre, "_influencia.csv"),
    row.names = FALSE
  )
  
  graficar_final <- function() {
    plot(
      entrenamiento_mariana$Week, cook,
      type = "h",
      main = paste(nombre, "- influencia"),
      xlab = "Semana", ylab = "Distancia de Cook"
    )
    abline(h = 4 / length(cook), col = "red", lty = 2)
    
    acf(
      residuos, lag.max = 52,
      main = paste(nombre, "- ACF de residuos")
    )
    
    boxplot(
      residuos ~ factor(
        format(entrenamiento_mariana$Week, "%m"),
        levels = sprintf("%02d", 1:12)
      ),
      main = paste(nombre, "- residuos por mes"),
      xlab = "Mes", ylab = "Residuos"
    )
    abline(h = 0, col = "red", lty = 2)
  }
  
  graficar_final()
  
  pdf(
    paste0("resultados/mariana/", nombre, "_diagnosticos.pdf"),
    width = 9, height = 6
  )
  graficar_residuos_mariana(modelo, nombre)
  graficar_final()
  dev.off()
}



