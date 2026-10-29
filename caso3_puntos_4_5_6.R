# CASO 3 - Marketing digital y demanda de alquiler de vehículos
# Business Analytics 2026-3 | Pontificia Universidad Javeriana
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