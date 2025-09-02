library(tidyverse)
library(vars)

#Seteo directorio, limpio el environment y abro el último R script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls(all.names = TRUE))
gc()

source("TP2.2_Procesamiento.R")   # carga datos_hp_cycle ya ordenado
source("PS3_LP_Tools.R")          # define lp(), lp.pw(), etc.

#Ordenamos las variables de acuerdo al enunciado
datos_hp_cycle <- datos_hp_cycle[, c("smdi", "gdp_socios", "gdp_agro_pc", "gdp_resto_pc", "employment", "cons_pc", 
                                     "invest_pc", "reer")]

smdi <- datos_hp_cycle[, "smdi"]

#Estimamos el AR(2)
AR <- arima(smdi, order = c(2, 0, 0), include.mean = TRUE)

#Obtenemos los residuos estimados
epsilon_wt <- residuals(AR)

#Construimos la variable dummy y los distintos sigmas
Dt <- as.numeric(epsilon_wt > 0)

sigma1 <- sd(epsilon_wt[Dt == 1])
sigma2 <- sd(epsilon_wt[Dt == 0])

#Creamos los shocks estandarizados
eps_ws1 <- Dt * epsilon_wt / sigma1
eps_ws2 <- (1 - Dt) * epsilon_wt / sigma2

datos <- as.matrix(cbind(datos_hp_cycle, eps_ws1 = as.numeric(eps_ws1), eps_ws2 = as.numeric(eps_ws2)))
colnames(datos) <- c(colnames(datos_hp_cycle), "eps_ws1", "eps_ws2")

rm(list = setdiff(ls(), c("datos", "Dt")))

#Preparamos todo para graficar
source("TP2_LP_Tools.R")

p_HQC <- 1
H <- 20

titulos <- list(
  smdi           = "SMDI",
  gdp_socios     = "PIB de Socios Comerciales",
  gdp_agro_pc    = "PIB Agropecuario per cápita",
  gdp_resto_pc   = "PIB Resto per cápita",
  employment     = "Empleo",
  cons_pc        = "Consumo per cápita",
  invest_pc      = "Inversión per cápita",
  reer           = "Tipo de Cambio Real Multilateral")

for (var in names(titulos)) {
  y <- as.numeric(datos[, var])
  res <- lp_asimetrica(y, datos[, "eps_ws1"], datos[, "eps_ws2"], p_HQC, H)
  
  df_plot <- tibble(h = 0:H, Shock = "Negativo", pe = res$pe1, lb = res$lb1, ub = res$ub1, variable = titulos[[var]]) %>%
    bind_rows(tibble(h = 0:H, Shock = "Positivo", pe = res$pe2, lb = res$lb2, ub = res$ub2, variable = titulos[[var]]))
  
  g <- ggplot(df_plot, aes(x = h, y = pe, color = Shock, fill = Shock)) +
    geom_line(linewidth = 1) +
    geom_ribbon(aes(ymin = lb, ymax = ub), alpha = 0.5, color = NA) +
    facet_wrap(~ Shock, scales = "free_y") +
    scale_color_manual(values = c("Positivo" = "blue", "Negativo" = "red")) +
    scale_fill_manual(values = c("Positivo" = "blue", "Negativo" = "red")) +
    labs(x = "Horizonte (trimestres)", y = "Respuesta estimada (%)", title = titulos[[var]]) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"), axis.title = element_text(face = "bold"),  
          strip.text = element_text(color = "black", face = "bold", size = 11), legend.position = "bottom")
  
  print(g)
  ggsave(paste0("Gráficos/", var, "_asimetrico.png"), g, width = 10, height = 5, dpi = 300)
}

#Definiación alternativa de SMDI

datos_hp_cycle <- datos[, c("smdi", "gdp_socios", "gdp_agro_pc", "gdp_resto_pc", "employment", "cons_pc", 
                                     "invest_pc", "reer")]

smdi_lag1 <- stats::lag(datos_hp_cycle[, "smdi"], -1)
smdi_lag2 <- stats::lag(datos_hp_cycle[, "smdi"], -2)
smdi_lag3 <- stats::lag(datos_hp_cycle[, "smdi"], -3)
smdi_lag4 <- stats::lag(datos_hp_cycle[, "smdi"], -4)

smdi_t4 <- window((smdi_lag1 + smdi_lag2 + smdi_lag3 + smdi_lag4) / 4, start = c(2006, 1), end = c(2023, 4), frequency = 4)

#Estimamos el AR(2)
AR <- arima(smdi_t4, order = c(2, 0, 0), include.mean = TRUE)

#Obtenemos los residuos estimados
epsilon_wt <- residuals(AR)

#Construimos la variable dummy y los distintos sigmas
Dt <- as.numeric(epsilon_wt > 0)

sigma1 <- sd(epsilon_wt[Dt == 1])
sigma2 <- sd(epsilon_wt[Dt == 0])

#Creamos los shocks estandarizados
eps_ws1 <- Dt * epsilon_wt / sigma1
eps_ws2 <- (1 - Dt) * epsilon_wt / sigma2

datos <- as.matrix(cbind(datos_hp_cycle, eps_ws1 = as.numeric(eps_ws1), eps_ws2 = as.numeric(eps_ws2)))
colnames(datos) <- c(colnames(datos), "eps_ws1", "eps_ws2")

rm(list = setdiff(ls(), c("datos", "Dt")))