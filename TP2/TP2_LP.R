library(tidyverse)
library(vars)
library(zoo)
library(lmtest)
library(sandwich)
library(ggplot2)

#Seteo directorio, limpio el environment y abro el último R script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls(all.names = TRUE))
gc()

source("TP2.2_Procesamiento.R")

#Ordenamos las variables de acuerdo al enunciado
datos_hp_cycle <- datos_hp_cycle[, c("smdi", "gdp_socios", "gdp_agro_pc", "gdp_resto_pc", "employment", "cons_pc", 
                                     "invest_pc", "reer")]

smdi <- datos_hp_cycle[, "smdi"]

#Estimamos el AR(1)
AR <- arima(smdi, order = c(2, 0, 0), include.mean = TRUE)

#Obtenemos los residuos estimados y graficamos
epsilon_wt <- residuals(AR)

fechas <- seq(as.Date("2005-01-01"), by = "quarter", length.out = length(smdi))
df_plot <- data.frame(fecha = fechas, smdi = as.numeric(smdi), epsilon_wt = as.numeric(epsilon_wt))

ggplot(df_plot, aes(x = fecha)) +
  geom_line(aes(y = smdi, color = "SMDI")) +
  geom_line(aes(y = epsilon_wt, color = "Shocks estimados (ε)")) +
  scale_y_continuous(name = "SMDI", sec.axis = sec_axis(~.*0.85, name = expression("Shocks estimados ("*epsilon[t]^w*")"))) +
  scale_color_manual(name = NULL, values = c("SMDI" = "red", "Shocks estimados (ε)" = "blue")) +
  labs(x = "Fecha", title = expression("SMDI y shocks estimados ("*epsilon[t]^w*")")) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), legend.position = "bottom")
ggsave(filename = "Gráficos/epsilon.png", width = 20, height = 16, units = "cm", dpi = 300)

#Estandarizamos el shock
sigma_eps <- sd(epsilon_wt)
epsilon_wst <- epsilon_wt / sigma_eps

datos <- as.matrix(cbind(datos_hp_cycle, epsilon = as.numeric(epsilon_wst)))
colnames(datos) <- c(colnames(datos_hp_cycle), "epsilon")

#rm(list = setdiff(ls(), c("datos")))

#Local Projections

source("TP2_LP_Tools.R")

p_HQC <- 1
H <- 20

#Alternativa 1

resultados_irf_alt1 <- list()
vars <- colnames(datos)[-9]

for (var in vars) {
  irf <- lp_alt1(y = datos[, var], shock = datos[, "epsilon"], p = p_HQC, H = H)
  resultados_irf_alt1[[var]] <- tibble(h = 0:H, pe = irf$pe, lb = irf$lb, ub = irf$ub, variable = var)
}

df_irf_alt1 <- bind_rows(resultados_irf_alt1)

titulos <- list(
  smdi           = "SMDI",
  gdp_socios     = "PIB de Socios Comerciales",
  gdp_agro_pc    = "PIB Agropecuario per cápita",
  gdp_resto_pc   = "PIB Resto per cápita",
  employment     = "Empleo",
  cons_pc        = "Consumo per cápita",
  invest_pc      = "Inversión per cápita",
  reer           = "Tipo de Cambio Real")

df_irf_alt1$response_label <- factor(df_irf_alt1$variable, levels = names(titulos), labels = titulos)

ggplot(df_irf_alt1, aes(x = h, y = pe)) +
  geom_line(color = "red", linewidth = 1) +
  geom_ribbon(aes(ymin = lb, ymax = ub), alpha = 0.8, fill = "gray") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~ response_label, scales = "free_y", ncol = 2) +
  labs(title = "IRF estimadas vía Local Projections", subtitle = "Shock estructural de sequía (SMDI)", 
       x = "Horizonte (trimestres)", y = "Respuesta estimada", caption = "IC al 95% (HAC)") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5, face = "bold"), 
        strip.text = element_text(color = "black", face = "bold", size = 11), panel.spacing = unit(1.5, "lines"))


## NINA
datos <- as.data.frame(datos)
Y_pw <- as.matrix(cbind(smdi = datos$smdi, datos[, vars]))
D <- ifelse(epsilon_wt > 0, 1, 0)

sigma1 <- sd(epsilon_wt[D == 1])
sigma2 <- sd(epsilon_wt[D == 0])

eps_s1 <- D * epsilon_wt / sigma1
eps_s2 <- (1 - D) * epsilon_wt / sigma2

source("PS3_LP_Tools.R")

p <- 1
H <- 4
gamma <- 0.95

idx.s <- 1  # smdi
idx.r <- which(colnames(Y_pw) == "employment")

irf_empleo_pw <- lp.pw(Y = Y_pw, D = D, p = p, idx.s = idx.s, idx.r = idx.r, H = H, gamma = gamma)



