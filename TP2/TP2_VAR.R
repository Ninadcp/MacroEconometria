library(tidyverse)
library(vars)

#Seteo directorio, limpio el environment y abro el último R script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls(all.names = TRUE))
gc()

source("TP2.2_Procesamiento.R")

#Ordenamos las variables de acuerdo al enunciado
datos_hp_cycle <- datos_hp_cycle[, c("smdi", "gdp_socios", "gdp_agro_pc", "gdp_resto_pc", "employment", "cons_pc", 
                                     "invest_pc", "reer")]

#Elección del número de lags
p <- VARselect(datos_hp_cycle, lag.max = 4, type = "cons")

cat("Lag según HQC:", p$selection[2], "| Lag según AIC:", p$selection[1], "\n")

p_HQC <- p$selection[2]
p_AIC <- p$selection[1]

#Estimamos el VAR
VAR_unrestr <- VAR(datos_hp_cycle, p = p_HQC, type = "const")

resmat <- matrix(c(
  1,        0,             0,              0,             0,            0,         0,           0,      1,
  0,        1,             0,              0,             0,            0,         0,           0,      1,
  1,        1,             1,              1,             1,            1,         1,           1,      1,
  1,        1,             1,              1,             1,            1,         1,           1,      1,
  1,        1,             1,              1,             1,            1,         1,           1,      1,
  1,        1,             1,              1,             1,            1,         1,           1,      1,
  1,        1,             1,              1,             1,            1,         1,           1,      1,
  1,        1,             1,              1,             1,            1,         1,           1,      1), 
  nrow = 8, byrow = TRUE)

VAR_sequia <- restrict(VAR_unrestr, method = "manual", resmat = resmat)


#SVAR

m <- VAR_sequia$K
T <- VAR_sequia$obs

#A Matrix
Amat <- diag(m)
for (i in 2:m) {
  for (j in 1:(i - 1)) {
    Amat[i, j] <- NA
  }
}

Amat[2,1] <- 0

#B Matrix
Bmat <- matrix(0, m, m)
for (i in 1:m) {
  Bmat[i, i] <- NA
}

SVAR_sequia <- SVAR(VAR_sequia, Amat = Amat, Bmat = Bmat, lrtest = FALSE)
SVAR_sequia1 <- SVAR(VAR_unrestr, Amat = Amat, Bmat = Bmat, lrtest = FALSE) #NINA 

#Cargamos algunas funciones que me van a servir para graficar

source("PS2_SVAR_Tools.R")
source("PS2_SVAR_Plots.R")
source("Plot_IRFs.R")

H <- 20
R <- 1000 #Probarlo con 1000 para que no tarde una eternidad
type <- "nonparametric"
gamma <- 0.95
p <- p_HQC
Y.boot <- boot.replicate(VAR_unrestr, R, type)
IRF.boot <- SVAR.sirf.boot(SVAR_sequia, Amat, Bmat, H, gamma, Y.boot)

#Ajustamos los nombres de las variables

var_names <- dimnames(IRF.boot$pe)[[1]]
dimnames(IRF.boot$pe)[[2]] <- var_names
dimnames(IRF.boot$lb)[[2]] <- var_names
dimnames(IRF.boot$ub)[[2]] <- var_names

#Ahora sí, graficamos las IRFs

titulos <- list(
  SMDI           = "SMDI",
  GDP_SOCIOS     = "PIB de Socios Comerciales",
  GDP_AGRO_PC    = "PIB Agropecuario per cápita",
  GDP_RESTO_PC   = "PIB Resto per cápita",
  EMPLOYMENT     = "Empleo",
  CONS_PC        = "Consumo per cápita",
  INVEST_PC      = "Inversión per cápita",
  REER           = "Tipo de Cambio Real")

IRF <- create_irf_boot_df(IRF.boot, H)
irf_smdi <- IRF %>% filter(shock == "SMDI")

irf_smdi$response_label <- factor(irf_smdi$response, levels = names(titulos), labels = titulos)

ggplot(irf_smdi, aes(x = horizon, y = pe)) +
  geom_line(color = "red", linewidth = 1) +
  geom_ribbon(aes(ymin = lb, ymax = ub), alpha = 0.8, fill = "gray") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~ response_label, scales = "free_y", ncol = 2) +
  labs(x = "Horizonte (trimestres)", y = "Respuesta (%)", title = "IRFs ante un shock en SMDI", 
       caption = "IC al 95% (Bootstrap)") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5, face = "bold"), 
        strip.text = element_text(color = "black", face = "bold", size = 11), panel.spacing = unit(1.5, "lines"))
