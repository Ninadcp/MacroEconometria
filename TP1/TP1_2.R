#VERSIÓN 22/07 09:42 HS
#Puse nombres = que el archivo de Franco

library(urca)
library(stringr)
library(tibble)
library(dplyr)
library(readr) 
library(vars)

rm(list=ls())

source("/Users/ninadicostanzopereira/Desktop/MacroMetrics/MacroEconometria/TP1/TP1_Procesamiento.R") 
source("/Users/ninadicostanzopereira/Desktop/MacroMetrics/MacroEconometria/PS2 2/PS2_SVAR_Tools.R")
source("/Users/ninadicostanzopereira/Desktop/MacroMetrics/MacroEconometria/PS2 2/PS2_SVAR_Plots.R")

wd <- "/Users/ninadicostanzopereira/Desktop/MacroMetrics/MacroEconometria/TP1"
output <- file.path(wd, "output")
#Chile

plot(var_chile[, "imacec"], main = "Serie mensual para el Indicador mensual de actividad económica (IMACEC)", 
     xlab = "Fecha", ylab = "Valor", col = "darkblue", lwd = 2, type = "l")

plot(var_chile[, "ipc_sae"], main = "Serie mensual para el IPC núcleo (IPC (SAE))", 
     xlab = "Fecha", ylab = "Valor", col = "darkblue", lwd = 2, type = "l")

plot(var_chile[, "tcn_chile"], main = "Serie mensual para el Tipo de cambio nominal (pesos chilenos por dólar)", 
     xlab = "Fecha", ylab = "Valor", col = "darkblue", lwd = 2, type = "l")

plot(var_chile[, "embi_chile"], main = "Serie mensual para el Índice EMBI (en puntos porcentuales)", 
     xlab = "Fecha", ylab = "Valor", col = "darkblue", lwd = 2, type = "l")

plot(var_chile[, "tpm_chile"], main = "Serie mensual para la Tasa de Política Monetaria (en puntos porcentuales)", 
     xlab = "Fecha", ylab = "Valor", col = "darkblue", lwd = 2, type = "l")
#México

plot(var_mexico[, "igae"], main = "Serie mensual para el Indicador mensual de actividad económica (IGAE)", 
     xlab = "Fecha", ylab = "Valor", col = "darkred", lwd = 2, type = "l")

plot(var_mexico[, "inpc"], main = "Serie mensual para el IPC núcleo (INPC)", 
     xlab = "Fecha", ylab = "Valor", col = "darkred", lwd = 2, type = "l")

plot(var_mexico[, "tcn_mexico"], main = "Serie mensual para el Tipo de cambio nominal (pesos mexicanos por dólar)", 
     xlab = "Fecha", ylab = "Valor", col = "darkred", lwd = 2, type = "l")

plot(var_mexico[, "embi_mexico"], main = "Serie mensual para el Índice EMBI (en puntos porcentuales)", 
     xlab = "Fecha", ylab = "Valor", col = "darkred", lwd = 2, type = "l")

plot(var_mexico[, "tpm_mexico"], main = "Serie mensual para la Tasa de Política Monetaria (en puntos porcentuales)", 
     xlab = "Fecha", ylab = "Valor", col = "darkred", lwd = 2, type = "l")

# URT: Augmented-Dickey-Fuller
test_adf <- function(ts_data, pais) {
  resultados <- lapply(colnames(ts_data), function(nombre) {
    serie <- ts_data[, nombre]
    test <- ur.df(serie, type = "trend", selectlags = "BIC")
    resumen <- summary(test)
    
    estadistico <- round(resumen@teststat[1], 3)
    valor_critico <- resumen@cval[1, "5pct"]
    decision <- ifelse(estadistico < valor_critico,
                       "Rechazo H0 (es estacionaria)",
                       "No rechazo H0 (tiene raíz unitaria)")
  
    tibble(
      País = pais,
      Variable = nombre,
      Lag_BIC = test@lags,
      Estadístico_ADF = estadistico,
      Valor_crítico_5pct = valor_critico,
      Decisión = decision
    )
  })
  
  bind_rows(resultados)
}

# Ejecutar test para Chile y México
adf_chile <- test_adf(var_chile, "Chile")
adf_mexico <- test_adf(var_mexico, "México")

# Unir y exportar
adf_resultados <- bind_rows(adf_chile, adf_mexico)
write_csv(adf_resultados, file.path(output, "adf_resultados.csv"))

### ACA VAN COSAS NUEVAS LO ANTERIOR FUI YO CAMBIANDO LO D LUCA PARA Q CORRA EN MAC
#HICE CHILE COMO HACE EL TUTOR Y EL OTRO COMO YO LO SABÍA HACER debería comp
#tomar dif a las no estacionarias

# ================================
# 2(a) - Preparo datos para el VAR
# ================================

# CHILE - paso a diferencias lo que no era estacionario
diff_imacec     <- diff(var_chile[, "imacec"])
diff_ipc_sae    <- diff(var_chile[, "ipc_sae"])
diff_tcn_chile  <- diff(var_chile[, "tcn_chile"])
diff_tpm_chile  <- diff(var_chile[, "tpm_chile"])
diff_embi_chile <- diff(var_chile[, "embi_chile"]) #DF nos da q no pero lo hacemos con el más robusto 
embi_chile_recortado <- window(diff_embi_chile, start = start(diff_imacec))

# Armo el dataset final
var_chile_diff <- cbind(diff_imacec, diff_ipc_sae, diff_tcn_chile, embi_chile_recortado, diff_tpm_chile)
colnames(var_chile_diff) <- c("d_imacec", "d_ipc_sae", "d_tcn_chile", "embi_chile", "d_tpm_chile")

# Veo el número de rezagos óptimo
VARselect(var_chile_diff, lag.max = 12, type = "const")

# MÉXICO - mismo tratamiento
diff_inpc        <- diff(var_mexico[, "inpc"])
diff_tcn_mexico  <- diff(var_mexico[, "tcn_mexico"])
diff_tpm_mexico  <- diff(var_mexico[, "tpm_mexico"])
diff_embi_mexico <- diff(var_mexico[, "embi_mexico"]) #DF nos da q no pero lo hacemos con el más robusto 
igae_recortado        <- window(var_mexico[, "igae"], start = start(diff_inpc))
embi_mexico_recortado <- window(diff_embi_mexico, start = start(diff_inpc))

var_mexico_diff <- cbind(igae_recortado, diff_inpc, diff_tcn_mexico, embi_mexico_recortado, diff_tpm_mexico)
colnames(var_mexico_diff) <- c("igae", "d_inpc", "d_tcn_mexico", "embi_mexico", "d_tpm_mexico")

VARselect(var_mexico_diff, lag.max = 12, type = "const")
p <- 1
# Acá también elijo 1 rezago NINA NINA VER

# ================================
# 2(b) - Supuesto recursivo: precios y actividad responden con rezago 
# ================================
# Chile: actividad y precios NO SE VEN AFECTADAS por el shock de forma contemporanea.
orden_chile <- c("d_ipc_sae", "d_imacec", "d_tpm_chile", "d_tcn_chile", "embi_chile")
var_chile_diff_ordered <- var_chile_diff[, orden_chile]

# México: misma 
orden_mexico <- c("d_inpc", "igae", "d_tpm_mexico", "d_tcn_mexico", "embi_mexico")
var_mexico_diff_ordered <- var_mexico_diff[, orden_mexico]

# ================================
# 2(c) - SVAR e IRFs: CHILE
# ================================

# Estimación VAR reducido
var_chile_VAR <- vars::VAR(var_chile_diff_ordered, p = p, type = "const")

# Dimensiones del sistema
m <- var_chile_VAR$K
T <- var_chile_VAR$obs

# Matrices estructurales (AB)
Amat_chile <- diag(m)
for (i in 2:m) {
  for (j in 1:(i - 1)) {
    Amat_chile[i, j] <- NA
  }
}
Bmat_chile <- matrix(0, m, m)
diag(Bmat_chile) <- NA

# Estimación SVAR estructural
svar_chile <- SVAR(var_chile_VAR, Amat = Amat_chile, Bmat = Bmat_chile, lrtest = FALSE)

# Matrices de impacto
S <- t(resid(var_chile_VAR)) %*% resid(var_chile_VAR) / (T - m * p - 1)
P <- solve(svar_chile$A, svar_chile$B)
S.SVAR <- P %*% t(P)

# Parámetros estructurales
pars.R <- Bcoef(var_chile_VAR)
pars.S <- solve(svar_chile$A, pars.R)

# Horizonte de análisis
H <- 24
H.ERPT <- 240
R <- 500
gamma <- 0.95
type <- "nonparametric"

# IRF
IRF_chile <- SVAR.sirf(svar_chile, H)
png(file.path(output, "IRF_Chile.png"), width = 1000, height = 800)
plot.sirf(IRF_chile, m, H)
dev.off()

# IRF acumulada
IRF_chile_c <- SVAR.sirf(svar_chile, H, cumulative = TRUE)
png(file.path(output, "IRF_Chile_Cumulative.png"), width = 1000, height = 800)
plot.sirf(IRF_chile_c, m, H)
dev.off()

# FEVD
FEVD_chile <- SVAR.fevd(svar_chile, H)
png(file.path(output, "FEVD_Chile.png"), width = 1000, height = 800)
plot.fevd(FEVD_chile, m, H)
dev.off()

# HD
HD_chile <- SVAR.hd(svar_chile)
png(file.path(output, "HD_Chile.png"), width = 1000, height = 800)
plot.hd(var_chile_diff, HD_chile, m)
dev.off()

# ERPT
ERPT_chile <- SVAR.erpt(svar_chile, H.ERPT, 3, 2)
png(file.path(output, "ERPT_Chile.png"), width = 1000, height = 800)
plot.erpt(ERPT_chile, H.ERPT)
dev.off()

# Bootstrap
Y.boot_chile <- boot.replicate(var_chile_VAR, R, type)

# IRF bootstrap
IRF_boot_chile <- SVAR.sirf.boot(svar_chile, Amat_chile, Bmat_chile, H, gamma, Y.boot_chile)
png(file.path(output, "IRF_Chile_Boot.png"), width = 1000, height = 800)
plot.sirf.boot(IRF_boot_chile, m, H)
dev.off()

# IRF acumulada bootstrap
IRF_c_boot_chile <- SVAR.sirf.boot(svar_chile, Amat_chile, Bmat_chile, H, gamma, Y.boot_chile, cumulative = TRUE)
png(file.path(output, "IRF_Chile_Boot_Cumulative.png"), width = 1000, height = 800)
plot.sirf.boot(IRF_c_boot_chile, m, H)
dev.off()

# FEVD bootstrap
FEVD_boot_chile <- SVAR.fevd.boot(svar_chile, Amat_chile, Bmat_chile, H, gamma, Y.boot_chile)
png(file.path(output, "FEVD_Chile_Boot.png"), width = 1000, height = 800)
plot.fevd.boot(FEVD_boot_chile, m, H)
dev.off()

# ERPT bootstrap
ERPT_boot_chile <- SVAR.erpt.boot(svar_chile, Amat_chile, Bmat_chile, H.ERPT, 3, 2, gamma, Y.boot_chile)
png(file.path(output, "ERPT_Chile_Boot.png"), width = 1000, height = 800)
plot.erpt.boot(ERPT_boot_chile, H.ERPT)
dev.off()

# ================================
# 2(c) - SVAR e IRFs: MÉXICO
# ================================

# Estimación VAR reducido
var_mexico_VAR <- vars::VAR(var_mexico_diff_ordered, p = p, type = "const")

# Dimensiones del sistema
m_mex <- var_mexico_VAR$K
T_mex <- var_mexico_VAR$obs

# Matrices estructurales (AB)
Amat_mexico <- diag(m_mex)
for (i in 2:m_mex) {
  for (j in 1:(i - 1)) {
    Amat_mexico[i, j] <- NA
  }
}
Bmat_mexico <- matrix(0, m_mex, m_mex)
diag(Bmat_mexico) <- NA

# Estimación SVAR estructural
svar_mexico <- SVAR(var_mexico_VAR, Amat = Amat_mexico, Bmat = Bmat_mexico, lrtest = FALSE)

# Matrices de impacto
S_mex <- t(resid(var_mexico_VAR)) %*% resid(var_mexico_VAR) / (T_mex - m_mex * p - 1)
P_mex <- solve(svar_mexico$A, svar_mexico$B)
S.SVAR_mex <- P_mex %*% t(P_mex)

# Parámetros estructurales
pars.R_mex <- Bcoef(var_mexico_VAR)
pars.S_mex <- solve(svar_mexico$A, pars.R_mex)

# IRF
IRF_mexico <- SVAR.sirf(svar_mexico, H)
png(file.path(output, "IRF_Mexico.png"), width = 1000, height = 800)
plot.sirf(IRF_mexico, m_mex, H)
dev.off()

# IRF acumulada
IRF_mexico_c <- SVAR.sirf(svar_mexico, H, cumulative = TRUE)
png(file.path(output, "IRF_Mexico_Cumulative.png"), width = 1000, height = 800)
plot.sirf(IRF_mexico_c, m_mex, H)
dev.off()

# FEVD
FEVD_mexico <- SVAR.fevd(svar_mexico, H)
png(file.path(output, "FEVD_Mexico.png"), width = 1000, height = 800)
plot.fevd(FEVD_mexico, m_mex, H)
dev.off()

# HD
HD_mexico <- SVAR.hd(svar_mexico)
png(file.path(output, "HD_Mexico.png"), width = 1000, height = 800)
plot.hd(var_mexico_diff, HD_mexico, m_mex)
dev.off()

# ERPT
ERPT_mexico <- SVAR.erpt(svar_mexico, H.ERPT, 3, 2)
png(file.path(output, "ERPT_Mexico.png"), width = 1000, height = 800)
plot.erpt(ERPT_mexico, H.ERPT)
dev.off()

# Bootstrap
Y.boot_mexico <- boot.replicate(var_mexico_VAR, R, type)

# IRF bootstrap
IRF_boot_mexico <- SVAR.sirf.boot(svar_mexico, Amat_mexico, Bmat_mexico, H, gamma, Y.boot_mexico)
png(file.path(output, "IRF_Mexico_Boot.png"), width = 1000, height = 800)
plot.sirf.boot(IRF_boot_mexico, m_mex, H)
dev.off()

# IRF acumulada bootstrap
IRF_c_boot_mexico <- SVAR.sirf.boot(svar_mexico, Amat_mexico, Bmat_mexico, H, gamma, Y.boot_mexico, cumulative = TRUE)
png(file.path(output, "IRF_Mexico_Boot_Cumulative.png"), width = 1000, height = 800)
plot.sirf.boot(IRF_c_boot_mexico, m_mex, H)
dev.off()

# FEVD bootstrap
FEVD_boot_mexico <- SVAR.fevd.boot(svar_mexico, Amat_mexico, Bmat_mexico, H, gamma, Y.boot_mexico)
png(file.path(output, "FEVD_Mexico_Boot.png"), width = 1000, height = 800)
plot.fevd.boot(FEVD_boot_mexico, m_mex, H)
dev.off()

# ERPT bootstrap
ERPT_boot_mexico <- SVAR.erpt.boot(svar_mexico, Amat_mexico, Bmat_mexico, H.ERPT, 3, 2, gamma, Y.boot_mexico)
png(file.path(output, "ERPT_Mexico_Boot.png"), width = 1000, height = 800)
plot.erpt.boot(ERPT_boot_mexico, H.ERPT)
dev.off()

