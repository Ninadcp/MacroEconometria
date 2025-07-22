#VERSIÓN 20/07 05:16 HS

library(urca)
library(stringr)
library(tibble)
library(dplyr)
library(readr) 
library(vars)

rm(list=ls())

source("/Users/ninadicostanzopereira/Desktop/MacroMetrics/TP1/TP1_Procesamiento.R") #acá m limpia el global enviroment
wd <- "/Users/ninadicostanzopereira/Desktop/MacroMetrics/TP1"
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
# 2(c) - SVAR e IRFs
# ================================

# ---------- Chile ---------- #

# Estimo el VAR reducido 
var_chile_VAR <- vars::VAR(var_chile_diff_ordered, p = 1, type = "const")

# Tamaño del sistema = 5
m <- var_chile_VAR$K

# Matrices de restricciones (AB-model)
Amat_chile <- diag(m)
for (i in 2:m) {
  for (j in 1:(i - 1)) {
    Amat_chile[i, j] <- NA
  }
}

Bmat_chile <- matrix(0, m, m)
diag(Bmat_chile) <- NA

# Estimación SVAR estructural (forma AB)
svar_chile <- SVAR(var_chile_VAR, Amat = Amat_chile, Bmat = Bmat_chile, lrtest = FALSE)

# IRFs
irf_chile <- irf(svar_chile, impulse = "d_tpm_chile", response = colnames(var_chile_diff_ordered), 
                 n.ahead = 12, ortho=TRUE, boot = TRUE)

png(file.path(output, "IRF_Chile_nuev.png"), width = 1000, height = 800)
plot(irf_chile)
dev.off()

# FEVD
fevd_chile <- fevd(svar_chile, n.ahead = 12)
png(file.path(output, "FEVD_Chile_nuev.png"), width = 1000, height = 800)
plot(fevd_chile)
dev.off()

# ---------- México ---------- #

var_mexico_VAR <- vars::VAR(var_mexico_diff_ordered, p = 1, type = "const")
m_mex <- var_mexico_VAR$K

Amat_mexico <- diag(m_mex)
for (i in 2:m_mex) {
  for (j in 1:(i - 1)) {
    Amat_mexico[i, j] <- NA
  }
}

Bmat_mexico <- matrix(0, m_mex, m_mex)
diag(Bmat_mexico) <- NA

svar_mexico <- vars::SVAR(var_mexico_VAR, Amat = Amat_mexico, Bmat = Bmat_mexico, lrtest = FALSE)

irf_mexico <- irf(svar_mexico, impulse = "d_tpm_mexico", response = colnames(var_mexico_diff_ordered), 
                  n.ahead = 12, ortho=TRUE, boot = TRUE) #AGREGAR SU D IDENTIFICACIÓN
#irf_one <- irf(SVAR, response = "er", impulse = "pcom", n.ahead = 10, ortho = TRUE, boot = TRUE)

png(file.path(output, "IRF_Mexico.png"), width = 1000, height = 800)
plot(irf_mexico)
dev.off()
#DEFINIR IRF_MEX2 CON LA FUNCIÓN

fevd_mexico <- fevd(svar_mexico, n.ahead = 20)
png(file.path(output, "FEVD_Mexico.png"), width = 1000, height = 800)
par(cex.main = 1.6,   
    cex.lab = 1.7,    
    cex.axis = 1,   
    mar = c(4, 4, 2, 1))
plot(fevd_mexico)
dev.off()

#


