library(urca)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

rm(list = ls(all.names = TRUE))
gc()

source("TP1_Procesamiento.R")

carpeta <- ""

#URT: Augmented-Dickey-Fuller

test_adf <- function(ts_data) {
  resultados <- lapply(colnames(ts_data), function(nombre) {
    serie <- ts_data[, nombre]
    test <- ur.df(serie, type = "trend", selectlags = "BIC")
    resumen <- summary(test)
    
    estadistico <- round(resumen@teststat[1], 3)
    valor_critico <- resumen@cval[1, "5pct"]
    decision <- ifelse(estadistico < valor_critico, "Rechazo H0 (es estacionaria)", "No rechazo H0 (tiene raíz unitaria)")
    
    tibble(Variable = nombre, Lag_BIC = test@lags, Estadístico_ADF = estadistico, Valor_crítico_5pct = valor_critico, 
           Decisión = decision)})
  
  bind_rows(resultados)
}

#Chile

png("Gráficos/imacec.png", width = 20, height = 16, units = "cm", res = 300)
plot(var_chile[, "imacec"], main = "Serie mensual para el Indicador mensual de actividad económica (IMACEC)", 
     xlab = "Fecha", ylab = "Valor", col = "darkblue", lwd = 2, type = "l")
dev.off()

png("Gráficos/ipc_sae.png", width = 20, height = 16, units = "cm", res = 300)
plot(var_chile[, "ipc_sae"], main = "Serie mensual para el IPC núcleo (IPC (SAE))", 
     xlab = "Fecha", ylab = "Valor", col = "darkblue", lwd = 2, type = "l")
dev.off()

png("Gráficos/tcn_chile.png", width = 20, height = 16, units = "cm", res = 300)
plot(var_chile[, "tcn_chile"], main = "Serie mensual para el Tipo de cambio nominal (pesos chilenos por dólar)", 
     xlab = "Fecha", ylab = "Valor", col = "darkblue", lwd = 2, type = "l")
dev.off()

png("Gráficos/embi_chile.png", width = 20, height = 16, units = "cm", res = 300)
plot(var_chile[, "embi_chile"], main = "Serie mensual para el Índice EMBI (en puntos porcentuales)", 
     xlab = "Fecha", ylab = "Valor", col = "darkblue", lwd = 2, type = "l")
dev.off()

png("Gráficos/tpm_chile.png", width = 20, height = 16, units = "cm", res = 300)
plot(var_chile[, "tpm_chile"], main = "Serie mensual para la Tasa de Política Monetaria (en puntos porcentuales)", 
     xlab = "Fecha", ylab = "Valor", col = "darkblue", lwd = 2, type = "l")
dev.off()

#URT: Augmented-Dickey-Fuller

adf_chile <- test_adf(var_chile)

for (serie in colnames(var_chile)) {
  
  print(serie)
  
  print(summary(ur.df(y = var_chile[, serie], type = "trend", selectlags = "BIC")))

}

#México

png("Gráficos/igae.png", width = 20, height = 16, units = "cm", res = 300)
plot(var_mexico[, "igae"], main = "Serie mensual para el Indicador mensual de actividad económica (IGAE)", 
     xlab = "Fecha", ylab = "Valor", col = "darkred", lwd = 2, type = "l")
dev.off()

png("Gráficos/inpc.png", width = 20, height = 16, units = "cm", res = 300)
plot(var_mexico[, "inpc"], main = "Serie mensual para el IPC núcleo (INPC)", 
     xlab = "Fecha", ylab = "Valor", col = "darkred", lwd = 2, type = "l")
dev.off()

png("Gráficos/tcn_mexico.png", width = 20, height = 16, units = "cm", res = 300)
plot(var_mexico[, "tcn_mexico"], main = "Serie mensual para el Tipo de cambio nominal (pesos mexicanos por dólar)", 
     xlab = "Fecha", ylab = "Valor", col = "darkred", lwd = 2, type = "l")
dev.off()

png("Gráficos/embi_mexico.png", width = 20, height = 16, units = "cm", res = 300)
plot(var_mexico[, "embi_mexico"], main = "Serie mensual para el Índice EMBI (en puntos porcentuales)", 
     xlab = "Fecha", ylab = "Valor", col = "darkred", lwd = 2, type = "l")
dev.off()

png("Gráficos/tpm_mexico.png", width = 20, height = 16, units = "cm", res = 300)
plot(var_mexico[, "tpm_mexico"], main = "Serie mensual para la Tasa de Política Monetaria (en puntos porcentuales)", 
     xlab = "Fecha", ylab = "Valor", col = "darkred", lwd = 2, type = "l")
dev.off()

#URT: Augmented-Dickey-Fuller

adf_mexico <- test_adf(var_mexico)

for (serie in colnames(var_mexico)) {
  
  print(serie)
  
  print(summary(ur.df(y = var_mexico[, serie], type = "trend", selectlags = "BIC")))

}

#Extras

ers_chile <- ur.ers(y = var_chile[, "embi_chile"], type = "DF-GLS", model = "const", lag.max = 4)
summary(ers_chile)

ers_mexico <- ur.ers(y = var_mexico[, "embi_mexico"], type = "DF-GLS", model = "const", lag.max = 4)
summary(ers_mexico)

adf_igae <- ur.df(y = var_mexico[, "igae"], type = "trend", selectlags = "BIC")
summary(adf_igae)

ers_igae <- ur.ers(y = var_mexico[, "igae"], type = "DF-GLS", model = "const", lag.max = 4)
summary(ers_igae)

kpss_igae <- ur.kpss(y = var_mexico[, "igae"], type = "tau", lags = "short")
summary(kpss_igae) #Se rechaza la hipótesis nula de estacionariedad

pp_igae <- ur.pp(var_mexico[, "igae"], type = "Z-tau", model = "trend", lags = "short")
summary(pp_igae)

kpss_embi_chile <- ur.kpss(y = var_chile[, "embi_chile"], type = "tau", lags = "short")
summary(kpss_embi_chile) #Se rechaza la hipótesis nula de estacionariedad

pp_embi_chile <- ur.pp(var_chile[, "embi_chile"], type = "Z-tau", model = "trend", lags = "short")
summary(pp_embi_chile)

kpss_embi_mex <- ur.kpss(y = var_mexico[, "embi_mexico"], type = "tau", lags = "short")
summary(kpss_embi_mex) #Se rechaza la hipótesis nula de estacionariedad

pp_embi_mex <- ur.pp(var_mexico[, "embi_mexico"], type = "Z-tau", model = "trend", lags = "short")
summary(pp_embi_mex)

#Bueno, medio como que todas las series son no estacionarias, por lo que diferencio las series

var_chile_diff <- diff(var_chile)
var_mexico_diff <- diff(var_mexico)

rm(list = setdiff(ls(), c("var_chile_diff", "var_mexico_diff", "var_internacionales", "test_adf")))

#Vuelvo a correr los tests

test_pp <- function(ts_data) {
  resultados <- lapply(colnames(ts_data), function(nombre) {
    serie <- ts_data[, nombre]
    test <- ur.pp(serie, type = "Z-tau", model = "trend", lags = "short")
    
    estadistico <- round(test@teststat, 3)
    valor_critico_5pct <- test@cval[1, "5pct"]
    decision <- ifelse(estadistico < valor_critico_5pct, "Rechazo H0 (es estacionaria)", "No rechazo H0 (tiene raíz unitaria)")
    
    tibble(Variable = nombre, Estadístico_PP = estadistico, Valor_crítico_5pct = valor_critico_5pct, Decisión = decision)})
  
  bind_rows(resultados)
}

test_ers <- function(ts_data) {
  resultados <- lapply(colnames(ts_data), function(nombre) {
    serie <- ts_data[, nombre]
    test <- ur.ers(serie, type = "DF-GLS", model = "trend", lag.max = 4)
    resumen <- summary(test)
    
    estadistico <- round(test@teststat, 3)
    valor_critico_5pct <- resumen@cval[1, "5pct"]
    
    decision <- ifelse(estadistico < valor_critico_5pct, "Rechazo H0 (es estacionaria)", "No rechazo H0 (tiene raíz unitaria)")
    
    tibble(Variable = nombre, Estadístico_ERS = estadistico, Valor_crítico_5pct = valor_critico_5pct, Decisión = decision)})
  
  bind_rows(resultados)
}

test_kpss <- function(ts_data) {
  resultados <- lapply(colnames(ts_data), function(nombre) {
    serie <- ts_data[, nombre]
    test <- ur.kpss(serie, type = "tau", lags = "short")
    
    estadistico <- round(test@teststat, 3)
    valor_critico_5pct <- test@cval[1, "5pct"]
    decision <- ifelse(estadistico > valor_critico_5pct, "Rechazo H0 (NO estacionaria)", "No rechazo H0 (es estacionaria)")
    
    tibble(Variable = nombre, Estadístico_KPSS = estadistico, Valor_crítico_5pct = valor_critico_5pct, Decisión = decision)})
  
  bind_rows(resultados)
}

adf_v2_chile <- test_adf(var_chile_diff)
ers_chile <- test_ers(var_chile_diff)
kpss_chile <- test_kpss(var_chile_diff)
pp_chile <- test_pp(var_chile_diff)

adf_v2_mexico <- test_adf(var_mexico_diff)
ers_mexico <- test_ers(var_mexico_diff)
kpss_mexico <- test_kpss(var_mexico_diff)
pp_mexico <- test_pp(var_mexico_diff)

#Salvo por las variables que vendrían a ser un proxy del IPC núcleo (en ambos países), pareciera que diferenciar solo una
#vez es suficiente para volver estacionarias a las series

rm(list = setdiff(ls(), c("var_chile_diff", "var_mexico_diff", "var_internacionales")))