#install.packages("mFilter")
#install.packages("seasonalview")
library(readxl)
library(dplyr)
library(seasonal)
library(mFilter)

#Seteo directorio, limpio el environment y bajo el df
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls(all.names = TRUE))
gc()
df <- read_excel("/Users/ninadicostanzopereira/Desktop/MacroMetrics/MacroEconometria/TP2/data_uruguay_tp2.xlsx")

###Creo PBI no agro y CCNN per capita
df <- df %>%
  mutate(
    gdp_resto    = gdp_total - gdp_agro,
    gdp_total_pc = gdp_total / pop,
    gdp_agro_pc  = gdp_agro / pop,
    gdp_resto_pc = gdp_resto / pop,
    cons_pc      = cons / pop,
    invest_pc    = invest / pop
  )

###Me quedo solo con las variables pc y convierto a time series
df <- df[, c("gdp_total_pc", "gdp_agro_pc", "gdp_resto_pc", "cons_pc", "invest_pc", "reer", "smdi", "gdp_socios", "employment")]
datos_ts <- ts(df, start = c(2005, 1), frequency = 4)

###Desestacionalización
datos_sa <- datos_ts
cols_no_deseason <- c("reer", "gdp_socios", "smdi") #defino cuáles no desest

for (col in colnames(datos_ts)) {
  serie <- datos_ts[, col]
  
  if (col %in% cols_no_deseason) {
    # No desestacionalizar → dejar tal cual
    datos_sa[, col] <- serie
  } else {
    # Intentar desestacionalizar con seas()
    seas.adj <- seas(serie)
    x.sa <- seas.adj$series$s11
    datos_sa[, col] <- x.sa
  }
}

### Transformar a log × 100 (excluyendo smdi)
cols_to_log <- setdiff(colnames(datos_sa), "smdi")
for (col in cols_to_log) {
  datos_sa[, col] <- log(datos_sa[, col]) * 100
}

###Filtro HP (excepto smdi)
cols_to_filter <- setdiff(colnames(datos_sa), "smdi")
datos_hp_cycle <- datos_sa  # Copiar estructura

for (col in cols_to_filter) {
  hp_result <- hpfilter(datos_sa[, col], freq = 1600)
  datos_hp_cycle[, col] <- hp_result$cycle
}
