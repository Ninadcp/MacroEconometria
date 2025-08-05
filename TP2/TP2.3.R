#install.packages("mFilter")
#install.packages("seasonalview")
library(readxl)
library(dplyr)
library(seasonal)
library(mFilter)
library(vars)

#Seteo directorio, limpio el environment y bajo el df
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls(all.names = TRUE))
gc()

#importo lo otro
source("TP2.2_Procesamiento.R")

df_var <- cbind(
  smdi               = datos_sa[, "smdi"],
  gdp_socios         = datos_sa[, "gdp_socios"],
  gdp_agro           = datos_hp_cycle[, "gdp_agro_pc"],
  gdp_resto          = datos_hp_cycle[, "gdp_resto_pc"],
  cons               = datos_hp_cycle[, "cons_pc"],
  invest             = datos_hp_cycle[, "invest_pc"],
  reer               = datos_sa[, "reer"],
  employment         = datos_hp_cycle[, "employment"]
)
Y <- df_var
colnames(Y) <- c("smdi", "gdp_socios", "gdp_agro", "gdp_resto", "cons", "invest", "reer", "employment")

# Lag Order Selection
pmax <- 4 # Maximum lag order

popt <- VARselect(Y, lag.max = pmax, type = "const")
popt
p <- popt$selection[2] # HQIC

Y <- ts(Y[(pmax - p + 1):nrow(Y), ], end = end(Y), 
        frequency = frequency(Y)) # Starting in Jan-05

VAR <- VAR(Y, p = p, type = "const")

m <- VAR$K # Number of variables in the VAR
T <- VAR$obs # Number of effective sample observations, excluding "p" starting values

# Model Checking
roots(VAR, modulus = TRUE)

h.BG <- 6
serial.test(VAR, lags.bg = h.BG, type = "ES")

