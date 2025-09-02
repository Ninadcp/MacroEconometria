library(tidyverse)
library(readxl)
library(seasonal)
rm(list = ls(all.names = TRUE))
gc()

wd <- setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Ruta a la carpeta de datos
inputs <- file.path(wd, "Datos")

# IMACEC
imacec <- readxl::read_excel(file.path(inputs, "IMACEC.xlsx"), skip = 2)
imacec.ts <- ts(imacec$Imacec, start = c(2002, 1), frequency = 12)
seas.adj <- seas(imacec.ts)
imacec.sa <- seas.adj$series$s11
imacec <- 100 * log(imacec.sa)
rm(imacec.ts, imacec.sa, seas.adj)

# IGAE
igae <- readxl::read_excel(file.path(inputs, "IGAE.xlsx"), sheet = "IGAE") %>%
  dplyr::filter(Fecha >= as.Date("2002-01-01"))
igae.ts <- ts(igae$Índice, start = c(2002, 1), frequency = 12)
seas.adj <- seas(igae.ts)
igae.sa <- seas.adj$series$s11
igae <- 100 * log(igae.sa)
rm(igae.ts, igae.sa, seas.adj)

# IPC núcleo - Chile
ipc_sae <- readxl::read_excel(file.path(inputs, "IPC SAE.xlsx"), skip = 1) %>%
  dplyr::select(-Var)
ipc_sae.ts <- ts(ipc_sae$IPC_SAE, start = c(2002, 1), frequency = 12)
seas.adj <- seas(ipc_sae.ts)
ipc_sae.sa <- seas.adj$series$s11
ipc_sae <- 100 * log(ipc_sae.sa)
rm(ipc_sae.ts, ipc_sae.sa, seas.adj)

# INPC subyacente - México (con suppressWarnings xq sino m tiraba warnings)
inpc <- readxl::read_excel(file.path(inputs, "INPC.xlsx"), skip = 12) %>%
  dplyr::filter(!is.na(suppressWarnings(as.numeric(Fecha)))) %>%
  dplyr::mutate(Fecha = as.Date(as.numeric(Fecha), origin = "1899-12-30")) %>%
  dplyr::select(Fecha, F865542) %>%
  dplyr::rename(ipcn_subyacente = F865542) %>%
  na.omit()
inpc.ts <- ts(inpc$ipcn_subyacente, start = c(2002, 1), frequency = 12)
seas.adj <- seas(inpc.ts)
inpc.sa <- seas.adj$series$s11
inpc <- 100 * log(inpc.sa)
rm(inpc.ts, inpc.sa, seas.adj)

# Tipo de cambio nominal
tcn_chile <- readxl::read_excel(file.path(inputs, "TCN_chile.xlsx"), skip = 2) %>%
  dplyr::pull(TCN) %>%
  ts(start = c(2002, 1), frequency = 12) %>%
  log() %>%
  `*`(100)

tcn_mexico <- readxl::read_excel(file.path(inputs, "TCN_mexico.xlsx"), skip = 17) %>%
  dplyr::select(Fecha, TCN = SF328) %>%
  dplyr::pull(TCN) %>%
  ts(start = c(2002, 1), frequency = 12) %>%
  log() %>%
  `*`(100)

# Tasa de Política Monetaria
tpm_chile <- readxl::read_excel(file.path(inputs, "TPM_chile.xlsx"), skip = 2) %>%
  dplyr::pull(TPM) %>%
  ts(start = c(2002, 1), frequency = 12)

tpm_mexico <- readxl::read_excel(file.path(inputs, "TPM_mexico.xlsx"), sheet = "Hoja3") %>%
  dplyr::select(Fecha = `Fecha...1`, TPM) %>%
  dplyr::pull(TPM) %>%
  ts(start = c(2002, 1), frequency = 12)

# EMBI
embi <- readxl::read_excel(file.path(inputs, "EMBI.xlsx"), skip = 2)
embi$Chile <- embi$Chile / 100
embi$México <- embi$México / 100
embi_chile <- ts(embi$Chile, start = c(2002, 1), frequency = 12)
embi_mexico <- ts(embi$México, start = c(2002, 1), frequency = 12)
rm(embi)

# Wu-Xia Shadow Rate
shadow_rate <- readxl::read_excel(file.path(inputs, "WuXiaShadowRate.xlsx"), sheet = "Data") %>%
  dplyr::select(-...3, -...4) %>%
  dplyr::rename(wu_xia = `Wu-Xia shadow federal funds rate`) %>%
  dplyr::filter(Fecha >= as.Date("2002-01-01")) %>%
  dplyr::pull(wu_xia) %>%
  ts(start = c(2002, 1), frequency = 12)

# Inflación mensual EE.UU.
us_cpi <- readxl::read_excel(file.path(inputs, "CPIAUCNS.xlsx"), sheet = "Monthly") %>%
  dplyr::mutate(inflation = c(NA, diff(log(CPIAUCNS)) * 100)) %>%
  dplyr::filter(observation_date >= as.Date("2002-01-01"))
uspi.ts <- ts(us_cpi$inflation, start = c(2002, 1), frequency = 12)
seas.adj <- seas(uspi.ts)
uspi_sa.ts <- seas.adj$series$s11
rm(us_cpi, uspi.ts, seas.adj)

# Índice de actividad global - Killian
kilian_index <- readxl::read_excel(file.path(inputs, "igrea.xlsx")) %>%
  dplyr::filter(Date >= as.Date("2002-01-01")) %>%
  dplyr::pull(Index) %>%
  ts(start = c(2002, 1), frequency = 12)

# Excess Bond Premium
ebp <- read.csv(file.path(inputs, "ebp.csv")) %>%
  dplyr::filter(date >= as.Date("2002-01-01")) %>%
  dplyr::pull(ebp) %>%
  ts(start = c(2002, 1), frequency = 12)

# Términos de intercambio - Chile
chile_tot <- readxl::read_excel(file.path(inputs, "Commodity_Terms_of_Trade_Chile.xlsx"), sheet = "Data") %>%
  tidyr::pivot_longer(cols = -País, names_to = "Fecha", values_to = "Indice") %>%
  dplyr::mutate(Fecha = as.Date(as.numeric(Fecha), origin = "1899-12-30")) %>%
  dplyr::filter(!is.na(Fecha) & Fecha >= as.Date("2002-01-01")) %>%
  dplyr::pull(Indice) %>%
  ts(start = c(2002, 1), frequency = 12)

# Términos de intercambio - México
mexico_tot <- readxl::read_excel(file.path(inputs, "Commodity_Terms_of_Trade_México.xlsx"), sheet = "Data") %>%
  tidyr::pivot_longer(cols = -País, names_to = "Fecha", values_to = "Indice") %>%
  dplyr::mutate(Fecha = as.Date(as.numeric(Fecha), origin = "1899-12-30")) %>%
  dplyr::filter(!is.na(Fecha) & Fecha >= as.Date("2002-01-01")) %>%
  dplyr::pull(Indice) %>%
  ts(start = c(2002, 1), frequency = 12)

# Juntamos todo
var_chile <- cbind(imacec, ipc_sae, tcn_chile, embi_chile, tpm_chile)
var_chile <- window(var_chile, start = c(2002, 1), end = c(2023, 6))

var_mexico <- cbind(igae, inpc, tcn_mexico, embi_mexico, tpm_mexico)
var_mexico <- window(var_mexico, start = c(2002, 1), end = c(2023, 6))

var_internacionales <- cbind(shadow_rate, uspi_sa.ts, kilian_index, ebp, chile_tot, mexico_tot)
var_internacionales <- window(var_internacionales, start = c(2002, 1), end = c(2023, 6))

# Limpiamos entorno, dejando solo las variables principales
rm(list = setdiff(ls(), c("var_chile", "var_mexico", "var_internacionales")))