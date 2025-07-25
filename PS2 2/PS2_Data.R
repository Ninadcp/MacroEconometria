#------------------------------------------------------------------------------#
# Maestría en Economía
# Macroeconometría
# 2025, 2do trimestre 
# Profesor: Javier Garcia-Cicco
# Tutor: Franco Nu?ez

# Material basado en código de Luis Libonatti (usado en versiones anteriores de 
# la materia)
#------------------------------------------------------------------------------#
library(readxl)

# Commodity Price Index ####
# We are going to use a temporal file
pcom.file <- paste(tempfile(), ".ashx", sep = "")
download.file("https://www.imf.org/-/media/Files/Research/CommodityPrices/Monthly/external-data.ashx", pcom.file, mode = "wb")
# Note: the IMF has changed this link in the past, for example:
# download.file("https://www.imf.org/-/media/Files/Research/CommodityPrices/Monthly/external-dataseptember.ashx", pcom.file, mode = "wb")
# download.file("https://www.imf.org/external/np/res/commod/external_data.xls", pcom.file, mode = "wb")
# download.file("https://www.imf.org/-/media/Files/Research/CommodityPrices/Monthly/external-dataaugust.ashx", pcom.file, mode = "wb")

pcom <- read_excel(pcom.file, skip = 3, sheet = 1)
pcom <- as.numeric(pcom$Monthly...2)

pcom <- pcom[complete.cases(pcom)] # delete NAs
pcom <- ts(pcom, start = c(2003, 01), frequency = 12) # time-series format

remove(pcom.file)

# Nominal Exchange Rate (USD/ARS) ####
#tcn.file <- paste(tempfile(), ".xls", sep = "")
#download.file("http://www.bcra.gov.ar/Pdfs/PublicacionesEstadisticas/com3500.xls", tcn.file, mode = "wb")



er <- read_excel("com3500.xls", skip = 1, sheet = 2)
er <- as.numeric(er$`Tipo de cambio nominal promedio mensual`)

er <- er[complete.cases(er)] # delete NAs
er <- ts(er, start = c(2002, 03), frequency = 12)

#remove(tcn.file)

# Consumer Price Index ####

# Historic CPI
ipc <- read.csv(url("https://apis.datos.gob.ar/series/api/series/?ids=178.1_NL_GENERAL_0_0_13&limit=5000&format=csv"))
ipc <- ts(ipc$nivel_general, start = c(1943, 01), frequency = 12)
ipc <- diff(log(ipc))
ipc <- window(ipc, end = c(2006, 12))

# CPI, Province of San Luis 
ipc.sl <- read.csv(url("https://apis.datos.gob.ar/series/api/series/?ids=197.1_NIVEL_GENERAL_2014_0_13&limit=5000&format=csv"))
ipc.sl <- ts(ipc.sl$nivel_general, start = c(2005, 10), frequency = 12)
ipc.sl <- diff(log(ipc.sl))
ipc.sl <- window(ipc.sl, start = c(2007, 01), end = c(2012, 07))

# CPI, City of Buenos Aires
ipc.ba <- read.csv(url("https://apis.datos.gob.ar/series/api/series/?ids=193.1_NIVEL_GENERAL_JULI_0_13&limit=5000&format=csv"))
ipc.ba <- ts(ipc.ba$nivel_general, start = c(2012, 07), frequency = 12)
ipc.ba <- diff(log(ipc.ba))
ipc.ba <- window(ipc.ba, end = c(2016, 04))

# CPI, Greater Buenos Aires (INDEC)
ipc.gba <- read.csv(url("https://apis.datos.gob.ar/series/api/series/?ids=101.1_I2NG_2016_M_22&limit=5000&format=csv"))
ipc.gba <- ts(ipc.gba$ipc_2016_nivel_general, start = c(2016, 04), frequency = 12)
ipc.gba <- diff(log(ipc.gba))
ipc.gba <- window(ipc.gba, end = c(2016, 12))

# CPI, National (INDEC)
ipc.arg <- read.csv(url("https://apis.datos.gob.ar/series/api/series/?ids=145.3_INGNACNAL_DICI_M_15&limit=5000&format=csv"))
ipc.arg <- ts(ipc.arg$ipc_ng_nacional, start = c(2016, 12), frequency = 12)
ipc.arg <- diff(log(ipc.arg))

pc <- c(ipc, ipc.sl, ipc.ba, ipc.gba, ipc.arg)
pc <- c(1, cumprod(exp(pc)))
pc <- ts(pc, start = c(1943, 01), frequency = 12)
pc <- 100 * (pc / mean(tail(pc, 12)))

remove(ipc, ipc.sl, ipc.ba, ipc.gba, ipc.arg)

# Final Series ####
pcom <- window(pcom, start = c(2002, 03), end = c(2019, 12))
er <- window(er, start = c(2002, 03), end = c(2019, 12))
pc <- window(pc, start = c(2002, 03), end = c(2019, 12))