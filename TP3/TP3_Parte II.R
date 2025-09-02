library(tidyverse)
library(readxl)
library(lubridate)
library(zoo)
library(fixest)
library(purrr)
library(ggplot2)

Sys.setlocale("LC_TIME", "es_ES.UTF-8")  # NO PODÍA GENERAR LAS FECHAS


source("TP3_Parte I.R")
# _________________________________________________
# Abro series, esto está en el git (https://github.com/Ninadcp/MacroEconometria/tree/b6513256350fa4b2fa1b8c5f7deed5d9328b6940/TP3)
# _________________________________________________

imacec <- read_excel("datos/IMACEC.xlsx", skip = 2)

tcn <- read_excel("datos/TCN_chile.xlsx", skip = 2) 

ipc <- read_excel("datos/IPC SAE.xlsx", skip = 2)

embi <- read_excel("datos/EMBI.xlsx", skip = 2) %>%
  transmute(Periodo, embi = `Chile`) # borro lo de mx

tot <- read_excel("datos/tot_com.xlsx", sheet = "Data")  %>%
  select(-País) %>%                     # elimino columna "País"
  pivot_longer(
    cols = everything(),                
    names_to = "Periodo",
    values_to = "tot"
  ) %>%                               # estaba con el formato d fecha d Excel
  mutate(
    Periodo = as.Date(as.numeric(Periodo), origin = "1899-12-30"), 
    tot = as.numeric(tot)
  ) %>%
  arrange(Periodo)

tpm <- read_excel("datos/TPM_chile.xlsx", skip = 2) 

# Tasas bancarias
tasas <- read_excel("datos/tasas.xls", sheet = "TIP colocación y plazo", skip = 8) %>%                                   # completa el año hacia abajo
  drop_na(Mes)  %>%
  fill(Año)  %>%
  mutate(Periodo = as.Date(as.yearmon(paste(Año, Mes), "%Y %B")))%>%
  transmute(Periodo, tasa_bancaria = `Comercial`)

# UNO EN UN SÓLO DF

macro_cl <- list(imacec, ipc, tcn, embi, tot, tpm, tasas) %>%
  reduce(full_join, by = "Periodo") %>%
  arrange(Periodo)

macro_cl <- macro_cl %>% 
  drop_na() %>% # No todas las series arrancan al mismo tiempo
  rename( 
    date = Periodo,
    imacec = Imacec,
    ipc = IPC_SAE,
    tc_nominal = TCN,
    embi_ch = embi,
    tot = tot,
    tpm = TPM
    ) %>% #rename así queda más cómodo
  mutate(
    imacec_g = 100 * (log(imacec) - lag(log(imacec))),
    tc_g     = 100 * (log(tc_nominal) - lag(log(tc_nominal))),
    infl     = 100 * (log(ipc) - lag(log(ipc))),
    tot_g    = 100 * (log(tot) - lag(log(tot)))
  )
df_monthly <- df_monthly %>%
  mutate(date = as.Date(month)) %>%   # convertir yearmon a Date
  select(-month)
# JUNTO CON LA PARTE 1
df_all <- df_monthly %>%
  left_join(macro_cl, by = "date") %>%
  drop_na()
# _________________________________________________
# GRAFICO 
# _________________________________________________
library(ggplot2)

plot_var_vs_shock <- function(df, var, var_label) {
  ggplot(df, aes(x = date)) +
    geom_line(aes(y = .data[[var]]), color = "#2C7BB6") +
    geom_line(aes(y = vf_purged), color = "#e357ff", linetype = "dashed") + 
    scale_y_continuous(
      name = var_label,
      sec.axis = sec_axis(~./10, name = "Shock vf_purged")  
    ) +
    labs(title = paste("Serie:", var_label, "y Shock vf_purged (OS)"),
         x = "Fecha") +
    theme_minimal()
}

# PARA Q GRAFIQUE
vars <- c("imacec_g", "infl", "tc_g", "embi_ch", "tot_g", "tasa_bancaria", "tpm")
labels <- c("IMACEC (%)", "Inflación (%)", "Tipo de cambio (%)", "EMBI (pb)", 
            "Términos de intercambio (%)", "Tasa bancaria (%)", "TPM (%)")
if (!dir.exists("graficos II.1")) dir.create("graficos II.1") # Creo la carpeta

for (i in seq_along(vars)) {
  p <- plot_var_vs_shock(df_all, vars[i], labels[i])
  ggsave(paste0("graficos II.1/", vars[i], ".png"), plot = p, width = 10, height = 6, dpi = 300)
}

