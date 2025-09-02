library(tidyverse)
library(readxl)
library(lubridate)
library(zoo)
library(bsvarSIGNs)

#Seteo directorio, limpio el environment y bajo el df
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls(all.names = TRUE))

#Cargamos los datos
finshocks <- read_excel("finshocks_public.xlsx", sheet = "daily-level")
ebp <- read_excel("excess-bond-premium-data.xlsx", sheet = "ebp") %>% 
  mutate(var_ebp = lead(ebp_agg, 1) - lag(ebp_agg, 1))

#Creo el dataset que combina ambas bases
df <- finshocks %>% 
  left_join(ebp, by = "date") %>% 
  dplyr::select(date, vf_outside, vf_purged, var_ebp) %>% 
  filter(date >= "2002-07-10")

rm(ebp, finshocks)

#BVAR con restricción de signo

#Pasamos las series a formato time-series
vf_outside_ts <- ts(df$vf_outside , start = c(2002, 07, 11), frequency = 30)
var_ebp_ts <- ts(df$var_ebp , start = c(2002, 07, 11), frequency = 30)

Y <- cbind(vf_outside_ts, var_ebp_ts)

#Especificamos el modelo VAR(1)
set.seed(1234)
sign_irf <- matrix(c(1, -1, 1, 1), 2, 2)  # restricciones de signo

spec <- specify_bsvarSIGN$new(data = Y, p = 1, sign_irf = sign_irf)

#Priors tight para tratar los datos como i.i.d.
spec$prior$lambda.shape <- 0.35
spec$prior$A[1, 1] <- 0
spec$prior$A[2, 2] <- 0

#Estimamos posterior
S <- 1500 
burn <- 150
spec$prior$estimate_hyper(S = S, burn_in = burn, mu = TRUE, delta = TRUE, lambda = TRUE, psi = TRUE)

posterior <- estimate(spec, S = S)

#Extraemos shocks estructurales
shocks <- compute_structural_shocks(posterior)

#************************************************************************************************************

#Alternativamente... (lo que nos compartieron) (dan lo mismo)

n_shocks <- dim(shocks)[1]
n_periods <- dim(shocks)[2]

summarize_shock <- function(shock_matrix) {
  
  summary_df <- t(apply(shock_matrix , 1, function(x) {quantile(x, probs = c(0.05 , 0.16 , 0.5, 0.84 , 0.95))}))
  summary_df <- as.data.frame(summary_df)
  
  colnames(summary_df) <- c("q05", "q16", "median", "q84", "q95")
  
  summary_df$t <- 1:nrow(summary_df)
  summary_df <- summary_df[, c("t", "q05", "q16", "median", "q84", "q95")]
  
  return(summary_df)
}

shock_summaries <- list()
for (i in 1:n_shocks) {
  shock_summaries [[i]] <- summarize_shock(shocks[i, ,])
}

df$vf_bvar <- c(NA, shock_summaries[[1]]["median"])

#************************************************************************************************************

#Serie final: tomamos la mediana
shock_bvar <- apply(shocks[1, , ], 1, median)  # primer shock = oferta
df$vf_bvar <- c(NA, shock_bvar)

#Poor man’s sign restriction
df <- df %>%
  mutate(vf_poor = ifelse(sign(vf_outside) != sign(var_ebp), vf_outside, 0), date = as.Date(date))

rm(list = setdiff(ls(), c("df")))

#Graficamos
df %>%
  select(date, vf_purged, vf_poor, vf_bvar) %>%
  tidyr::pivot_longer(-date, names_to = "series", values_to = "value") %>% 
  ggplot(aes(x = date, y = value, color = series)) +
  geom_line() +
  labs(title = "Shocks de oferta de crédito - Frecuencia diaria", x = "Fecha", y = "Shock estimado", color = "Serie") +
  scale_color_manual(values = c("vf_purged" = "red", "vf_bvar" = "blue", "vf_poor" = "green"),
  labels = c("vf_purged" = "vf_purged", "vf_bvar" = "BVAR", "vf_poor" = "Poor man's"), 
  breaks = c("vf_purged", "vf_bvar", "vf_poor")) +
  scale_x_date(breaks = seq(as.Date("2005-01-01"), max(df$date), by = "5 years"), date_labels = "%b %Y") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), legend.position = "bottom", 
        strip.text = element_text(color = "black", face = "bold", size = 11))
ggsave("serie_diaria.png", width = 14, height = 16, units = "cm", dpi = 300)

#Correlaciones
cor(df %>% select(vf_purged, vf_poor, vf_bvar), use = "pairwise.complete.obs")

#Frecuencia mensual

df %>%
  mutate(month = as.yearmon(date)) %>%
  group_by(month) %>%
  summarise(across(c(vf_purged, vf_poor, vf_bvar), ~ sum(., na.rm = TRUE))) %>%
  ungroup() %>%
  complete(month = seq(min(month), max(month), by = 1/12), fill = list(vf_purged = 0, vf_poor = 0, vf_bvar = 0))

#Graficamos
df_monthly %>%
  tidyr::pivot_longer(-month, names_to = "series", values_to = "value") %>% 
  ggplot(., aes(x = month, y = value, color = series)) +
  geom_line() +
  labs(title = "Shocks de oferta de crédito - Frecuencia mensual", x = "Mes", y = "Shock estimado", color = "Serie") +
  scale_color_manual(values = c("vf_purged" = "red", "vf_bvar" = "blue", "vf_poor" = "green"),
                     labels = c("vf_purged" = "vf_purged", "vf_bvar" = "BVAR", "vf_poor" = "Poor man's"), 
                     breaks = c("vf_purged", "vf_bvar", "vf_poor")) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), legend.position = "bottom", 
        strip.text = element_text(color = "black", face = "bold", size = 11))
ggsave("serie_mensual.png", width = 14, height = 16, units = "cm", dpi = 300)

#Correlaciones
cor(df_monthly %>% filter(vf_purged > 0) %>% select(vf_purged, vf_poor, vf_bvar))

rm(list = setdiff(ls(), c("df_monthly")))