# Cargar librerías necesarias
library(tidyverse)
library(lmtest)
library(sandwich)
library(ggplot2)

#-----------------------------------------------------
# 1. Definir shocks a partir de residuos del AR(2)
#-----------------------------------------------------
smdi <- datos_hp_cycle[, "smdi"]
ar_fit <- arima(smdi, order = c(2, 0, 0))  # AR(2)
epsilon_wt <- residuals(ar_fit)

# Determinar umbrales (1 SD)
sd_eps <- sd(epsilon_wt)
Dneg_large <- as.numeric(epsilon_wt < -sd_eps)
Dpos_large <- as.numeric(epsilon_wt >  sd_eps)

# Estandarización por grupo
sd_neg <- sd(epsilon_wt[Dneg_large == 1])
sd_pos <- sd(epsilon_wt[Dpos_large == 1])
sd_norm <- sd(epsilon_wt[Dneg_large == 0 & Dpos_large == 0])

eps_neg_std <- Dneg_large * epsilon_wt / sd_neg
eps_pos_std <- Dpos_large * epsilon_wt / sd_pos
eps_norm_std <- (1 - Dneg_large - Dpos_large) * epsilon_wt / sd_norm

#-----------------------------------------------------
# 2. Función LP robusta (con BW y Newey-West)
#-----------------------------------------------------
lp_hac_bw <- function(y, shock, H = 20, lags = 1) {
  T <- length(y)
  pe <- lb <- ub <- rep(NA, H + 1)
  z <- qnorm(0.975)
  
  for (h in 0:H) {
    y_lead <- y[(h + 1):T]
    x_shock <- shock[1:(T - h)]
    
    x_lags <- embed(cbind(y, shock), lags + 1)
    x_ctrl <- x_lags[, -c(1:2)]  # eliminar columnas de tiempo t
    
    y_reg <- y_lead[(lags):(length(y_lead) - 1)]
    x_reg <- cbind(1, x_shock[(lags):(length(x_shock) - 1)], x_ctrl)
    
    model <- lm(y_reg ~ x_reg - 1)
    se <- sqrt(diag(NeweyWest(model, lag = 4, prewhite = FALSE)))
    coef_est <- coef(model)[1]
    
    pe[h + 1] <- coef_est
    lb[h + 1] <- coef_est - z * se[1]
    ub[h + 1] <- coef_est + z * se[1]
  }
  
  tibble(h = 0:H, pe = pe, lb = lb, ub = ub)
}

#-----------------------------------------------------
# 3. Aplicar a una variable (ej. PIB Agro)
#-----------------------------------------------------
y <- datos_hp_cycle[, "gdp_agro_pc"]
lp_neg <- lp_hac_bw(y, eps_neg_std)
lp_pos <- lp_hac_bw(y, eps_pos_std)
lp_norm <- lp_hac_bw(y, eps_norm_std)

#-----------------------------------------------------
# 4. Plotear
#-----------------------------------------------------
df_plot <- bind_rows(
  mutate(lp_neg, Shock = "Negativo atípico"),
  mutate(lp_pos, Shock = "Positivo atípico"),
  mutate(lp_norm, Shock = "Normal")
)

ggplot(df_plot, aes(x = h, y = pe, color = Shock, fill = Shock)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = lb, ymax = ub), alpha = 0.2, color = NA) +
  labs(
    title = "Respuesta del PIB Agro per cápita a shocks climáticos",
    x = "Horizonte (trimestres)", y = "Respuesta estimada (%)"
  ) +
  scale_color_manual(values = c("Negativo atípico" = "red", "Positivo atípico" = "blue", "Normal" = "darkgreen")) +
  scale_fill_manual(values = c("Negativo atípico" = "red", "Positivo atípico" = "blue", "Normal" = "darkgreen")) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold", hjust = 0.5))
