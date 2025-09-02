#promedio de SMDI de los 4 trimestres anteriores
datos_hp_cycle <- datos[, c("smdi", "gdp_socios", "gdp_agro_pc", "gdp_resto_pc", 
                            "employment", "cons_pc", "invest_pc", "reer")]
smdi <- datos_hp_cycle[, "smdi"]

smdi_lag1 <- stats::lag(smdi, -1)
smdi_lag2 <- stats::lag(smdi, -2)
smdi_lag3 <- stats::lag(smdi, -3)
smdi_lag4 <- stats::lag(smdi, -4)

smdi4 <- window((smdi_lag1 + smdi_lag2 + smdi_lag3 + smdi_lag4) / 4, 
                start = c(2006, 1), end = c(2023, 4), frequency = 4)

#Dummys St: 1 si hay estado húmedo, 0 si estado seco
St <- as.numeric(smdi4 > 0)

#AR(2) sobre SMDI4 
AR <- arima(smdi4, order = c(2, 0, 0), include.mean = TRUE)
epsilon <- residuals(AR)
#no uso los res
epsilon <- smdi4


#Shocks diferenciados
sigma1 <- sd(epsilon[St == 1])
sigma2 <- sd(epsilon[St == 0])

eps_humedo <- St * epsilon / sigma1
eps_seco   <- (1 - St) * epsilon / sigma2

#armamos base con shocks diferenciados
datos <- as.matrix(cbind(datos_hp_cycle, eps_humedo = eps_humedo, eps_seco = eps_seco))
colnames(datos) <- c(colnames(datos_hp_cycle), "eps_humedo", "eps_seco")

rm(list = setdiff(ls(), c("datos", "St", "p_HQC", "H")))

#Grafico IRF (same código q luca's del punto 7)
source("~/Downloads/PS3_LP_Tools.R")
titulos <- list(
  smdi           = "SMDI",
  gdp_socios     = "PIB de Socios Comerciales",
  gdp_agro_pc    = "PIB Agropecuario per cápita",
  gdp_resto_pc   = "PIB Resto per cápita",
  employment     = "Empleo",
  cons_pc        = "Consumo per cápita",
  invest_pc      = "Inversión per cápita",
  reer           = "Tipo de Cambio Real Multilateral"
)


p_HQC <- 1
H <- 20
for (var in names(titulos)) {
  idx.r <- which(colnames(datos) == var)
  
  res <- lp.pw(
    Y = datos[, 1:8],
    D = St,
    p = p_HQC,
    idx.s = 1,
    idx.r = idx.r,
    H = H,
    gamma = 0.95
  )
  
  df_plot <- tibble(h = 0:H, Shock = "Seco", pe = res$pe[, "1-D"], lb = res$lb[, "1-D"], ub = res$ub[, "1-D"], variable = titulos[[var]]) %>%
    bind_rows(tibble(h = 0:H, Shock = "Húmedo", pe = res$pe[, "D"], lb = res$lb[, "D"], ub = res$ub[, "D"], variable = titulos[[var]]))
  
  g <- ggplot(df_plot, aes(x = h, y = pe, color = Shock, fill = Shock)) +
    geom_line(linewidth = 1) +
    geom_ribbon(aes(ymin = lb, ymax = ub), alpha = 0.5, color = NA) +
    facet_wrap(~ Shock, scales = "free_y") +
    scale_color_manual(values = c("Seco" = "purple", "Húmedo" = "blue")) +
    scale_fill_manual(values = c("Seco" = "purple", "Húmedo" = "blue")) +
    labs(x = "Horizonte (trimestres)", y = "Respuesta estimada (%)", title = titulos[[var]]) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          axis.title = element_text(face = "bold"),
          strip.text = element_text(color = "black", face = "bold", size = 11),
          legend.position = "bottom")
  
  print(g)
  ggsave(paste0("Gráficos/", var, "_shock_humedo_seco_nORES.png"), g, width = 10, height = 5, dpi = 300)
}

