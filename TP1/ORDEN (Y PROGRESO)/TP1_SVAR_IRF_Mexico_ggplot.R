
#===============================================================================
# Trabajo Práctico 1 - SVAR + IRF con gráficos ggplot2 - VERSIÓN 20/07
#===============================================================================

library(tidyverse)
library(vars)

# Configuración inicial
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls(all.names = TRUE))
gc()

# Cargar scripts
source("TP1_2.R")
source("PS2_SVAR_Tools.R")
source("PS2_SVAR_Plots.R")

# Elección de lags
lag_select <- VARselect(Y, lag.max = xx, type = "const")
lags <- lag_select$selection[2]

Y <- Y[(pmax - p + 1):nrow(Y), ]  # Ajustar según tu corte

# Estimar VAR reducido
VAR <- VAR(Y, p = p, type = "const")
m <- VAR$K
T <- VAR$obs

# Restricciones manuales
matC <- function(m, p, vx) {
  vy <- setdiff(1:m, vx)
  Cm <- matrix(1, m, m * p + 1)
  for (i in vx) {
    for (l in 1:p) {
      for (j in vy) {
        Cm[i, m * (l - 1) + j] <- 0
      }
    }
  }
  Cm
}
constraints <- matC(m, lags, 1)
VAR <- restrict(VAR, method = "man", resmat = constraints)

# Matrices A y B
Amat <- diag(m)
for (i in 2:m) {
  for (j in 1:(i - 1)) {
    Amat[i, j] <- NA
  }
}
Bmat <- matrix(0, m, m)
diag(Bmat) <- NA

# Estimar SVAR
SVAR <- SVAR(VAR, Amat = Amat, Bmat = Bmat, lrtest = FALSE)

# Bootstrap IRFs
H <- 12
mexico.boot <- boot.replicate(VAR, 1000, "nonparametric")
IRF.boot <- SVAR.sirf.boot(SVAR, Amat, Bmat, H, 0.95, mexico.boot)

# Preparar IRFs para ggplot
create_irf_boot_df <- function(IRF.boot, H) {
  responses <- dimnames(IRF.boot$pe)[[1]]
  shocks <- dimnames(IRF.boot$pe)[[2]]
  horizons <- 0:H

  df <- expand.grid(response = responses, shock = shocks, horizon = horizons)
  df$pe <- as.vector(IRF.boot$pe)
  df$lb <- as.vector(IRF.boot$lb)
  df$ub <- as.vector(IRF.boot$ub)
  return(df)
}

plot_irf <- function(df_irf, shock_name, country) {
  df_plot <- df_irf %>% filter(shock == shock_name)

  ggplot(df_plot, aes(x = horizon, y = pe)) +
    geom_ribbon(aes(ymin = lb, ymax = ub), fill = "grey80", alpha = 0.75) +
    geom_line(color = "black", linewidth = 1) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey") +
    facet_wrap(~response, scales = "free_y", ncol = 3, nrow = 2) +
    theme_minimal(base_size = 14) +
    labs(x = "Horizonte", y = "Respuesta", 
         title = paste("IRF al shock en", shock_name, "—", country), 
         caption = "IC al 95% (Bootstrap)") +
    theme(panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5, face = "bold"))
}

# Graficar
df_irf <- create_irf_boot_df(IRF.boot, H)
plot_irf(df_irf, "PCOM", "México")
