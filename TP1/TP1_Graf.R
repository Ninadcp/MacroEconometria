library(tidyverse)
library(vars)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

rm(list = ls(all.names = TRUE))
gc()

source("TP1_2.R")

#Elección del número de lags

lag_select <- VARselect(Y, lag.max = xx, type = "const")
lags <- lag_select$selection[2] # HQIC

Y <- Y[(pmax - p + 1):nrow(Y), ] # Starting in Jan-05  ?

VAR <- VAR(Y, p = p, type = "const")

m <- VAR$K
T <- VAR$obs

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

#Model Checking
roots(VAR, modulus = TRUE)

serial.test(VAR, lags.bg = 6, type = "ES")

#SVAR

#A Matrix
Amat <- diag(m)
for (i in 2:m) {
  for (j in 1:(i - 1)) {
    Amat[i, j] <- NA
  }
}

#B Matrix
Bmat <- matrix(0, m, m)
for (i in 1:m) {
  Bmat[i, i] <- NA
}

SVAR <- SVAR(VAR, Amat = Amat, Bmat = Bmat, lrtest = FALSE)

#*******************************************************************************************************

#SVAR Impact Matrix (Cholesky decomposition)
S <- t(resid(VAR)) %*% resid(VAR) / (T - m * p - 1)
P.chol <- t(chol(S))

#SVAR Impact Matrix (implied by AB model)
P <- solve(SVAR$A, SVAR$B)
S.SVAR <- P %*% t(P)

#Matriz de impacto contemporánea. Esperamos que tenga 0s de forma triangular inferior

#*******************************************************************************************************

source("PS2_SVAR_Tools.R")
source("PS2_SVAR_Plots.R")

H <- 12

chile.boot <- boot.replicate(VAR_chile, 1000, "nonparametric")
mexico.boot <- boot.replicate(VAR_mex, 1000, "nonparametric")

IRF.boot_chile <- SVAR.sirf.boot(SVAR_chile, Amat, Bmat, H, 0.95, chile.boot)
IRF.boot_mex <- SVAR.sirf.boot(SVAR_mex, Amat, Bmat, H, 0.95, mexico.boot)

#Ajustar esto...
var_names <- dimnames(IRF.boot_chile$pe)[[1]]
dimnames(IRF.boot$pe)[[2]] <- var_names
dimnames(IRF.boot$lb)[[2]] <- var_names
dimnames(IRF.boot$ub)[[2]] <- var_names

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
    geom_line(color = "black", size = 1) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey") +
    facet_wrap(~response, scales = "free_y", ncol = 3, nrow = 2) +
    theme_minimal(base_size = 14) +
    labs(x = "Horizonte", y = "Respuesta", title = paste("IRF al shock en", shock_name, "—", country), 
         caption = "IC al 95% (Bootstrap)") +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5, face = "bold"))
}

#Chile

chile_irf <- create_irf_boot_df(IRF.boot, H)
plot_irf(chile_irf, "PCOM", "Chile")

#México

mexico_irf <- create_irf_boot_df(IRF.boot, H)
plot_irf(mexico_irf, "PCOM", "México")

#*********************************************************************************************************************

var_names <- dimnames(IRF.boot$pe)[[1]]
dimnames(IRF.boot$pe)[[2]] <- var_names
dimnames(IRF.boot$lb)[[2]] <- var_names
dimnames(IRF.boot$ub)[[2]] <- var_names

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

plot_irf_one_shock <- function(df_irf, shock_name, country) {
  
  df_plot <- df_irf %>% filter(shock == shock_name)
  
  ggplot(df_plot, aes(x = horizon, y = pe)) +
    geom_ribbon(aes(ymin = lb, ymax = ub), fill = "grey80", alpha = 0.75) +
    geom_line(color = "black", size = 1) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey") +
    facet_wrap(~response, scales = "free_y", ncol = 3, nrow = 2) +
    theme_minimal(base_size = 14) +
    labs(x = "Horizonte", y = "Respuesta", title = paste("IRF al shock en", shock_name, "—", country), caption = "IC al 95% (Bootstrap)") +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5, face = "bold"))
}

df_irf <- create_irf_boot_df(IRF.boot, H)
plot_irf_one_shock(df_irf, "PCOM", "Chile")