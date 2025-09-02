library(tidyverse)
library(vars)
library(ggplot2)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls(all.names = TRUE))
gc()

source("TP1_UR.R")

# Ordenamos las variables domésticas según el orden recursivo que queremos probar
# Alternativa 1: TPM primero
var_chile_diff <- var_chile_diff[, c("tpm_chile", "imacec", "ipc_sae", "tcn_chile", "embi_chile")]
var_mexico_diff <- var_mexico_diff[, c("tpm_mexico", "igae", "inpc", "tcn_mexico", "embi_mexico")]

# Renombramos columnas para usar nombres genéricos
colnames(var_chile_diff) <- c("tpm", "imacec", "ipc_sae", "tcn", "embi")
colnames(var_mexico_diff) <- c("tpm", "igae", "inpc", "tcn", "embi")

# Elección del número de lags usando HQIC
p_chile <- VARselect(var_chile_diff, lag.max = 12, type = "const")
p_mex <- VARselect(var_mexico_diff, lag.max = 12, type = "const")

cat("Lag Chile según HQIC:", p_chile$selection[2], "| Lag México según HQIC:", p_mex$selection[2], "\n")

p_chile <- p_chile$selection[2]
p_mex <- p_mex$selection[2]

# Estimamos los VARs sin restricciones (no hay internacionales)
VAR_chile <- VAR(var_chile_diff, p = p_chile, type = "const")
VAR_mex <- VAR(var_mexico_diff, p = p_mex, type = "const")

# SVAR

# CHILE
m_c <- VAR_chile$K
T_c <- VAR_chile$obs

Amat_c <- diag(m_c)
for (i in 2:m_c) {
  for (j in 1:(i - 1)) {
    Amat_c[i, j] <- NA
  }
}

Bmat_c <- matrix(0, m_c, m_c)
for (i in 1:m_c) {
  Bmat_c[i, i] <- NA
}

SVAR_chile <- SVAR(VAR_chile, Amat = Amat_c, Bmat = Bmat_c, lrtest = FALSE)

# MÉXICO
m_m <- VAR_mex$K
T_m <- VAR_mex$obs

Amat_m <- diag(m_m)
for (i in 2:m_m) {
  for (j in 1:(i - 1)) {
    Amat_m[i, j] <- NA
  }
}

Bmat_m <- matrix(0, m_m, m_m)
diag(Bmat_m) <- NA

SVAR_mex <- SVAR(VAR_mex, Amat = Amat_m, Bmat = Bmat_m, lrtest = FALSE)

source("PS2_SVAR_Tools.R")
source("PS2_SVAR_Plots.R")
source("TP1_Plot_IRFs.R")

# Parámetros de IRF bootstrap
H <- 12
R <- 1000
type <- "nonparametric"
gamma <- 0.95

# -------------- chile -------------- 
p <- p_chile
m <- m_c
T <- T_c
VAR <- VAR_chile
SVAR <- SVAR_chile
VAR_model <- VAR

S <- t(resid(VAR)) %*% resid(VAR) / (T - m * p - 1)
P.chol <- t(chol(S))

P <- solve(SVAR$A, SVAR$B) # inv(A) %*% B
S.SVAR <- P %*% t(P)

pars.R <- Bcoef(VAR) # Reduced Form VAR
pars.S <- solve(SVAR$A, pars.R) # Structural Form VAR

IRF <- SVAR.sirf(SVAR, H)
plot.sirf <- function(X, m, H, filename = NULL) {
  
  # IA: Si se pasa un nombre de archivo, se guarda como imagen PNG
  if (!is.null(filename)) {
    png(filename, width = 1200, height = 800, res = 150)  # IA
  }
  
  # IA: Cambiamos el layout de los gráficos para que se adapte mejor al espacio
  par(mfrow = c(ceiling(sqrt(m)), ceiling(sqrt(m))))  # IA
  
  for (i in 1:m) {
    for (j in 1:m) {
      plot(0:H, X[i, j, ],
           main = paste("Response of", dimnames(X)[[1]][i], "to", dimnames(X)[[2]][j], "shock"),
           xlab = "Horizon", ylab = "",
           type = "o", lwd = 2)
      grid(NULL, NULL, lty = 1)
    }
  }
  
  # IA: Cerramos el dispositivo gráfico si estamos exportando
  if (!is.null(filename)) dev.off()  # IA
}
plot.sirf(IRF, m, H, filename = "Gráficos/IRF_exportado.png")

chile.boot <- boot.replicate(VAR, R, type)
IRF.boot_chile <- SVAR.sirf.boot(SVAR, Amat = Amat_c, Bmat = Bmat_c, H, gamma, chile.boot)
quartz()
plot.sirf.boot(IRF.boot_chile, m , H)

var_names <- dimnames(IRF.boot_chile$pe)[[1]]
dimnames(IRF.boot_chile$pe)[[1]] <- colnames(var_chile_diff)
dimnames(IRF.boot_chile$pe)[[2]] <- colnames(var_chile_diff)
dimnames(IRF.boot_chile$lb)[[2]] <- colnames(var_chile_diff)
dimnames(IRF.boot_chile$ub)[[2]] <- colnames(var_chile_diff)

labels_chile <- c("TPM"= "Tasa de política Monetaria", "IMACEC" = "IMACEC", "IPC_SAE" = "IPC (SAE)",
                  "TCN" = "Tipo de Cambio Nominal", "EMBI" = "EMBI")

var_domesticas_chile <- c("TPM", "IMACEC", "IPC_SAE", "TCN", "EMBI")

chile_irf <- create_irf_boot_df(IRF.boot_chile, H)
plot_irf(subset(chile_irf, response %in% var_domesticas_chile & shock == "TPM"), "TPM", "Chile", 
         response_labels = labels_chile, shock_labels = c("TPM" = "TPM"))
ggsave("Gráficos/IRF_Chile_v3.png", width = 20, height = 16, units = "cm", dpi = 300)
