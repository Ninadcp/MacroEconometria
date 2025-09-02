library(tidyverse)
library(vars)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

rm(list = ls(all.names = TRUE))
gc()

source("TP1_UR.R")

#Ordenamos los objetos según el orden de Cholesky de acuerdo a nuestro supuesto de identificación

var_chile_diff <- var_chile_diff[, c("imacec", "ipc_sae", "tpm_chile", "tcn_chile", "embi_chile")]
var_mexico_diff <- var_mexico_diff[, c("igae", "inpc", "tpm_mexico", "tcn_mexico", "embi_mexico")]

#Elección del número de lags

p_chile <- VARselect(var_chile_diff, lag.max = 12, type = "cons")
p_mex <- VARselect(var_mexico_diff, lag.max = 12, type = "cons")

cat("Lag Chile según HQIC:", p_chile$selection[2], "| Lag México según HQIC:", p_mex$selection[2], "\n")

p_chile <- p_chile$selection[2]
p_mex <- p_mex$selection[2]

VAR_chile <- VAR(var_chile_diff, p = p_chile, type = "cons")
VAR_mex <- VAR(var_mexico_diff, p = p_mex, type = "cons")

#Debería ser el mismo número de variables y de observaciones para ambos países, así para este paso no importa cual elija (?)

m <- VAR_chile$K
T <- VAR_chile$obs

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

SVAR_chile <- SVAR(VAR_chile, Amat = Amat, Bmat = Bmat, lrtest = FALSE)
SVAR_mex <- SVAR(VAR_mex, Amat = Amat, Bmat = Bmat, lrtest = FALSE)

#Cargamos algunas funciones que me van a servir para graficar

source("PS2_SVAR_Tools.R")
source("PS2_SVAR_Plots.R")
source("TP1_Plot_IRFs.R")

H <- 12
R <- 1000
type <- "nonparametric"
gamma <- 0.95

chile.boot <- boot.replicate(VAR_chile, R, type)
mexico.boot <- boot.replicate(VAR_mex, R, type)

IRF.boot_chile <- SVAR.sirf.boot(SVAR_chile, Amat, Bmat, H, gamma, chile.boot)
IRF.boot_mex <- SVAR.sirf.boot(SVAR_mex, Amat, Bmat, H, gamma, mexico.boot)

#Ajustamos los nombres de las variables

#Chile
var_names <- dimnames(IRF.boot_chile$pe)[[1]]
dimnames(IRF.boot_chile$pe)[[2]] <- var_names
dimnames(IRF.boot_chile$lb)[[2]] <- var_names
dimnames(IRF.boot_chile$ub)[[2]] <- var_names

#México

var_names <- dimnames(IRF.boot_mex$pe)[[1]]
dimnames(IRF.boot_mex$pe)[[2]] <- var_names
dimnames(IRF.boot_mex$lb)[[2]] <- var_names
dimnames(IRF.boot_mex$ub)[[2]] <- var_names

#Ahora sí, graficamos las IRFs

#Chile

labels_chile <- c("IMACEC" = "IMACEC", "IPC_SAE" = "IPC (SAE)", "TPM_CHILE" = "Tasa de Política Monetaria", 
                  "TCN_CHILE" = "Tipo de Cambio Nominal", "EMBI_CHILE" = "EMBI")


chile_irf <- create_irf_boot_df(IRF.boot_chile, H)
plot_irf(chile_irf, "TPM_CHILE", "Chile", response_labels = labels_chile, shock_labels = c("TPM_CHILE" = "TPM"))
ggsave("Gráficos/IRF_Chile.png", width = 20, height = 16, units = "cm", dpi = 300)

#México

labels_mex<- c("IGAE" = "IGAE", "INPC" = "INPC", "TPM_MEXICO" = "Tasa de Política Monetaria", 
                  "TCN_MEXICO" = "Tipo de Cambio Nominal", "EMBI_MEXICO" = "EMBI")

mexico_irf <- create_irf_boot_df(IRF.boot_mex, H)
plot_irf(mexico_irf, "TPM_MEXICO", "México", response_labels = labels_mex, shock_labels = c("TPM_MEXICO" = "TPM"))
ggsave("Gráficos/IRF_México.png", width = 20, height = 16, units = "cm", dpi = 300)

#FEVD (descomposición de la varianza del error de pronóstico)

plot.fevd <- function(X, m, H, colors = c("#EF476F", "#FFD166", "#06D6A0", "#118AB2", "#073B4C"), output_dir = NULL) {
  
  par(mar = c(5, 4, 4, 6), xpd = TRUE)
  
  for (i in 1:m) {
    
    data_to_plot <- X[i, , 1:(H + 1)]
    
    if (!is.null(output_dir)) {
      if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
      png(file.path(output_dir, paste0("FEVD_", dimnames(X)[[1]][i], ".png")),
          width = 20, height = 16, units = "cm", res = 300)
    }
    
    barplot(data_to_plot, names.arg = as.character(0:H),
            main = paste("FEVD de ", dimnames(X)[[1]][i], sep = ""), xlab = "Horizonte", ylab = "% de varianza explicada", 
            col = colors)
    
    legend("right", inset = c(-0.55, 0), legend = dimnames(X)[[2]], fill = colors, bty = "n", title = "Shock")
    
    dev.off()
  }
}

#Chile

FEVD_chile <- SVAR.fevd(SVAR_chile, H)
dimnames(FEVD_chile)[[1]] <- c("IMACEC", "IPC (SAE)", "TPM", "TCN", "EMBI")
dimnames(FEVD_chile)[[2]] <- c("IMACEC", "IPC (SAE)", "TPM", "TCN", "EMBI")
plot.fevd(FEVD_chile, m, H, output_dir = "Gráficos/FEVD_Chile")

#México

FEVD_mex <- SVAR.fevd(SVAR_mex, H)
dimnames(FEVD_mex)[[1]] <- c("IAGE", "INPC", "TPM", "TCN", "EMBI")
dimnames(FEVD_mex)[[2]] <- c("IAGE", "INPC", "TPM", "TCN", "EMBI")
plot.fevd(FEVD_mex, m, H, output_dir = "Gráficos/FEVD_México")