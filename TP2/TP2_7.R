
library(zoo)
library(lmtest)
library(sandwich)
library(ggplot2)

#Seteo directorio, limpio el environment y abro el último R script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls(all.names = TRUE))
gc()
source("TP2_LP.R")
datos <- as.data.frame(datos)
Y_pw <- as.matrix(cbind(smdi = datos$smdi, datos[, vars]))
D <- ifelse(epsilon_wt > 0, 1, 0)
vars_all <- colnames(Y_pw) 
Ttot <- nrow(Y_pw)
k    <- ncol(Y_pw)

sigma1 <- sd(epsilon_wt[D == 1])
sigma2 <- sd(epsilon_wt[D == 0])

eps_s1 <- D * epsilon_wt / sigma1
eps_s2 <- (1 - D) * epsilon_wt / sigma2

source("PS3_LP_Tools.R")

p <- 1
H <- 4
gamma <- 0.95

sigma1 <- sd(epsilon_wt[D == 1], na.rm = TRUE)
sigma2 <- sd(epsilon_wt[D == 0], na.rm = TRUE)

eps_s1 <- D * epsilon_wt / sigma1          # "sequía" (positivo)
eps_s2 <- (1 - D) * epsilon_wt / sigma2    # "lluvias" (negativo)

# 2) Data frame de resultados vacío
res_asym <- data.frame(
  var = character(0), h = integer(0),
  beta_pos = numeric(0), se_pos = numeric(0),
  beta_neg = numeric(0), se_neg = numeric(0),
  n_obs = integer(0),
  stringsAsFactors = FALSE
)

# 3) LP asimétricas para TODAS las variables y TODOS los horizontes
for (v in vars_all) {
  y_idx <- which(colnames(Y_pw) == v)
  if (length(y_idx) != 1) next
  
  for (h in 0:H) {
    # Rango temporal válido: t ∈ [1+p, T-h]
    t_start <- 1 + p
    t_end   <- Ttot - h
    if (t_start > t_end) stop("No hay datos suficientes para h = ", h)
    
    # LHS: y_{t+h}
    lhs <- Y_pw[(t_start + h):(t_end + h), y_idx]
    
    # Shocks en t
    x1  <- eps_s1[t_start:t_end]
    x2  <- eps_s2[t_start:t_end]
    
    # Controles: rezagos 1..p de TODAS las variables en t
    Xc <- NULL
    for (L in 1:p) {
      # Y_{t-L} para todas las k variables
      block_L <- Y_pw[(t_start - L):(t_end - L), , drop = FALSE]
      # nombro columnas para identificar lag y variable
      colnames(block_L) <- paste0("L", L, "_", colnames(Y_pw))
      # acumulo columnas
      if (is.null(Xc)) {
        Xc <- block_L
      } else {
        Xc <- cbind(Xc, block_L)
      }
    }
    
    # Armo data.frame para la regresión
    df_h <- data.frame(y_lead = lhs,
                       eps_s1 = x1,
                       eps_s2 = x2,
                       Xc,
                       check.names = FALSE)
    
    # Fórmula: y_{t+h} ~ eps_s1 + eps_s2 + TODOS los lags
    rhs_names <- colnames(df_h)[-(1:3)]
    fmla_txt  <- paste0("y_lead ~ eps_s1 + eps_s2 + ", paste(rhs_names, collapse = " + "))
    fmla      <- as.formula(fmla_txt)
    
    # OLS
    fit <- lm(fmla, data = df_h)
    
    # Vcov HAC (Newey-West). Regla simple: lag = max(1, h + p)
    Vnw <- sandwich::NeweyWest(fit, lag = max(1, h + p), prewhite = FALSE, adjust = TRUE)
    co  <- lmtest::coeftest(fit, vcov. = Vnw)
    
    # Extraigo β y SE para eps_s1 (positivo) y eps_s2 (negativo)
    if (!("eps_s1" %in% rownames(co)) || !("eps_s2" %in% rownames(co))) {
      # Si por colinealidad extrema no aparecen, continuar
      next
    }
    b1  <- co["eps_s1", "Estimate"]; se1 <- co["eps_s1", "Std. Error"]
    b2  <- co["eps_s2", "Estimate"]; se2 <- co["eps_s2", "Std. Error"]
    
    # Agrego fila a resultados
    res_asym <- rbind(
      res_asym,
      data.frame(var = v, h = h,
                 beta_pos = b1, se_pos = se1,
                 beta_neg = b2, se_neg = se2,
                 n_obs = nrow(df_h),
                 stringsAsFactors = FALSE)
    )
  }
}

# 4) Intervalos al 95% y test simple de asimetría por horizonte (Δ = β_pos - β_neg)
z <- 1.96
res_asym$pos_lo <- res_asym$beta_pos - z * res_asym$se_pos
res_asym$pos_hi <- res_asym$beta_pos + z * res_asym$se_pos
res_asym$neg_lo <- res_asym$beta_neg - z * res_asym$se_neg
res_asym$neg_hi <- res_asym$beta_neg + z * res_asym$se_neg

res_asym$delta     <- res_asym$beta_pos - res_asym$beta_neg
res_asym$se_delta  <- sqrt(res_asym$se_pos^2 + res_asym$se_neg^2)  # aprox (cov≈0)
res_asym$delta_lo  <- res_asym$delta - z * res_asym$se_delta
res_asym$delta_hi  <- res_asym$delta + z * res_asym$se_delta
res_asym$asym_sig  <- ifelse(res_asym$delta_lo > 0 | res_asym$delta_hi < 0, 1, 0)

# 5) Guardar resultados
write.csv(res_asym, "LP_asimetria_resultados.csv", row.names = FALSE)

# 6) Gráficos simples por variable: IRF pos y neg con bandas
dir_out <- "plots_lp_asym"
if (!dir.exists(dir_out)) dir.create(dir_out)

for (v in unique(res_asym$var)) {
  dfv <- res_asym[res_asym$var == v, ]
  dfv <- dfv[order(dfv$h), ]
  
  # Shock positivo (sequía)
  g_pos <- ggplot(dfv, aes(x = h, y = beta_pos)) +
    geom_ribbon(aes(ymin = pos_lo, ymax = pos_hi), alpha = 0.15) +
    geom_line(size = 0.9) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(title = paste0("IRF (shock positivo / sequía): ", v),
         x = "Horizonte h", y = "Respuesta") +
    theme_minimal(base_size = 12)
  
  # Shock negativo (lluvias)
  g_neg <- ggplot(dfv, aes(x = h, y = beta_neg)) +
    geom_ribbon(aes(ymin = neg_lo, ymax = neg_hi), alpha = 0.15) +
    geom_line(size = 0.9) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(title = paste0("IRF (shock negativo / lluvias): ", v),
         x = "Horizonte h", y = "Respuesta") +
    theme_minimal(base_size = 12)
  
  ggsave(file.path(dir_out, paste0("IRF_asym_pos_", v, ".png")), g_pos, width = 7, height = 4.5, dpi = 180)
  ggsave(file.path(dir_out, paste0("IRF_asym_neg_", v, ".png")), g_neg, width = 7, height = 4.5, dpi = 180)
}

v_demo <- vars_all[1]
df_demo <- subset(res_asym, var == v_demo)
df_demo <- df_demo[order(df_demo$h), ]

ggplot(df_demo, aes(x = h, y = beta_pos)) +
  geom_ribbon(aes(ymin = pos_lo, ymax = pos_hi), alpha = 0.15) +
  geom_line(size = 0.9) + geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = paste0("IRF (shock positivo / sequía): ", v_demo),
       x = "Horizonte h", y = "Respuesta") +
  theme_minimal(base_size = 12)

ggplot(df_demo, aes(x = h, y = beta_neg)) +
  geom_ribbon(aes(ymin = neg_lo, ymax = neg_hi), alpha = 0.15) +
  geom_line(size = 0.9) + geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = paste0("IRF (shock negativo / lluvias): ", v_demo),
       x = "Horizonte h", y = "Respuesta") +
  theme_minimal(base_size = 12)



