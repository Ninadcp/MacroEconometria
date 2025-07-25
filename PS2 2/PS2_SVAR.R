#------------------------------------------------------------------------------#
# Maestría en Econom+ia
# Macroeconometría
# 2024, 3er trimestre 
# Profesor: Javier Garcia-Cicco
# Tutor: Franco Nuñez

# Material basado en código de Luis Libonatti (usado en versiones anteriores de 
# la materia)
#------------------------------------------------------------------------------#
remove(list = ls(all.names = TRUE))
gc()

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Data ####

source("PS2_Data.R")

Yl.f <- cbind(pcom, er, pc) #y en log niveles
Yl.f <- log(Yl.f) # log transformation
Yd.f <- 100 * diff(Yl.f) # log-diff transformation

Yl <- window(Yl.f, start = c(2004, 01), end = c(2019, 12))
Yd <- window(Yd.f, start = c(2004, 01), end = c(2019, 12))

# VAR Estimation (Reduced Form) ####

library(vars)

Y <- Yd

# Lag Order Selection
pmax <- 12 # Maximum lag order

popt <- VARselect(Y, lag.max = pmax, type = "const")
popt
p <- popt$selection[2] # HQIC (For log-levels series, see Kilian & L?tkepohl, pp. 373), p > 1 neccesary

# Estimation
Y <- Y[(pmax - p + 1):nrow(Y), ] # Starting in Jan-05

VAR <- VAR(Y, p = p, type = "const")

m <- VAR$K # Number of variables in the VAR
T <- VAR$obs # Number of effective sample observations, excluding "p" starting values

# Ad hoc Function
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

# Simplification (no feedback from local variables to PCOM)
constraints <- matC(m, p, 1)
VAR <- restrict(VAR, method = "man", resmat = constraints)
VAR

# Model Checking
roots(VAR, modulus = TRUE)

h.BG <- 6
serial.test(VAR, lags.bg = h.BG, type = "ES")

# VAR Estimation (Structural Form) ####

# A Matrix
Amat <- diag(m)
for (i in 2:m) {
  for (j in 1:(i - 1)) {
    Amat[i, j] <- NA
  }
}

# B Matrix
Bmat <- matrix(0, m, m)
for (i in 1:m) {
  Bmat[i, i] <- NA
}

# SVAR Estimation (AB model configuration)
SVAR <- SVAR(VAR, Amat = Amat, Bmat = Bmat, lrtest= FALSE)
SVAR

# SVAR Impact Matrix (Cholesky decomposition)
S <- t(resid(VAR)) %*% resid(VAR) / (T - m * p - 1)
P.chol <- t(chol(S))
S

# SVAR Impact Matrix (implied by AB model)
P <- solve(SVAR$A, SVAR$B) # inv(A) %*% B
S.SVAR <- P %*% t(P)
S.SVAR

# Other SVAR Parameters
pars.R <- Bcoef(VAR) # Reduced Form VAR
pars.S <- solve(SVAR$A, pars.R) # Structural Form VAR
pars.R
pars.S

# SVAR Analysis ####

source("PS2_SVAR_Tools.R")
source("PS2_SVAR_Plots.R")

H <- 24 # Horizon
H.ERPT <- 240 # Horizon (ERPT)

# IRF
IRF <- SVAR.sirf(SVAR, H)
plot.sirf(IRF, m, H)

# IRF (cumulative) 
IRF.c <- SVAR.sirf(SVAR, H, cumulative = TRUE)
plot.sirf(IRF.c, m, H)

# FEVD
FEVD <- SVAR.fevd(SVAR, H)
plot.fevd(FEVD, m, H)

# HD
HD <- SVAR.hd(SVAR)
plot.hd(Y, HD, m)

# ERPT: Log-differences
ERPT <- SVAR.erpt(SVAR, H.ERPT, 3, 2)
plot.erpt(ERPT, H.ERPT)

# # ERPT: Log-levels
# ERPT <- SVAR.erpt(SVAR, H.ERPT, 3, 2, cumulative = FALSE)
# plot.erpt(ERPT, H.ERPT)

# Bootstrap Inference ####

R <- 500 # Number of bootstrap replications
type <- "nonparametric"
gamma <- 0.95 # Confidence level

# COMMENT ON THE IMPORTANCE OF MULTIVARIATE CONFIDENCE INTERVALS 

# Bootstrap Replications
Y.boot <- boot.replicate(VAR, R, type)

# IRF (Bootstrap)
IRF.boot <- SVAR.sirf.boot(SVAR, Amat, Bmat, H, gamma, Y.boot)
plot.sirf.boot(IRF.boot, m, H)

# Cumulative IRF (Bootstrap)
IRF.c.boot <- SVAR.sirf.boot(SVAR, Amat, Bmat, H, gamma, Y.boot, cumulative = TRUE)
plot.sirf.boot(IRF.c.boot, m, H)

# FEVD (Bootstrap)
FEVD.boot <- SVAR.fevd.boot(SVAR, Amat, Bmat, H, gamma, Y.boot)
plot.fevd.boot(FEVD.boot, m, H)

# ERPT (Bootstrap): Log-differences
ERPT.boot <- SVAR.erpt.boot(SVAR, Amat, Bmat, H.ERPT, 3, 2, gamma, Y.boot)
plot.erpt.boot(ERPT.boot, H.ERPT)

# # ERPT (Bootstrap): Log-levels
# ERPT.boot <- SVAR.erpt.boot(SVAR, Amat, Bmat, H.ERPT, 3, 2, gamma, Y.boot, cumulative = TRUE)
# plot.erpt.boot(ERPT.boot, H.ERPT)