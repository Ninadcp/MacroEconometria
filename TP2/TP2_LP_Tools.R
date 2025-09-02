lp_alt1 <- function(y, shock, p, H, gamma = 0.95, cumulative = FALSE) {
  
  T <- length(y)
  z <- qnorm(1 - (1 - gamma) / 2)
  
  y_lags <- embed(y, p + 1)
  shock_lags <- embed(shock, p + 1)
  
  yh_base <- y_lags[, 1]
  y_controls <- y_lags[, -1, drop = FALSE]
  shock_controls <- shock_lags[, -1, drop = FALSE]
  shock_0 <- shock_lags[, 1]
  
  irf.pe <- rep(NA, H + 1)
  irf.lb <- rep(NA, H + 1)
  irf.ub <- rep(NA, H + 1)
  
  for (h in 0:H) {
    if (cumulative) {
      yh <- diff(cumsum(c(0, yh_base)), h + 1)
    } else {
      yh <- yh_base[(h + 1):(T - p)]
    }
    
    controls <- cbind(y_controls[1:(T - p - h), , drop = FALSE], shock_controls[1:(T - p - h), , drop = FALSE])
    
    yh_resid <- resid(lm(yh ~ controls))
    
    projection <- lm(yh_resid ~ -1 + shock_0[1:(T - p - h)])
    b.pe <- coef(projection)
    b.se <- sqrt(diag(vcovHAC(projection)))
    
    irf.pe[h + 1] <- b.pe
    irf.lb[h + 1] <- b.pe - z * b.se
    irf.ub[h + 1] <- b.pe + z * b.se
  }
  
  list(pe = irf.pe, lb = irf.lb, ub = irf.ub)
}


lp_alt2 <- function(Y, shock, p, idx.r, H, gamma = 0.95, cumulative = FALSE) {
  T <- nrow(Y)
  z <- qnorm(1 - (1 - gamma) / 2)
  
  rsp <- Y[(p + 1):T, idx.r]
  
  m <- ncol(Y)
  Xc <- embed(Y, p + 1)
  W1 <- Xc[, (m + 1):(m * (p + 1))]
  Wt <- cbind(1, W1)
  
  irf.pe <- rep(NA, H + 1)
  irf.lb <- rep(NA, H + 1)
  irf.ub <- rep(NA, H + 1)
  
  for (h in 0:H) {
    if (cumulative) {
      yh <- diff(cumsum(c(0, rsp)), h + 1)
    } else {
      yh <- rsp[(h + 1):(T - p)]
    }
    
    Wth <- Wt[1:(T - p - h), ]
    sth <- shock[1:(T - p - h)]
    
    yh_resid <- resid(lm(yh ~ -1 + Wth))
    
    proj <- lm(yh_resid ~ -1 + sth)
    b.pe <- coef(proj)
    b.se <- sqrt(diag(vcovHAC(proj)))
    
    irf.pe[h + 1] <- b.pe
    irf.lb[h + 1] <- b.pe - z * b.se
    irf.ub[h + 1] <- b.pe + z * b.se
  }
  
  list(pe = irf.pe, lb = irf.lb, ub = irf.ub)
}