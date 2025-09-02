#Funciones para graficar IRFs

create_irf_boot_df <- function(IRF.boot, H) {
  responses <- dimnames(IRF.boot$pe)[[1]]
  shocks <- dimnames(IRF.boot$pe)[[2]]
  horizons <- 0:H
  
  df <- expand.grid(response = responses, shock = shocks, horizon = horizons)
  
  df$pe <- as.vector(IRF.boot$pe)
  df$lb <- as.vector(IRF.boot$lb)
  df$ub <- as.vector(IRF.boot$ub)
  
  #df$response <- sub("_.+", "", df$response)
  #df$shock <- sub("_.+", "", df$shock)
  
  return(df)
}

plot_irf <- function(df_irf, shock_name, country, response_labels = NULL, shock_labels = NULL) {
  
  df_plot <- df_irf %>% filter(shock == shock_name)
  
  lab <- if (!is.null(response_labels)) {
    labeller(response = as_labeller(response_labels))
  } else {
    "label_value"
  }
  
  shock_title <- if (!is.null(shock_labels) && shock_name %in% names(shock_labels)) {
    shock_labels[shock_name]
  } else {
    shock_name
  }
  
  p <- ggplot(df_plot, aes(x = horizon, y = pe)) +
    geom_ribbon(aes(ymin = lb, ymax = ub), fill = "grey80", alpha = 0.75) +
    geom_line(color = "black", linewidth = 1) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey") +
    facet_wrap(~response, scales = "free_y", ncol = 3, nrow = 2, labeller = lab) +
    labs(x = "Horizonte",y = "Respuesta", title = paste("IRF al shock en", shock_title, "-", country), 
         caption = "IC al 95% (Bootstrap)") +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5, face = "bold"))
  
  return(p)
}