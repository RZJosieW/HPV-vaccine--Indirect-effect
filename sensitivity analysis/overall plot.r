library(ggplot2)
library(patchwork)
pt_size    <- 1.7
pt_alpha   <- 0.5
ln_width   <- 1.2
rib_alpha  <- 0.24

overall_line_col   <- "#E58F8F"   
overall_ribbon_col <- "#D36B6B"   
overall_point_col  <- "#6E6E6E" 
plot_overall_from_hier <- function(fit_model, df,
                                   xvar   = c("coverage", "year", "agemid"),
                                   ndraws = 800, ngrid = 200) {
  xvar <- match.arg(xvar)
  xr   <- range(df[[xvar]], na.rm = TRUE)
  grid <- seq(xr[1], xr[2], length.out = ngrid)
  
  m  <- numeric(ngrid)
  ql <- numeric(ngrid)
  qh <- numeric(ngrid)
    for (i in seq_along(grid)) {
    newd         <- df
    newd[[xvar]] <- grid[i]
    newd$se      <- median(df$se, na.rm = TRUE)
    
    E <- posterior_epred(
      fit_model,
      newdata    = newd,
      draws      = ndraws,
      re_formula = NA
    )
    
    E_mean <- apply(E, 1, mean)
    m[i]  <- mean(E_mean)
    ql[i] <- quantile(E_mean, 0.025)
    qh[i] <- quantile(E_mean, 0.975)
  }
  
  dd <- data.frame(
    x    = grid,
    mean = m,
    lo   = ql,
    hi   = qh
  )
    ggplot(dd, aes(x = x)) +
    geom_ribbon(aes(ymin = lo, ymax = hi),
                fill   = overall_ribbon_col,
                alpha  = rib_alpha,
                colour = NA) +
    geom_line(aes(y = mean),
              colour    = overall_line_col,
              linewidth = ln_width) +
    geom_point(
      data = df,
      aes(x = .data[[xvar]], y = indirect),
      colour = overall_point_col,
      shape  = 16,
      size   = pt_size,
      alpha  = pt_alpha
    ) +
    labs(
      x = switch(xvar,
                 coverage = "Vaccine coverage",
                 year     = "Years since introduction",
                 agemid   = "Age"),
      y = "Indirect effect"
    ) +
    scale_y_continuous(limits = c(-10, 10),
                       breaks = seq(-10, 10, 5))
}
p_cov_overall  <- plot_overall_from_hier(fit_B, hpv, "coverage")
p_year_overall <- plot_overall_from_hier(fit_B, hpv, "year")
p_age_overall  <- plot_overall_from_hier(fit_B, hpv, "agemid")
panel_overall <- p_cov_overall | p_year_overall | p_age_overall
panel_overall <- panel_overall & 
  theme_bw() &  
  theme(
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background  = element_rect(fill = "white", colour = NA),
    
    panel.grid.major = element_line(colour = "#EBEBEB"),
    panel.grid.minor = element_line(colour = "#F5F5F5"),
    
    panel.border      = element_rect(colour = "#3E3E3E", fill = NA, linewidth = 0.8),
    axis.line         = element_line(color = "#3E3E3E", linewidth = 0.4),
    axis.ticks        = element_line(color = "#3E3E3E", linewidth = 0.4),
    axis.ticks.length = unit(3, "pt"),
    
    panel.spacing     = unit(8, "pt"),
    plot.tag          = element_text(size = 14, face = "plain") 
  )
panel_overall <- panel_overall + 
  plot_annotation(tag_levels = "A")
panel_overall