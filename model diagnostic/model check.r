library(brms)
library(bayesplot)
library(ggplot2)
library(patchwork)
color_scheme_set("blue")
theme_set(
  theme_bw(base_size = 12)
)
trace_plot <- mcmc_trace(
  as.array(fit_B),
  
  pars = c(
    "b_Intercept",
    "b_typecatchup",
    "b_typenontarget",
    "sds_scoverage_1",
    "sds_syear_1"
  ),
  
  facet_args = list(
    ncol = 2
  )
) +
  
  theme(
    panel.grid.minor = element_blank(),
    panel.border = element_rect(
      colour = "#4D4D4D",
      linewidth = 0.5
    ),
     strip.background = element_rect(
      fill = "#F2F2F2",
      colour = "#D9D9D9"
    )
  )
trace_plot
density_plot <- mcmc_areas(
  as.array(fit_B),
  pars = c(
    "b_Intercept",
    "b_typecatchup",
    "b_typenontarget"
  ),
  
  prob = 0.95
) +
  
  theme(
    
    panel.grid.minor = element_blank(),
    
    panel.border = element_rect(
      colour = "#4D4D4D",
      linewidth = 0.5
    )
  )

density_plot
pp_check_plot <- pp_check(
  
  fit_B,
  
  ndraws = 100
) +
  
  labs(
    
    title = "Posterior predictive check"
  ) +
  
  theme(
    
    panel.grid.minor = element_blank(),
    
    panel.border = element_rect(
      colour = "#4D4D4D",
      linewidth = 0.5
    ),
    
    plot.title = element_text(
      size = 13,
      face = "plain"
    )
  )

pp_check_plot
fitted_vals <- fitted(fit_B)[, "Estimate"]

residuals_vals <- residuals(fit_B)[, "Estimate"]

res_df <- data.frame(
  
  fitted = fitted_vals,
  
  residuals = residuals_vals
)

residual_plot <- ggplot(
  
  res_df,
  
  aes(
    x = fitted,
    y = residuals
  )
) +
  
  geom_point(
    
    shape = 1,
    
    size = 2,
    
    alpha = 0.7
  ) +
  
  geom_hline(
    
    yintercept = 0,
    
    linetype = "dashed",
    
    colour = "#4D4D4D"
  ) +
  
  labs(
    
    x = "Fitted values",
    
    y = "Residuals"
  ) +
  
  theme_bw(base_size = 12) +
  
  theme(
    
    panel.grid.minor = element_blank(),
    
    panel.border = element_rect(
      colour = "#4D4D4D",
      linewidth = 0.5
    )
  )

residual_plot
# 5. Rhat diagnostics

rhat_vals <- brms::rhat(fit_B)

summary(rhat_vals)
max(rhat_vals, na.rm = TRUE)
summary(fit_B)

# 6. Effective sample size

neff_vals <- neff_ratio(fit_B)

summary(neff_vals)

# 7. Divergent transitions

nuts_params <- nuts_params(fit_B)

table(nuts_params$Parameter)

