
library(brms)

df <- data.frame(
  x = c(17, 20, 23),
  y = c(2, 2, 1),
  n = c(233, 462, 103)
)

df$y <- round(df$y)
formula_nl <- bf(
  y | trials(n) ~ B + (A * (x - 15)^0.25 - B) * exp(-C * (x - 15)),
  A + B + C ~ 1,
  nl = TRUE
)

priors <- c(
  prior(normal(0.05, 0.03), nlpar = "A", lb = 0),   
  prior(normal(0.006, 0.004), nlpar = "B", lb = 0), 
  prior(normal(1.5, 0.8), nlpar = "C", lb = 0)     
)








fit_brm <- brm(
  formula = formula_nl,
  data = df,
  family = binomial(link = "identity"),
  prior = priors,
  chains = 4, iter = 6000, warmup = 3000,
  control = list(adapt_delta = 0.999, max_treedepth = 15),
  seed = 123
)

summary(fit_brm)

newdata <- data.frame(x = 16, n = 1)
pred <- posterior_epred(fit_id, newdata = newdata)
ci <- quantile(pred, probs = c(0.025, 0.975))
cat(" f(14):", mean(pred), "\n")
cat("95% CI (MCMC): [", ci[1], ", ", ci[2], "]\n")
plot(fit_brm)
mcmc_plot(fit_brm, type = "dens_overlay")
bayesplot::mcmc_acf()

age_seq <- 16:24
newdata <- data.frame(x = age_seq, n = 1)
posterior_preds <- posterior_epred(fit_brm, newdata = newdata)
f_mean <- apply(posterior_preds, 2, mean)
f_lower <- apply(posterior_preds, 2, quantile, probs = 0.025)
f_upper <- apply(posterior_preds, 2, quantile, probs = 0.975)
result_df <- data.frame(
  Age = age_seq,
  Predicted_Prevalence = round(f_mean, 4),
  Lower_95_CI = round(f_lower, 4),
  Upper_95_CI = round(f_upper, 4)
)
print(result_df, row.names = FALSE)



plot(age_seq, f_mean, type = "l", lwd = 2, col = "black",
     ylim = c(0, max(f_upper) + 0.05),
     xlab = "Age", ylab = "Predicted HPV Prevalence",
     main = "HPV Prevalence Prediction")


polygon(c(age_seq, rev(age_seq)),
        c(f_lower, rev(f_upper)),
        col = rgb(0.4, 0.4, 0.4, 0.4), border = NA)
lines(age_seq, f_mean, col = "black", lwd = 2)

points(df$x, df$y / df$n, col = "blue", pch = 19)
legend("topright", legend = c("Observed", "Predicted", "95% CI"),
       col = c("blue", "black", rgb(0.4, 0.4, 0.4, 0.4)),
       pch = c(19, NA, NA), lty = c(NA, 1, NA), lwd = c(NA, 2, NA),
       pt.cex = 1, bty = "n")

