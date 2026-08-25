

library(brms)
df <- data.frame(
  x = c(16.5, 22, 27, 32),
  y = c(37, 88, 54, 38),
  n = c(740, 445, 414, 433)
)


df$y <- round(df$y)
formula_nl <- bf(
  y | trials(n) ~ B + (A * (x - 13)^2 - B) * exp(-C * (x - 13)),
  A + B + C ~ 1,
  nl = TRUE
)
priors <- c(
  prior(normal(0.01, 0.1), nlpar = "A"),
  prior(normal(-0.1, 0.1), nlpar = "B"),
  prior(normal(0.2, 0.1), nlpar = "C")
)
fit_post_brm <- brm(
  formula = formula_nl,
  data = df,
  family = binomial(link = "identity"),  
  prior = priors,
  chains = 4,
  iter = 4000,
  seed = 123,
  warmup = 1000
)

summary(fit_post_brm)

newdata <- data.frame(x = 16, n = 1)
pred <- posterior_epred(fit_post_brm, newdata = newdata)
ci <- quantile(pred, probs = c(0.025, 0.975))

plot(fit_post_brm)
mcmc_plot(fit_post_brm, type = "dens_overlay")
bayesplot::mcmc_acf()

age_seq <- 15:34
newdata <- data.frame(x = age_seq, n = 1)
posterior_preds <- posterior_epred(fit_post_brm, newdata = newdata)
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


