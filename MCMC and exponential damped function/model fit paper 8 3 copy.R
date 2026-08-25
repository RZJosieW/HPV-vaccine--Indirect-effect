x <- c(18, 22.5)
n_total <- c(659, 659)
y_infected <- c(59, 123)

x0 <- 15
n_power <- 2

f_model <- function(x, A, B, C) {
  d <- x - x0
  B + (A * d^n_power - B) * exp(-C * d)
}

neg_log_likelihood <- function(par) {
  A <- par[1]; B <- par[2]; C <- par[3]
  p <- f_model(x, A, B, C)
  p <- pmax(pmin(p, 1 - 1e-6), 1e-6)
  -sum(y_infected * log(p) + (n_total - y_infected) * log(1 - p))
}

set.seed(1)
result <- optim(
  par = c(0.01, 0.1, 0.1),    
  fn = neg_log_likelihood,
  method = "BFGS",
  control = list(maxit = 1000, reltol = 1e-10)
)

fit_par <- setNames(result$par, c("A","B","C"))
print(fit_par)

curve_x <- seq(16, 24, length.out = 200)
fitted_p_curve <- f_model(curve_x, fit_par["A"], fit_par["B"], fit_par["C"])
fitted_p_curve <- pmax(pmin(fitted_p_curve, 1 - 1e-6), 1e-6)

plot(x, y_infected / n_total, col = "red", pch = 19,
     ylim = c(0, max(y_infected / n_total, fitted_p_curve) + 0.05),
     xlab = "Age", ylab = "HPV prevalence",
     main = "Fitted curve")
lines(curve_x, fitted_p_curve, col = "blue", lwd = 2)
legend("bottomright", legend = c("Observed", "Fitted"),
       col = c("red", "blue"), pch = c(19, NA), lty = c(NA, 1))

x_out <- 16:24
fitted_p_out <- f_model(x_out, fit_par["A"], fit_par["B"], fit_par["C"])
fitted_p_out <- pmax(pmin(fitted_p_out, 1 - 1e-6), 1e-6)

res_table <- data.frame(
  age = x_out,
  fitted_prevalence = round(fitted_p_out, 6)
)
print(res_table, row.names = FALSE)



# test 
set.seed(1)
result <- optim(
  par = c(0.01, 0.1, 0.1),
  fn = neg_log_likelihood,
  method = "BFGS",
  control = list(maxit = 1000, reltol = 1e-10),
  hessian = TRUE
)

fit_par <- setNames(result$par, c("A","B","C"))
H <- result$hessian

V <- solve(H)                 
se_par <- sqrt(diag(V))
param_ci <- cbind(
  est = fit_par,
  lwr = fit_par - 1.96*se_par,
  upr = fit_par + 1.96*se_par
)
print(param_ci)
grad_f <- function(x, A, B, C){
  d <- x - x0
  da <- (d^n_power) * exp(-C*d)              # ∂f/∂A
  db <- 1 - exp(-C*d)                        # ∂f/∂B
  dc <- -(A*d^n_power - B) * d * exp(-C*d)   # ∂f/∂C
  c(da, db, dc)
}

x_pred <- x_out
p_hat  <- f_model(x_pred, fit_par["A"], fit_par["B"], fit_par["C"])
p_hat  <- pmin(pmax(p_hat, 1e-6), 1-1e-6)

se_pred <- sapply(seq_along(x_pred), function(i){
  g <- grad_f(x_pred[i], fit_par["A"], fit_par["B"], fit_par["C"])
  sqrt( t(g) %*% V %*% g )
})

ci_pred_lin <- cbind(
  age = x_pred,
  p   = p_hat,
  lwr = pmax(0, p_hat - 1.96*se_pred),
  upr = pmin(1, p_hat + 1.96*se_pred)
)
print(ci_pred_lin, row.names = FALSE)
