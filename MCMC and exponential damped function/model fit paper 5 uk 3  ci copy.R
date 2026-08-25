x <- c(17, 20, 23)               
n_total <- c(1954, 737, 120)      
y_infected <- c(35, 19, 9)  

x0 <- 17                            
n_power <- 2                      
f_model <- function(x, A, B, C) {
  d <- x - x0
  B + (A * d^n_power - B) * exp(-C * d)
}

neg_log_likelihood <- function(par) {
  A <- par[1]
  B <- par[2]
  C <- par[3]
  
  p <- f_model(x, A, B, C)
  p <- pmax(pmin(p, 1 - 1e-6), 1e-6)  
  
  -sum(y_infected * log(p) + (n_total - y_infected) * log(1 - p))
}

result <- optim(
  par = c(0.01, 0.1, 0.1),       
  fn = neg_log_likelihood,      
  method = "BFGS"
)


curve_x <- seq(16, 24, length.out = 200)
fitted_p <- f_model(curve_x, result$par[1], result$par[2], result$par[3])

plot(x, y_infected / n_total, col = "red", pch = 19,
     ylim = c(0, 0.1),
     xlab = "Age", ylab = "HPV prevalence",
     main = "Fitted curve")
lines(curve_x, fitted_p, col = "blue", lwd = 2)
legend("topright", legend = c("Observed", "Fitted"),
       col = c("red", "blue"), pch = c(19, NA), lty = c(NA, 1))












#
set.seed(123)
x <- c(17, 20, 23)
n_total <- c(1954, 737, 120)
y_infected <- c(35, 19, 9)
x0 <- 17
n_power <- 2
f_model <- function(x, A, B, C) {
  d <- x - x0
  B + (A * d^n_power - B) * exp(-C * d)
}

neg_log_likelihood <- function(par) {
  p <- f_model(x, par[1], par[2], par[3])
  p <- pmin(pmax(p, 1e-6), 1 - 1e-6)
  -sum(y_infected * log(p) + (n_total - y_infected) * log(1 - p))
}
fit0 <- optim(par = c(0.01, 0.1, 0.1), fn = neg_log_likelihood, method = "BFGS")
est_par <- fit0$par
p_hat   <- f_model(x, est_par[1], est_par[2], est_par[3])
p_hat   <- pmin(pmax(p_hat, 1e-6), 1 - 1e-6)

B <- 1000
curve_x <- 16:24
pred_mat <- matrix(NA_real_, nrow = length(curve_x), ncol = B)

for (b in 1:B) {
  y_b <- rbinom(length(x), size = n_total, prob = p_hat)
  
  nll_b <- function(par) {
    p <- f_model(x, par[1], par[2], par[3])
    p <- pmin(pmax(p, 1e-6), 1 - 1e-6)
    -sum(y_b * log(p) + (n_total - y_b) * log(1 - p))
  }
  fit_b <- try(optim(par = est_par, fn = nll_b, method = "BFGS"), silent = TRUE)
  if (inherits(fit_b, "try-error")) next
  
  pred <- f_model(curve_x, fit_b$par[1], fit_b$par[2], fit_b$par[3])
  pred_mat[, b] <- pmin(pmax(pred, 0), 1) 
}
keep <- which(colSums(is.na(pred_mat)) == 0)
pred_mat <- pred_mat[, keep, drop = FALSE]
pred_mean <- rowMeans(pred_mat)
ci_lower  <- apply(pred_mat, 1, quantile, probs = 0.025)
ci_upper  <- apply(pred_mat, 1, quantile, probs = 0.975)
plot(x, y_infected / n_total, col = "red", pch = 19,
     ylim = range(c(0, ci_upper, y_infected / n_total)),
     xlab = "Age", ylab = "HPV prevalence",
     main = "Refit bootstrap 95% CI")

polygon(c(curve_x, rev(curve_x)),
        c(ci_lower,  rev(ci_upper)),
        border = NA, col = adjustcolor("gray", 0.3))

lines(curve_x, pred_mean, col = "blue", lwd = 2)
legend("topright", c("Observed","Fitted mean","95% CI"),
       col = c("red","blue","gray"), pch = c(19, NA, 15),
       lty = c(NA, 1, NA), bty = "n")
int_ages <- curve_x[round(curve_x) == curve_x]

ci_table_int <- data.frame(
  age   = int_ages,
  mean  = pred_mean[round(curve_x) == curve_x],
  lower = ci_lower[round(curve_x) == curve_x],
  upper = ci_upper[round(curve_x) == curve_x]
)
ci_table_int <- data.frame(
  age   = int_ages,
  mean  = round(ci_table_int$mean, 3),
  lower = round(ci_table_int$lower, 3),
  upper = round(ci_table_int$upper, 3)
)

print(ci_table_int, row.names = FALSE)


