
n <- 100
x <- rnorm(n)

n_qi <- 100
x0 <- seq(-3, 3, length.out = n_qi)

beta <- c(-2, 1)
lambda <- exp(beta[1] + beta[2]*x)

n_sims <- 1000
tau_hat_mle <- tau_hat_avg <- matrix(NA, nrow = n_sims, ncol = n_qi)
for (i in 1:n_sims) {
  y <- rpois(n, lambda = lambda)
  fit <- glm(y ~ x, family = poisson)
  beta_hat <- coef(fit)
  Sigma_hat <- vcov(fit)
  beta_tilde <- MASS::mvrnorm(1000, mu = beta_hat, Sigma = Sigma_hat)
  tau_tilde <- t(exp(cbind(1, x0)%*%t(beta_tilde)))*beta_tilde[, 2]
  tau_hat_avg[i, ] <- apply(tau_tilde, 2, mean)
  tau_hat_mle[i, ] <- exp(beta_hat[1] + beta_hat[2]*x0)*beta_hat[2]
}

sims_df <- data.frame(true_qi = exp(beta[1] + beta[2]*x0)*beta[2], 
                      mle = apply(tau_hat_mle, 2, mean),
                      avg = apply(tau_hat_avg, 2, mean)) %>%
  gather(method, ev, mle, avg)

ggplot(sims_df, aes(x = true_qi, y = ev - true_qi, linetype = method)) + 
  geom_line() + 
  theme_bw() + 
  labs(title = "Bias in Estimates of Poisson Marginal Effects",
       x = "True Marginal Effect",
       y = "Bias in Estimates of Marginal Effect",
       linetype = "Method")
ggsave("doc/figs/poisson-mcs.pdf", height = 3, width = 6)
