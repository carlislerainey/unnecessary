
setwd(here::here())

set.seed(8764)

library(tidyverse)
library(magrittr)
library(forcats)

n <- 100
x <- rnorm(n)

n_qi <- 100
x0 <- seq(-3, 3, length.out = n_qi)

beta <- c(-2, 1)
lambda <- exp(beta[1] + beta[2]*x)

n_sims <- 1000
tau_hat_mle <- tau_hat_avg <- matrix(NA, nrow = n_sims, ncol = n_qi)
beta_hat <- matrix(NA, nrow = n_sims, ncol = length(beta))
for (i in 1:n_sims) {
  y <- rpois(n, lambda = lambda)
  fit <- glm(y ~ x, family = poisson)
  beta_hat[i, ] <- coef(fit)
  Sigma_hat <- vcov(fit)
  beta_tilde <- MASS::mvrnorm(1000, mu = beta_hat[i, ], Sigma = Sigma_hat)
  tau_tilde <- t(exp(cbind(1, x0)%*%t(beta_tilde)))*beta_tilde[, 2]
  tau_hat_avg[i, ] <- apply(tau_tilde, 2, mean)
  tau_hat_mle[i, ] <- exp(beta_hat[i, 1] + beta_hat[i, 2]*x0)*beta_hat[i, 2]
}

e_beta_hat <- apply(beta_hat, 2, mean)
sims_df <- data.frame(x = x0, 
                      true_qi = exp(beta[1] + beta[2]*x0)*beta[2],
                      tau_e_beta_hat = exp(e_beta_hat[1] + e_beta_hat[2]*x0)*e_beta_hat[2], 
                      mle = apply(tau_hat_mle, 2, mean),
                      avg = apply(tau_hat_avg, 2, mean)) %>%
  gather(method, ev, mle, avg) %>%
  mutate(method = fct_recode(method, `Average of Simulations` = "avg", `Maximum Likelihood` = "mle"))

ggplot(sims_df, aes(x = true_qi, y = ev - tau_e_beta_hat, linetype = method, color = method)) + 
  geom_line(size = .9) + 
  scale_linetype_manual(values = c("dotted", "solid")) + 
  scale_color_manual(values = c("#1b9e77", "#d95f02")) +
  theme_minimal() + 
  labs(title = expression(paste("Transformation-Induced ",  tau, "-Bias in Estimates of Poisson Marginal Effects")),
       x = "True Marginal Effect",
       y = expression(paste("Transformation-Induced ",  tau, "-Bias")),
       linetype = "Method", 
       color = "Method") + 
  guides(linetype = guide_legend(keywidth = 2, keyheight = 1))
ggsave("doc/figs/poisson-mcs.pdf", height = 3, width = 8)
