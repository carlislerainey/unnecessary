
# load packages
library(tidyverse)

# set seed
set.seed(8769)

# dgp parameters (from Rainey 2017)
n <- 10
b_cons <- 2.5
b_edu <- 0.1
sigma2 <- 1
edu <- runif(n, 10, 20)

# number of simuations
n_sims <- 100000  # mc simulations
n_ktw <- 10000  # number of simulations of qi

# do simulations
df <- data.frame(edu)
Xbeta <- b_cons + b_edu*edu
qi_true <- exp(b_cons + b_edu*20)
prog <- progress_estimated(n_sims)
qi_mle <- qi_avg <- numeric(n_sims)
for (sim in 1:n_sims) {
  error <- rnorm(n, 0, sqrt(sigma2))
  log_income <- Xbeta + error
  df$income <- exp(log_income)
  fit <- lm(log(income) ~ edu, data = df)
  beta_hat <- coef(fit)
  Sigma_hat <- vcov(fit)
  beta_tilde <- MASS::mvrnorm(n_ktw, mu = beta_hat, Sigma = Sigma_hat)
  qi_mle[sim] <- exp(beta_hat[1] + beta_hat[2]*20)
  qi_avg[sim] <- mean(exp(beta_tilde[, 1] + beta_tilde[, 2]*20))
  prog$tick()$print()
}

# wrangle simulations
sim_df <- data.frame(true = qi_true, 
                     mle = qi_mle,
                     avg = qi_avg) %>%
  gather(method, estimate, mle, avg) %>%
  glimpse()

# density plot of simulations
ggplot(sim_df, aes(x = estimate, fill = method, linetype = method)) + 
  geom_density(alpha = 0.2) + 
  geom_segment(x = exp(b_cons + b_edu*20), xend = exp(b_cons + b_edu*20), 
               y = 0, yend = Inf) + 
  scale_x_continuous(limits = c(0, 500)) + 
  scale_fill_manual(values = c("#1b9e77", "#7570b3")) + 
  theme_minimal() + 
  labs(x = "Estimated Median Income",
       y = "Density", 
       fill = "Method", 
       linetype = "Method", 
       caption = "Estimates larger than 500 are truncated in this plot.")
ggsave("doc/figs/rainey-2017-density.pdf", height  = 3, width = 5.5)

# summarize simulations
smry_df <- sim_df %>%
  group_by(method) %>%
  summarize(expected_value = mean(estimate),
            bias = expected_value - mean(true),
            sd = sd(estimate),
            rmse = sqrt(sd^2 + bias^2)) %>%
  gather(crit, value, bias:rmse) %>%
  mutate(crit = fct_recode(crit, Bias = "bias", SD = "sd", `R.M.S.E` = "rmse"),
         crit = fct_reorder(crit, value)) %>%
  glimpse()

# plot summaries
ggplot(smry_df, aes(x = method, y = value)) + 
  geom_col(width = 0.5) + 
  facet_wrap(~ crit, scales = "free_y", nrow = 1) + 
  theme_minimal() + 
  theme(panel.grid.major.x = element_blank()) + 
  labs(x = "Method", 
       y = "Value")
ggsave("doc/figs/rainey-2017-summary.pdf", height = 2, width = 5.5)

