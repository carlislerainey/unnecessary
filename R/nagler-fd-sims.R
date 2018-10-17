
# set seed
set.seed(589)

# load packages
library(tidyverse)
library(magrittr)

# load data
turnout_df <- haven::read_dta("data/scobit.dta") %>%
  filter(newvote != -1) %>%
  mutate(case_priority = sample(1:n())) %>%
  glimpse()

# fit model
f <- newvote ~ poly(neweduc, 2, raw = TRUE) + closing + poly(age, 2, raw = TRUE) + south + gov
fit <- glm(f, data = turnout_df, family = binomial(link = "probit"))

# print coef. estimates
texreg::screenreg(fit)

# simulation parameters
n_c <- 250  # number of cases for which to compute the quantity of interest
n_sims <- 10000
sample_size <- c(100, 200, 400, 800)

# compute simulation requisites
beta <- coef(fit)
turnout_df %<>%
  mutate(p = predict(fit, type = "response")) %>%
  glimpse()
pred_df <- filter(turnout_df, case_priority <= n_c)
pred1_df <- pred_df %>%
  mutate(closing = closing + sd(closing))
X_pred <- model.matrix(f, data = pred_df)
X1_pred <- model.matrix(f, data = pred1_df)

# do simulation
bias_df <- NULL
for (j in 1:length(sample_size)){
  qi_df <- NULL
  beta_hat_mat <- matrix(NA, nrow = n_sims, ncol = length(beta))
  fd_mle <- fd_avg <- matrix(NA, nrow = n_sims, ncol = n_c)
  sim_df <- turnout_df %>%
    filter(case_priority <= sample_size[j])
  cat(paste0("\nWorking on sample size ", sample_size[j], "...\n"))
  progress <- progress_estimated(n_sims)
  for (i in 1:n_sims) {
    sim_df %<>%
      mutate(y_sim = rbinom(sample_size[j], 1, p))
    sim_f <- update(f, y_sim ~ .)
    sim_fit <- glm(sim_f, data = sim_df, 
                   family = binomial(link = "probit"))
    # extract estimates
    beta_hat_mat[i, ] <- coef(sim_fit)
    Sigma_hat <- vcov(sim_fit)
    # simulate beta
    beta_tilde <- MASS::mvrnorm(10000, beta_hat_mat[i, ], Sigma_hat)
    # compute tau-hat
    fd_mle[i, ] <- pnorm(X1_pred%*%beta_hat_mat[i, ]) - pnorm(X_pred%*%beta_hat_mat[i, ])
    fd_avg[i, ] <- apply(pnorm(X1_pred%*%t(beta_tilde)) - pnorm(X_pred%*%t(beta_tilde)), 1, mean)
    progress$tick()$print()
  }
  
  # predicted probability
  e_beta <- apply(beta_hat_mat, 2, mean)
  tau_beta <- pnorm(X1_pred%*%beta) - pnorm(X_pred%*%beta)
  tau_e_beta <- pnorm(X1_pred%*%e_beta) - pnorm(X_pred%*%e_beta)
  e_tau_beta_mle <- apply(fd_mle, 2, mean)
  e_tau_beta_avg <- apply(fd_avg, 2, mean)
  
  # compute biases
  ci_bias <- tau_e_beta - tau_beta
  ti_bias <- e_tau_beta_mle - tau_e_beta
  sim_bias <- e_tau_beta_avg - e_tau_beta_mle
  bias_df_j <- data.frame(sample_size = sample_size[j], 
                          tau = tau_beta,
                          ci_bias = ci_bias,
                          ti_bias = ti_bias,
                          sim_bias = sim_bias, 
                          case_id = pred_df$case_priority)
  bias_df <- bind_rows(bias_df, bias_df_j)
}

# tidy the data
tall_bias_df <- bias_df %>%
  gather(concept, bias, ends_with("_bias")) %>%
  mutate(concept = fct_relevel(concept, c("ci_bias", "ti_bias", "sim_bias")),
         concept = fct_recode(concept,  
                              `Coefficient-Induced Bias` = "ci_bias",
                              `Transformation-Induced Bias` = "ti_bias",
                              `Simulation-Induced Bias` = "sim_bias"),
         sample_size_fct = factor(paste0("N = ", sample_size))) %>%
  glimpse() %>%
  write_rds("data/nagler-fd-bias.rds") %>%
  write_csv("data/nagler-fd-bias.csv")

