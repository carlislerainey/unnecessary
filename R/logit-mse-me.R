
# set seed
set.seed(8769)

# load packages
library(tidyverse)

# create colors
mc <- c("#1b9e77",  # ml
        "#d95f02",  # avg
        "#7570b3")  # ann

# dgp parameters (from Rainey 2017)
n <- 100
b_cons <- 0.0
b_x <- 1.0
x <- rnorm(n)

# number of simuations
n_sims <- 10000  # mc simulations
n_ktw <- 10000  # number of simulations of qi
n_pred <- 100
x_pred <- seq(-3, 3, length.out = n_pred)

# do simulations
df <- data.frame(x)
Xbeta <- b_cons + b_x*x
prog <- progress_estimated(n_sims)
qi_mle <- qi_avg <- matrix(nrow = n_sims, ncol = n_pred)
for (sim in 1:n_sims) {
  df$y <- rbinom(n, 1, plogis(Xbeta))
  fit <- glm(y ~ x, data = df, family = binomial)
  beta_hat <- coef(fit)
  Sigma_hat <- vcov(fit)
  beta_tilde <- MASS::mvrnorm(n_ktw, mu = beta_hat, Sigma = Sigma_hat)
  qi_mle[sim, ] <- dlogis(beta_hat[1] + beta_hat[2]*x_pred)*beta_hat[2]
  qi_avg[sim, ] <- apply(dlogis(t(cbind(1, x_pred)%*%t(beta_tilde))*beta_tilde[, 2]), 2, mean)
  prog$tick()$print()
}

# true df
true_df <- data.frame(true = dlogis(b_cons + b_x*x_pred)*b_x, x_pred = x_pred) %>% 
  glimpse()

# wrangle simulations
sim_df <- data.frame(mle = qi_mle,
                     avg = qi_avg) %>%
  gather(method, estimate, starts_with("mle."), starts_with("avg.")) %>%
  separate(method, into = c("method", "x_pred_index")) %>%
  mutate(x_pred_index = as.numeric(x_pred_index)) %>%
  mutate(x_pred = x_pred[x_pred_index]) %>%
  select(-x_pred_index) %>%
  glimpse()

library(boot)
x0 <- 1:5
bias_fn <- function(est, i, true) mean(est[i])
sd_fn <- function(est, i, true) sd(est[i])
rmse_fn <- function(est, i, true) {
  e <- est[i] - (true)
  s <- e^2
  m <- mean(s)
  r <- sqrt(m)
  return(r)
}
boot_it <- function(x, stat_fn, true) {
  bs <- boot(data = x, statistic = stat_fn, R = 400, true = true)
  se <- sd(bs$t)
  return(se)
}
boot_it(x0, rmse_fn, true = 1000)

# summarize simulations
smry_df <- sim_df %>% 
  left_join(true_df) %>%
  group_by(method, x_pred) %>% 
  summarize(est_bias = mean(estimate) - mean(true),
            se_bias = boot_it(estimate, bias_fn, true),
            est_sd = sd(estimate),
            se_sd = boot_it(estimate, sd_fn, true),
            est_rmse = sqrt(est_sd^2 + est_bias^2),
            se_rmse = boot_it(estimate, rmse_fn, true)) %>%
  gather(crit, value, est_bias:se_rmse) %>% 
  separate(crit, into = c("type", "crit")) %>% 
  spread(type, value) %>% 
  mutate(crit = fct_recode(crit, Bias = "bias", SD = "sd", `RMSE` = "rmse"),
         crit = fct_reorder(crit, est)) %>%
  glimpse()

# plot summaries
smry_df %>%
  ungroup() %>%
  mutate(method = factor(method, levels = c("mle", "avg")),
         method = fct_recode(method, `Maximum Likelihood` = "mle", 
                             `Average of Simulations` = "avg")) %>%
  ggplot(aes(x = x_pred, y = est, color = method, fill = method, 
              linetype = method,
              ymin = est - se, ymax = est + se)) + 
  #geom_ribbon(alpha = 0.5, color = NA) + 
  geom_line() + 
  facet_wrap(~ crit) + 
  theme_bw() + 
  scale_color_manual(values = mc[1:2]) + 
  scale_linetype_manual(values = c("solid", "longdash")) + 
  labs(x = "x", 
       y = "Value",
       color = "Method",
       linetype = "Method") + 
  guides(color = guide_legend(keywidth = 1.6, keyheight = 1))
ggsave("doc/figs/logit-mse-me.pdf", height = 2.2, width = 7)

# compute percent of range that mle is better than avg
smry_df %>%
  select(-se) %>%
  spread(method, est) %>% 
  group_by(crit) %>%
  summarize(percent_lower = mean(abs(mle) < abs(avg))) %>%
  glimpse()

