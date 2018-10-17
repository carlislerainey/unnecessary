
# set seed
set.seed(8769)

# load packages
library(tidyverse)

# create colors
mc <- c("#1b9e77",  # ml
        "#d95f02",  # avg
        "#7570b3")  # ann

# dgp parameters (from Rainey 2017)
n <- 10
b_cons <- 2.5
b_edu <- 0.1
sigma2 <- 1
edu <- runif(n, 10, 20)

# number of simuations
n_sims <- 10000  # mc simulations
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
  mutate(method = factor(method, levels = c("mle", "avg")),
         method = fct_recode(method, 
                             `Maximum Likelihood` = "mle", 
                             `Average of Simulations` = "avg")) %>%
  glimpse()

# density plot of simulations
ggplot(sim_df, aes(x = estimate)) + 
  stat_density(aes(linetype = method, color = method), 
               geom = "line", position = "identity") + 
  stat_density(aes(fill = method), 
               geom = "area", alpha = 0.2, position = "identity") + 
  #geom_segment(x = exp(b_cons + b_edu*20), xend = exp(b_cons + b_edu*20), 
  #             y = 0, yend = Inf) + 
  scale_x_continuous(limits = c(0, 500)) + 
  scale_fill_manual(values = mc[1:2]) + 
  scale_color_manual(values = mc[1:2]) + 
  scale_linetype_manual(values = c("solid", "longdash")) + 
  theme_bw() + 
  labs(x = "Median Income",
       y = "Density", 
       fill = "Method", 
       color = "Method",
       linetype = "Method", 
       caption = "Values larger than 500 are truncated in this plot.") + 
  guides(fill = guide_legend(keywidth = 1.5, keyheight = 1)) + 
  annotate("segment", x = qi_true, xend = qi_true, y = 0.0005, yend = 0, arrow = arrow(length = unit(0.05, "in")), size = 0.3) + 
  annotate("label", x = qi_true, y = 0.0005, label = "tau", size = 4, parse = TRUE,
           label.padding = unit(0.17, "lines")) + 
  annotate("segment", x = mean(qi_mle), xend = mean(qi_mle), y = 0.0015, yend = 0, 
           arrow = arrow(length = unit(0.05, "in")), size = 0.3, color = mc[1]) + 
  annotate("label", x = mean(qi_mle), y = 0.0015, label = "hat(tau)^{mle}", size = 2.5, parse = TRUE, color = mc[1],
           label.padding = unit(0.15, "lines")) + 
  annotate("segment", x = mean(qi_avg), xend = mean(qi_avg), y = 0.0007, yend = 0, 
           arrow = arrow(length = unit(0.05, "in")), size = 0.3, color = mc[2]) + 
  annotate("label", x = mean(qi_avg), y = 0.0007, label = "hat(tau)^{avg}", size = 2.5, parse = TRUE, color = mc[2],
           label.padding = unit(0.15, "lines"))

ggsave("doc/figs/rainey-2017-density.pdf", height  = 2.5, width = 5.5, scale = 1.4)

# summarize simulations
smry_df <- sim_df %>%
  group_by(method) %>%
  summarize(expected_value = mean(estimate),
            bias = expected_value - mean(true),
            sd = sd(estimate),
            rmse = sqrt(sd^2 + bias^2)) %>%
  gather(crit, value, bias:rmse) %>%
  mutate(crit = fct_recode(crit, Bias = "bias", SD = "sd", RMSE = "rmse"),
         crit = fct_reorder(crit, value)) %>%
  glimpse()

# plot summaries
smry2_df <- smry_df %>%
  mutate(method = fct_recode(method, 
                             `Maximum\nLikelihood` = "Maximum Likelihood", 
                             `Average of\nSimulations` = "Average of Simulations")) %>%
  glimpse()

gg <- ggplot(smry2_df, aes(x = method, y = value)) + 
  geom_col(width = 0.5) + 
  facet_wrap(~ crit, scales = "free_x", nrow = 1) + 
  theme_bw()  + 
  theme(panel.grid.major.x = element_blank()) + 
  labs(x = "Method", 
       y = "Value") 
ggsave("doc/figs/rainey-2017-summary.pdf", height = 2, width = 5.3)



