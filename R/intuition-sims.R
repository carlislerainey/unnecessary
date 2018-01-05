
setwd(here::here())

set.seed(19743)

library(tidyverse)
library(magrittr)
library(ggrepel)
library(forcats)

tau <- function(x) {
  x^2
}

n <- 100
n_sims <- 1000
n_plot_keep <- 4
tilde_df <- mu_mle_seg_df <- tau_mle_seg_df <- tau_avg_seg_df <- ests_df <- NULL
for (i in 1:n_sims) {
  y <- rnorm(n)
  mu_hat <- mean(y)
  sigma_hat <- 1/sqrt(n)
  mu_tilde <- rnorm(1000, mean = mu_hat, sd = sigma_hat)
  tau_tilde <- tau(mu_tilde)
  tau_hat_avg <- mean(tau_tilde)
  tau_hat_mle <- tau(mu_hat)
  ests_df <- ests_df %>%
    rbind(data.frame(iteration = i,
                     quantity = c("mu", "tau", "tau"),
                     method = c("mle", "mle", "avg."),
                     est = c(mu_hat, tau_hat_mle, tau_hat_avg)))
  if (i <= n_plot_keep) {
    tilde_df <- tilde_df %>%
      rbind(data.frame(iteration = i, 
                       mu = mu_tilde, 
                       tau = tau_tilde))
    mu_mle_seg_df <- mu_mle_seg_df %>%
      rbind(data.frame(iteration = i,
                       x = mu_hat,
                       xend = mu_hat,
                       y = -Inf,
                       yend = tau(mu_hat)))
    tau_mle_seg_df <- tau_mle_seg_df %>%
      rbind(data.frame(iteration = i,
                       x = -Inf,
                       xend = mu_hat,
                       y = tau(mu_hat),
                       yend = tau(mu_hat)))
    tau_avg_seg_df <- tau_avg_seg_df %>%
      rbind(data.frame(iteration = i,
                       x = -Inf,
                       xend = Inf,
                       y = tau_hat_avg,
                       yend = tau_hat_avg))
  }
  if (i == 1) {
    pts_to_label_df <- data.frame(iteration = i,
                                  x = c(-Inf, -Inf, mu_hat),
                                  y = c(tau_hat_avg, tau(mu_hat), -Inf),
                                  label = c("hat(tau)^{avg.}", 
                                            "hat(tau)^{mle}",
                                            "hat(mu)^{mle}"),
                                  color = c("avg.", 
                                            "mle",
                                            "mle"))
  }
}


gg_list <- list()
for (i in 1:n_plot_keep) {
  gg <- ggplot(data = filter(tilde_df, iteration == i), 
               aes(x = mu, y = tau)) + 
    geom_point(alpha = 0.2, 
               shape = 21) + 
    theme_minimal() + 
    geom_rug(alpha = 0.2) + 
    geom_segment(data = filter(mu_mle_seg_df, iteration == i), aes(x = x, xend = xend, y = y, yend = yend), 
                 color = "#1b9e77") + 
    geom_segment(data = filter(tau_mle_seg_df, iteration == i), aes(x = x, xend = xend, y = y, yend = yend),
                 color = "#1b9e77") + 
    geom_segment(data = filter(tau_avg_seg_df, iteration == i), aes(x = x, xend = xend, y = y, yend = yend),
                 color = "#7570b3", linetype = "dashed") + 
    labs(x = expression(tilde(mu)),
         y = expression(tilde(tau))) + 
    guides(color = FALSE)
  if (i == 1) {
    gg <- gg + 
    geom_label_repel(data = pts_to_label_df, aes(x = x, y = y, 
                                                 label = paste(label),
                                                 color = color), 
                     parse = TRUE, size = 2.3) + 
    scale_color_manual(values = c("#7570b3", "#1b9e77"))
  }
  gg <- ggExtra::ggMarginal(gg)
  ggsave(gg, filename = paste0("doc/figs/intuition-", i, ".pdf"),
         height = 3, width = 5)
  }

gg_ests_df <- ests_df %>%
  mutate(method = fct_recode(method, 
                             `Maximum Likelihood` = "mle", 
                             `Average of Simulations` = "avg."),
         quantity = fct_recode(quantity, 
                               `hat(tau)` = "tau", 
                               `hat(mu)` = "mu"))

ggplot(gg_ests_df, aes(x = est, fill = method, color = method)) + 
  geom_density(alpha = 0.2) + 
  facet_wrap(~ quantity, scales = "free", labeller = label_parsed) +  
  scale_fill_manual(values = c("#7570b3", "#1b9e77")) +
  scale_color_manual(values = c("#7570b3", "#1b9e77")) +
  theme_minimal() + 
  labs(x = "Estimate",
       y = "Density",
       fill = "Method",
       color = "Method")
ggsave("doc/figs/intuition-sampling.pdf", height = 3, width = 9)

