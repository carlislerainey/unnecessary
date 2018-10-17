
# set seed
set.seed(19743)

# load packages
library(tidyverse)
library(ggrepel)
library(forcats)

# create colors
mc <- c("#1b9e77",  # ml
        "#d95f02",  # avg
        "#7570b3")  # ann

# create tau
tau <- function(x) {
  x^2
}

# simulation parameters
n <- 100  # defined in paper
n_sims <- 10000  # large enough to smooth density plots
n_plot_keep <- 4

# do simulations
tilde_df <- mu_mle_seg_df <- tau_mle_seg_df <- tau_avg_seg_df <- ests_df <- NULL
pb <- progress_estimated(n_sims)
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
                     iteration_label = paste0("Simulation ", i, " of 10,000"),
                     quantity = c("mu", "tau", "tau"),
                     method = c("mle", "mle", "avg."),
                     est = c(mu_hat, tau_hat_mle, tau_hat_avg)))
  if (i <= n_plot_keep) {
    tilde_df <- tilde_df %>%
      rbind(data.frame(iteration = i, 
                       iteration_label = paste0("Simulation ", i, " of 10,000"),
                       mu = mu_tilde, 
                       tau = tau_tilde))
    mu_mle_seg_df <- mu_mle_seg_df %>%
      rbind(data.frame(iteration = i,
                       iteration_label = paste0("Simulation ", i, " of 10,000"),
                       x = mu_hat,
                       xend = mu_hat,
                       y = -Inf,
                       yend = tau(mu_hat)))
    tau_mle_seg_df <- tau_mle_seg_df %>%
      rbind(data.frame(iteration = i,
                       iteration_label = paste0("Simulation ", i, " of 10,000"),
                       x = -Inf,
                       xend = mu_hat,
                       y = tau(mu_hat),
                       yend = tau(mu_hat)))
    tau_avg_seg_df <- tau_avg_seg_df %>%
      rbind(data.frame(iteration = i,
                       iteration_label = paste0("Simulation ", i, " of 10,000"),
                       x = -Inf,
                       xend = Inf,
                       y = tau_hat_avg,
                       yend = tau_hat_avg))
  }
  if (i == 1) {
    pts_to_label_df <- data.frame(iteration = i,
                                  iteration_label = paste0("Simulation ", i, " of 10,000"),
                                  x = c(-Inf, -Inf, mu_hat),
                                  y = c(tau_hat_avg, tau(mu_hat), -Inf),
                                  label = c("hat(tau)^{avg.}", 
                                            "hat(tau)^{mle}",
                                            "hat(mu)^{mle}"),
                                  color = c("avg.", 
                                            "mle",
                                            "mle"))
  }
  pb$tick()$print()
}

# 4 figure illustration
ggplot(data = tilde_df, 
             aes(x = mu, y = tau)) + 
  facet_wrap(vars(iteration_label)) +
  geom_point(alpha = 0.2, 
             shape = 21) + 
  theme_bw() + theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) + 
  geom_rug(alpha = 0.2) + 
  geom_segment(data = mu_mle_seg_df, aes(x = x, xend = xend, y = y, yend = yend), 
               color = mc[1]) + 
  geom_segment(data = tau_mle_seg_df, aes(x = x, xend = xend, y = y, yend = yend),
               color = mc[1]) + 
  geom_segment(data = tau_avg_seg_df, aes(x = x, xend = xend, y = y, yend = yend),
               color = mc[2], linetype = "longdash") + 
  labs(x = expression(tilde(mu)),
       y = expression(tilde(tau))) + 
  guides(color = FALSE) + 
  geom_label_repel(data = pts_to_label_df, aes(x = x, y = y, 
                                               label = paste(label),
                                               color = color), 
                   label.padding = unit(0.1, "lines"), 
                   parse = TRUE, size = 2.3) + 
  scale_color_manual(values = c(mc[2], mc[1]))
ggsave("doc/figs/intuition.pdf", height = 5, width = 7)

gg_ests_df <- ests_df %>%
  mutate(method = factor(method, levels = c("mle", "avg.")),
         method = fct_recode(method, 
                             `Maximum Likelihood` = "mle", 
                             `Average of Simulations` = "avg."),
         quantity = fct_recode(quantity, 
                               `hat(tau)` = "tau", 
                               `hat(mu)` = "mu"))

ggplot(gg_ests_df, aes(x = est)) + 
  #geom_density(alpha = 0.2) + 
  stat_density(aes(linetype = method, color = method), 
               geom = "line", position = "identity") + 
  stat_density(aes(fill = method), 
               geom = "area", alpha = 0.2, position = "identity") + 
  facet_wrap(~ quantity, scales = "free", labeller = label_parsed) +  
  scale_fill_manual(values = mc[1:2]) +
  scale_color_manual(values = mc[1:2]) +
  scale_linetype_manual(values = c("solid", "longdash")) + 
  theme_bw() + 
  labs(x = "Estimate",
       y = "Density",
       fill = "Method",
       color = "Method",
       linetype = "Method") + 
  guides(fill = guide_legend(keywidth = 1.5, keyheight = 1))
ggsave("doc/figs/intuition-sampling.pdf", height = 3, width = 9)

