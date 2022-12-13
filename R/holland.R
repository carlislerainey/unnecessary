
# load packages
library(tidyverse)
library(ggrepel)
library(kableExtra)
# sandwich package also required
# MASS package also required

# set seed
set.seed(8904)

# load data
holland <- haven::read_dta("data/Enforcement.dta") %>% 
  # keep only the variables we use
  select(city, district, operations, lower, vendors, budget, population) %>%
  glimpse()

# formula corresponds to model 1 for each city in holland (2015) table 2
holland_f <- operations ~ lower + vendors + budget + population

# create function to fit regression model
fit_model <- function(data) {
  fit <- glm(holland_f, family = poisson, data = data)
  return(fit)
}

# simulate coefficients
simulate_coefficients <- function(beta_hat, Sigma_hat) {
  MASS::mvrnorm(5000000, mu = beta_hat, Sigma = Sigma_hat)
}

# simulate quantities of interest
compute_tau <- function(data, beta_hat, beta_tilde) {
  # set scenarios
  X_hyp <- X_obs <- model.matrix(holland_f, data = data)
  X_hyp[, "lower"] <- X_hyp[, "lower"]*.5
  
  # compute quantites of interest
  tau_tilde <- t((exp(X_hyp%*%t(beta_tilde)) - exp(X_obs%*%t(beta_tilde)))/exp(X_obs%*%t(beta_tilde)))
  tau_hat_avg <- apply(tau_tilde, 2, mean)  # simulation average
  tau_hat_mle <- (exp(X_hyp%*%beta_hat) - exp(X_obs%*%beta_hat))/exp(X_obs%*%beta_hat)  # mle
  
  # compute quantites of interest (hyp)
  tau_tilde_lo <- t(exp(X_hyp%*%t(beta_tilde)))
  tau_hat_avg_lo <- apply(tau_tilde_lo, 2, mean)  # simulation average
  tau_hat_mle_lo <- exp(X_hyp%*%beta_hat)
  
  # compute quantites of interest (obs)
  tau_tilde_hi <- t(exp(X_obs%*%t(beta_tilde)))
  tau_hat_avg_hi <- apply(tau_tilde_hi, 2, mean)  # simulation average
  tau_hat_mle_hi <- exp(X_obs%*%beta_hat)
  
  # combine estimated qis into a data frame
  tau <- data.frame(mle = tau_hat_mle, 
                         avg = tau_hat_avg,
                         mle_lo = tau_hat_mle_lo, 
                         avg_lo = tau_hat_avg_lo,
                         mle_hi = tau_hat_mle_hi, 
                         avg_hi = tau_hat_avg_hi) %>%
    bind_cols(data)
  return(tau)
}

# fit models and compute quantities of interest
estimations <- holland %>%
  group_by(city) %>%
  nest() %>% 
  mutate(fit = map(data, fit_model),
         beta_hat = map(fit, coef),
         Sigma_hat = map(fit, sandwich::vcovHC, type = "HC4m"),
         beta_tilde = map2(beta_hat, Sigma_hat, simulate_coefficients),
         tau = pmap(list(data, beta_hat, beta_tilde), compute_tau)) %>%
  glimpse()

# wrangle estimates into usuable form
tau <- estimations %>%
  unnest(tau) %>%
  ungroup() %>%
  mutate(city = str_to_title(city)) %>%
  mutate(district = reorder(district, lower)) %>%
  glimpse()

# create a data frame of annotations of mle and aos estimators
ann <- tau %>%
  group_by(city) %>%
  #filter(city == "Santiago") %>%
  filter(avg == max(avg)) %>%
  mutate(avg_label = "hat(tau)^avg") %>%
  mutate(mle_label = "hat(tau)^mle") %>%
  mutate(ch_pos = (avg + mle)/2,
         ch_label = paste0(round(100*(avg-mle)/mle), "'%'"),
         ch_percent = round(100*(avg-mle)/mle)) %>%
  glimpse()

# create a data frome of annotations for top 5 districts in each city
ann_city <- tau %>%
  group_by(city) %>%
  top_n(5, lower) %>%
  glimpse()

# create a helper function to label the y-axis
lab_fn <- function(x) {
  scales::percent(x, accuracy = 1)
}

# plot the estimates of the quantities of interest
ggplot(tau, aes(x = lower, xend = lower, y = avg, yend = mle)) + 
  facet_wrap(vars(city), scales = "free_y") + 
  #geom_point(size = 0.5) + 
  geom_segment(arrow = arrow(length = unit(0.05, "inches"))) + 
  scale_y_continuous(labels = lab_fn) + 
  theme_bw() + 
  labs(x = "Percent of District in Lower Class", 
       y = "Percent Increase in Enforcement Operations") + 
  geom_segment(data = ann, aes(x = lower + 1, xend = lower + 9, y = avg, yend = avg), 
               size = 0.2, color = "#d95f02") + 
  geom_segment(data = ann, aes(x = lower + 1, xend = lower + 9, y = mle, yend = mle), 
               size = 0.2, color = "#1b9e77") + 
  geom_label(data = ann, aes(x = lower + 7.5, y = avg, label = avg_label), 
             parse = TRUE, size = 2.5, color = "#d95f02", label.padding = unit(0.1, "lines")) + 
  geom_label(data = ann, aes(x = lower + 7.5, y = mle, label = mle_label), 
             parse = TRUE, size = 2.5, color = "#1b9e77", label.padding = unit(0.1, "lines")) + 
  geom_text_repel(data = ann_city, aes(x = lower, y = avg, label = district),
                  color = "grey50", size = 2.5, direction = "both", angle = 0, nudge_x = -14,
                  segment.size = .2, point.padding = 0.5, min.segment.length = 0)
ggsave("doc/figs/holland.pdf", height = 3, width = 9, scale = 1.2)

# 5 largest biases for each district
smry <- tau %>%
  mutate(ratio = (mle - avg)/avg) %>%
  group_by(city) %>%
  top_n(5, -ratio) %>%
  glimpse()

# create latex table w/ details for 5 largest qis in each city
smry %>%
  mutate(shrinkage = -(mle - avg)/avg) %>%
  arrange(city, desc(avg)) %>%
  mutate(avg = paste0(round(100*avg), "%"),
         avg_lo = round(avg_lo, 1),
         avg_hi = round(avg_hi, 1),
         mle = paste0(round(100*mle), "%"),
         mle_lo = round(mle_lo, 1),
         mle_hi = round(mle_hi, 1),
         shrinkage = paste0(round(100*shrinkage), "%")) %>%
  select(City = city,
         District = district, 
         `% Change[note]` = avg, 
         `From[note]` = avg_hi, 
         `To[note]` = avg_lo, 
         `% Change` = mle, 
         From = mle_hi, 
         To = mle_lo, 
         `Shrinkage[note]` = shrinkage) %>%
  kable("latex", booktabs = TRUE, align = c(rep("l", 2), rep("c", 7)), 
        caption = "\\label{tab:top-5}This table presents the details for the districts labelled in Figure \\ref{fig:holland}.") %>%
  kable_styling(latex_options = "hold_position", position = "center", font_size = 8) %>%
  add_header_above(c(" " = 2, "Average of Simulations" = 3, 
                     "ML Estimate" = 3, 
                     " " = 1), 
                     bold = TRUE) %>%
  column_spec(1, bold = TRUE) %>%
  column_spec(2, bold = TRUE) %>%
  row_spec(0, bold = TRUE) %>%
  collapse_rows(columns = 1, latex_hline = "major", valign = "middle") %>%
  add_footnote(c("Quantity of interest; percent change in enforcement operations when the percent in the lower class drops by half.", 
                 "Enforcement operations when the percent in the lower class equals its observed value.", 
                 "Enforcement operations when the percent in the lower class equals half its observed value.",
                 "Shrinkage in the quantity of interest due to switching from the average of simulations to the ML estimator."), 
               notation = "alphabet") %>%
  cat(file = "doc/tabs/top-5.tex")

# median bias for each district
smry2 <- tau %>%
  mutate(ratio = (mle - avg)/avg) %>%
  group_by(city) %>%
  summarize(med = median(ratio)) %>%
  glimpse() %>%
  write_csv("doc/tabs/holland-medians.csv")

texreg::screenreg(estimations$fit)


