
# set seed
set.seed(4835)

# load packages
library(tidyverse)

# load data
ge <- read_csv("data/ge.csv")

# drop missing
keep <- c("court", "dq", "cr", "pc", "ag", "sp", "pe", "cc", "ap", "dc", "st", "sg")
ge <- na.omit(ge[, keep])

# formula
f <- court ~ dq + cr + pc + ag + sp + pe + cc + ap + dc + st + sg

# estimate models
fit <- glm(f, data = ge, family = binomial(link = "probit"))

# regression table
texreg::texreg(fit, single.row = TRUE, stars = 0.05, custom.coef.names = vnp <- c("Intercept",
                                                    "Death-Qualified Jury",
                                                    "Capital Punishment Proportional to Offense",
                                                    "Particularizing Circumstances",
                                                    "Aggravating Factors",
                                                    "State Psychiatric Examination",
                                                    "Conservative Political Environment",
                                                    "Court Change",
                                                    "State Appellant",
                                                    "Inexperienced Defense Counsel",
                                                    "Repeat Player State",
                                                    "Amicus Brief from Solicitor General"))


# predicted probability
beta_hat <- coef(fit)
Sigma_hat <- vcov(fit)
beta_tilde <- MASS::mvrnorm(10000, mu = beta_hat, Sigma = Sigma_hat)
X_c <- cbind(1, as.matrix(select(ge, -court)))
tau_tilde <- t(pnorm(X_c%*%t(beta_tilde)))

tau_hat_avg <- apply(tau_tilde, 2, mean)  # simulation average

tau_hat_mle <- pnorm(X_c%*%beta_hat)  # mle

tau_df <- data.frame(mle = tau_hat_mle, 
                     avg = tau_hat_avg)
gg <- ggplot(tau_df, aes(x = mle, avg)) + 
  scale_color_gradient2(mid = "black", low = "blue", high = "red") + 
  geom_point(shape = 21) + 
  geom_abline(intercept = 0, slope = 1) + 
  theme_minimal() + 
  labs(title = "Probability of a Conservative Decision",
       x = "Maximum Likelihood Estimate",
       y = "Simulation Estimate")
ggsave("doc/figs/ge-pr.pdf", plot = gg, height = 3, width = 4)

# first difference for sg
X_lo <- X_hi <- X_c
X_lo[, "sg"] <- 0
X_hi[, "sg"] <- 1

tau_tilde <- t(pnorm(X_hi%*%t(beta_tilde)) - pnorm(X_lo%*%t(beta_tilde)))
tau_hat_avg <- apply(tau_tilde, 2, mean)  # simulation average

tau_hat_mle <- pnorm(X_hi%*%beta_hat) - pnorm(X_lo%*%beta_hat)  # mle
tau_df <- data.frame(mle = tau_hat_mle, 
                     avg = tau_hat_avg) 
gg2 <- ggplot(tau_df, aes(x = mle, avg)) + 
  geom_point(shape = 21) + 
  geom_abline(intercept = 0, slope = 1) + 
  theme_minimal() + 
  labs(title = "Effect of a Solicitor General Brief",
       x = "Maximum Likelihood Estimate",
       y = "Simulation Estimate")
ggsave("doc/figs/ge-fd.pdf", plot = gg2, height = 3, width = 4)

# hanmer and kalkan
summarize(tau_df, avg_avg = mean(avg), avg_mle = mean(mle))
  