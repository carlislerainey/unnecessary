
# load packages
library(tidyverse)

# read in the biases
tall_bias_df <- read_rds("data/nagler-fd-bias.rds")

# colors 
my_colors <- c("#1b9e77", "#d95f02", "#7570b3")

# compute the avg. abs. bias
aab_df <- tall_bias_df %>% 
  group_by(concept, sample_size_fct, sample_size) %>%
  summarize(aab = format(round(mean(abs(bias)), 3), digits = 3, nsmall = 3),
            x = max(tall_bias_df$tau),
            y = max(tall_bias_df$bias)) %>%
  mutate(aab_text = ifelse(concept == "Coefficient-Induced Bias" & sample_size == 100,
                           paste0("avg. abs. bias = ", aab), as.character(aab))) %>%
  glimpse()

# create a data frame of annotations
annotate_df <- data.frame(x = -0.025, 
                          y = 0.004, 
                          label = "10% bias", 
                          concept = "Coefficient-Induced Bias",
                          sample_size_fct = "N = 100")

# plot biases
ggplot() +
  geom_abline(intercept = 0, 
              slope = 0.1, 
              size = 0.2,
              color = my_colors[2], 
              linetype = "solid") + 
  geom_abline(intercept = 0,
              slope = -0.1, 
              size = 0.2,
              color = my_colors[2], 
              linetype = "solid") + 
  geom_rug(data = tall_bias_df, 
           aes(x = tau, y = bias), 
           sides = "bl", 
           alpha = 0.1,
           color = my_colors[1]) +
  geom_point(data = tall_bias_df, 
             aes(x = tau, y = bias),
             size = 1, alpha = 0.5, pch = 21) + 
  facet_grid(sample_size_fct ~ concept)  + 
  geom_text(data = aab_df, aes(label = aab_text, x = x, y = y),
            size = 2.7,
            hjust = 1.0, vjust = 1, 
            color = my_colors[3]) +
  theme_minimal() + 
  labs(x = "Actual Effect of a 10-Day Shift in Closing Date",
       y = "Bias in Estimated Effect") + 
  geom_text(data = annotate_df, aes(x = x, y = y, label = label),
            color = my_colors[2], size = 2.7) + 
  scale_x_continuous(breaks = seq(-0.04, 0.0, by = 0.01))

# save plot
ggsave("doc/figs/nagler-fd-bias.pdf", height = 5, width = 4, scale = 1.6)
