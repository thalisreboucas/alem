library("ggplot2")
library("dplyr")
library("reshape2")

options(scipen = 999)
set.seed(1)

comp1.vals <- data_frame(comp = "A", 
                         vals = rnorm(50, mean = 1, sd = 0.5))
comp2.vals <- data_frame(comp = "B", 
                         vals = rnorm(50, mean = 1.5, sd = 0.5))

vals.df <- bind_rows(comp1.vals, comp2.vals)

vals.df %>%
  ggplot(aes(x = vals, y = "A", color = factor(comp))) +
  geom_point(alpha = 0.4) +
  scale_color_discrete(name = "Source of Data") +
  xlab("Values") +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "top")


vals.df %>%
  group_by(comp) %>%
  summarize(mean_vals = mean(vals),
            sd_vals = sd(vals))