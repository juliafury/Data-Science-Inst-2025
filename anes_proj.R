library(here)
library(tidyverse)
library(dplyr)
library(ggplot2)

anes_data <- read_csv(here("data", "anes_pilot_2016.csv"))

white_adv <- anes_data$wad4b
white_adv <- ifelse(white_adv == 9, NA, white_adv)

ggplot(anes_data, aes(x = factor(white_adv, exclude = NULL))) +
  geom_bar(fill = "blue") +
  labs(
    title = "Distribution of Responses on White Advantage/Disadvantage",
    x = "Responses (1-7, NA)",
    y = "Count"
  ) +
  theme_minimal()

pdf(here("figs", "white_adv_plot.pdf"))
ggplot(anes_data, aes(x, y_sq)) + geom_point() + geom_smooth()
dev.off()


