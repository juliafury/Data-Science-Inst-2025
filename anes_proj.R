library(here)
library(tidyverse)
library(dplyr)
library(ggplot2)

anes_data <- read_csv(here("data", "anes_pilot_2016.csv"))

vote2016 <- anes_data$vote16dt
vote2016 <- ifelse(vote2016 == 9, NA, vote2016)
vote2016 <- factor(vote2016, 
                             levels = c(1, 2, 3, 4),
                             labels = c("Clinton", "Trump", "Someone Else", "Probably Not Voting"))


white_adv <- anes_data$wad4b
white_adv <- ifelse(white_adv == 9, NA, white_adv)

# graphing responses re: white advantage/disadvantage
# scale: 1 = "large advantage", 4 = "it does not make any difference",
# 7 = "large disadvantage"

ggplot(anes_data, aes(x = factor(white_adv, exclude = NULL))) +
  geom_bar(fill = "darkgreen") +
  labs(
    title = "Distribution of Responses on White Advantage/Disadvantage",
    x = "Responses (1-7, NA)",
    y = "Count"
  ) +
  theme_minimal()

ggplot(anes_data, aes(x = factor(white_adv), fill = factor(vote2016))) +
  geom_bar() +
  labs(
    title = "Relationship between 2016 Vote and Views on Racial Advantage/Disadvantage",
    x = "White Adv",
    y = "Count",
    fill = "Vote in 2016"
  ) +
  theme_minimal()


warm_do <- anes_data$warmdo
warm_do <- ifelse(warm_do == 9, NA, warm_do)

# scale: 1 = fed gov should be doing a great deal more, 4 = is doing the right
# amount, 7 = should be doing a great deal less

ggplot(anes_data, aes(x = factor(warm_do), fill = factor(vote2016))) +
  geom_bar() +
  labs(
    title = "Relationship between 2016 Vote and Views on Global Warming and the Federal Government",
    x = "Federal Government Should Do More About Global Warming",
    y = "Count",
    fill = "Vote in 2016"
  ) +
  theme_minimal()

pdf(here("figs", "white_adv_plot.pdf"))
ggplot(anes_data, aes(x, y_sq)) + geom_point() + geom_smooth()
dev.off()


