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

anes_data$vote2016 <- vote2016
anes_data$white_adv <- white_adv

anes_data_clean <- anes_data %>%
  filter(!is.na(vote2016) & !is.na(white_adv))

ggplot(anes_data_clean, aes(x = factor(white_adv, exclude = NULL))) +
  geom_bar(fill = "darkgreen") +
  labs(
    title = "Distribution of Responses on White Advantage/Disadvantage",
    x = "Responses",
    y = "Count"
  ) +
  theme_minimal()

pdf(here("figs", "racial_adv.pdf"))
ggplot(anes_data_clean, aes(x = factor(white_adv), fill = factor(vote2016))) +
  geom_bar() +
  labs(
    title = "Relationship between 2016 Vote and Views on Racial Advantage/Disadvantage",
    x = "Perception of White Advantage/Disadvantage",
    y = "Count",
    fill = "Vote in 2016"
  ) +
  theme_minimal()
dev.off()

warmdo <- anes_data$warmdo

anes_data_clean <- anes_data %>%
  filter(!is.na(vote2016) & !is.na(warmdo))

ggplot(anes_data_clean, aes(x = factor(warmdo, exclude = NULL))) +
  geom_bar(fill = "orange") +
  labs(
    title = "The Federal Government Should Do More/Less About Global Warming",
    x = "Response",
    y = "Count"
  ) +
  theme_minimal()

pdf(here("figs", "gov_warming.pdf"))
ggplot(anes_data_clean, aes(x = factor(warmdo), fill = factor(vote2016))) +  
  geom_bar() +
  labs(
    title = "Relationship between 2016 Vote and Opinion on Federal Government's
    Responsibility to Address Global Warming",
    x = "Opinion on Government Action About Global Warming",
    y = "Count",
    fill = "Vote in 2016"
  ) +
  theme_minimal()
dev.off()

immig_numb <- anes_data$immig_numb

anes_data_clean <- anes_data %>%
  filter(!is.na(vote2016) & !is.na(immig_numb))

ggplot(anes_data_clean, aes(x = factor(immig_numb, exclude = NULL))) +
  geom_bar(fill = "red") +
  labs(
    title = "Respondents' Opinions on Immigration Numbers",
    x = "Responses",
    y = "Count"
  ) +
  theme_minimal()

pdf(here("figs", "immig_opin.pdf"))
ggplot(anes_data_clean, aes(x = factor(immig_numb), fill = factor(vote2016))) +
  geom_bar() +
  labs(
    title = "Relationship between 2016 Vote and Respondents' Opinions on Immigration Numbers",
    x = "Opinion on Immigration Numbers",
    y = "Count",
    fill = "Vote in 2016"
  ) +
  theme_minimal()
dev.off()

econnow <- anes_data$econnow

anes_data_clean <- anes_data %>%
  filter(!is.na(vote2016) & !is.na(econnow))

ggplot(anes_data_clean, aes(x = factor(econnow, exclude = NULL))) +
  geom_bar(fill = "purple") +
  labs(
    title = "Views on Economy Compared To A Year Earlier",
    x = "Responses",
    y = "Count"
  ) +
  theme_minimal()

pdf(here("figs", "econ_opin.pdf"))
ggplot(anes_data_clean, aes(x = factor(econnow), fill = factor(vote2016))) +
  geom_bar() +
  labs(
    title = "Relationship between 2016 Vote and Views on Economy",
    x = "View on Economy Compared To A Year Earlier",
    y = "Count",
    fill = "Vote in 2016"
  ) +
  theme_minimal()
dev.off()


