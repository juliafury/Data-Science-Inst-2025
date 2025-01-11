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

ggplot(anes_data_clean, aes(x = factor(white_adv), fill = factor(vote2016))) +
  geom_bar() +
  labs(
    title = "Relationship between 2016 Vote and Views on Racial Advantage/Disadvantage",
    x = "Views",
    y = "Count",
    fill = "Vote in 2016"
  ) +
  theme_minimal()

warmdo <- anes_data$warmdo

anes_data_clean <- anes_data %>%
  filter(!is.na(vote2016) & !is.na(warmdo))

ggplot(anes_data_clean, aes(x = factor(warmdo, exclude = NULL))) +
  geom_bar(fill = "orange") +
  labs(
    title = "The Federal Government Should Do More/Less About Global Warming",
    x = "Responses",
    y = "Count"
  ) +
  theme_minimal()

ggplot(anes_data_clean, aes(x = factor(warmdo), fill = factor(vote2016))) +  
  geom_bar() +
  labs(
    title = "Relationship between 2016 Vote and Views on Global Warming and the Federal Government",
    x = "Federal Government Should Do More About Global Warming",
    y = "Count",
    fill = "Vote in 2016"
  ) +
  theme_minimal()

immig_numb <- anes_data$immig_numb

anes_data_clean <- anes_data %>%
  filter(!is.na(vote2016) & !is.na(immig_numb))

ggplot(anes_data_clean, aes(x = factor(immig_numb, exclude = NULL))) +
  geom_bar(fill = "red") +
  labs(
    title = "Public Opinion Immigration Numbers (2016)",
    x = "Responses",
    y = "Count"
  ) +
  theme_minimal()

ggplot(anes_data_clean, aes(x = factor(immig_numb), fill = factor(vote2016))) +
  geom_bar() +
  labs(
    title = "Relationship between 2016 Vote and Public Opinion Immigration Numbers",
    x = "Public Opinions",
    y = "Count",
    fill = "Vote in 2016"
  ) +
  theme_minimal()

econnow <- anes_data$econnow

anes_data_clean <- anes_data %>%
  filter(!is.na(vote2016) & !is.na(econnow))

ggplot(anes_data_clean, aes(x = factor(econnow, exclude = NULL))) +
  geom_bar(fill = "purple") +
  labs(
    title = "Views on Economy",
    x = "Responses",
    y = "Count"
  ) +
  theme_minimal()

ggplot(anes_data_clean, aes(x = factor(econnow), fill = factor(vote2016))) +
  geom_bar() +
  labs(
    title = "Relationship between 2016 Vote and Views on Economy",
    x = "Views on Economy",
    y = "Count",
    fill = "Vote in 2016"
  ) +
  theme_minimal()
