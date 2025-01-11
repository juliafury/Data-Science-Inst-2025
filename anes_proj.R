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

impute_mode <- function(x) {
  mode_val <- as.numeric(names(sort(table(x), decreasing = TRUE)[1]))
  ifelse(is.na(x), mode_val, x)
}

white_adv <- impute_mode(white_adv)
vote2016 <- factor(impute_mode(as.numeric(vote2016)),
                   levels = c(1, 2, 3, 4),
                   labels = c("Clinton", "Trump", "Someone Else", "Probably Not Voting"))

impute_mean <- function(x) {
  mean_val <- mean(x, na.rm = TRUE)
  ifelse(is.na(x), mean_val, x)
}

warmdo <- impute_mean(anes_data$warmdo)
immig_numb <- impute_mean(anes_data$immig_numb)
econnow <- impute_mean(anes_data$econnow)

anes_data$vote2016 <- vote2016
anes_data$white_adv <- white_adv
anes_data$warmdo <- warmdo
anes_data$immig_numb <- immig_numb
anes_data$econnow <- econnow

ggplot(anes_data, aes(x = factor(white_adv, exclude = NULL))) +
  geom_bar(fill = "darkgreen") +
  labs(
    title = "Distribution of Responses on White Advantage/Disadvantage",
    x = "Responses",
    y = "Count"
  ) +
  theme_minimal()

ggplot(anes_data, aes(x = factor(white_adv), fill = factor(vote2016))) +
  geom_bar() +
  labs(
    title = "Relationship between 2016 Vote and Views on Racial Advantage/Disadvantage",
    x = "Views",
    y = "Count",
    fill = "Vote in 2016"
  ) +
  theme_minimal()

ggplot(anes_data, aes(x = factor(warmdo, exclude = NULL))) +
  geom_bar(fill = "orange") +
  labs(
    title = "The Federal Government Should Do More/Less About Global Warming",
    x = "Responses",
    y = "Count"
  ) +
  theme_minimal()

ggplot(anes_data, aes(x = factor(warmdo), fill = factor(vote2016))) +  
  geom_bar() +
  labs(
    title = "Relationship between 2016 Vote and Views on Global Warming and the Federal Government",
    x = "Federal Government Should Do More About Global Warming",
    y = "Count",
    fill = "Vote in 2016"
  ) +
  theme_minimal()

ggplot(anes_data, aes(x = factor(immig_numb, exclude = NULL))) +
  geom_bar(fill = "red") +
  labs(
    title = "Public Opinion Immigration Numbers (2016)",
    x = "Responses",
    y = "Count"
  ) +
  theme_minimal()

ggplot(anes_data, aes(x = factor(immig_numb), fill = factor(vote2016))) +
  geom_bar() +
  labs(
    title = "Relationship between 2016 Vote and Public Opinion Immigration Numbers",
    x = "Public Opinions",
    y = "Count",
    fill = "Vote in 2016"
  ) +
  theme_minimal()

ggplot(anes_data, aes(x = factor(econnow, exclude = NULL))) +
  geom_bar(fill = "purple") +
  labs(
    title = "Views on Economy",
    x = "Responses",
    y = "Count"
  ) +
  theme_minimal()

ggplot(anes_data, aes(x = factor(econnow), fill = factor(vote2016))) +
  geom_bar() +
  labs(
    title = "Relationship between 2016 Vote and Views on Economy",
    x = "Views on Economy",
    y = "Count",
    fill = "Vote in 2016"
  ) +
  theme_minimal()

