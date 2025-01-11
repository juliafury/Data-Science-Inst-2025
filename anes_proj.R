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

anes_data$vote2016 <- vote2016
anes_data$warm_do <- warmdo

anes_data_clean <- anes_data %>%
  filter(!is.na(vote2016) & !is.na(warmdo))

ggplot(anes_data_clean, aes(x = factor(warmdo), fill = factor(vote2016))) +  # Use 'anes_data_clean' here
  geom_bar() +
  labs(
    title = "Relationship between 2016 Vote and Views on Global Warming and the Federal Government",
    x = "Federal Government Should Do More About Global Warming",
    y = "Count",
    fill = "Vote in 2016"
  ) +
  theme_minimal()

immig_numb <- factor(anes_data$immig_numb, 
                     levels = c(1, 4, 7), 
                     labels = c("Increased a lot", "Kept the same", "Decreased a lot"))
immig_numb <- na.omit(immig_numb)

library(ggplot2)

ggplot(data = data.frame(immig_numb), aes(x = immig_numb)) +
  geom_bar(fill = "red") + 
  labs(title = "Public Opinion Immigration Numbers (2016)", 
       x = "Should immigration numbers be increased, decreased, or kept the same", 
       y = "Count of Respondents")

economy <- anes_data$econnow
ggplot(anes_data, aes(x = factor(economy), fill = factor(vote2016))) +
  geom_bar() +
  labs(
    title = "Relationship between 2016 Vote and Views on Economy",
    x = "Economy compared to one year ago",
    y = "Count",
    fill = "Vote in 2016"
  ) +
  theme_minimal()
