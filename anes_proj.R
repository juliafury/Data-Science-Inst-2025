library(here)
library(tidyverse)
library(stargazer)
# Please cite as: 

# `Hlavac, Marek (2022). stargazer: Well-Formatted Regression and Summary Statistics Tables.`
# `R package version 5.2.3. https://CRAN.R-project.org/package=stargazer `

anes_data <- read_csv(here("data", "anes_pilot_2016.csv"))

vote2016 <- anes_data$vote16dt
vote2016 <- ifelse(vote2016 == 9, NA, vote2016)
vote2016 <- factor(vote2016, 
                   levels = c(1, 2, 3, 4),
                   labels = c("Clinton", "Trump", "Someone Else", "Probably Not Voting"))

vote2016_binary <- case_when(
  vote2016 == "Clinton" ~ 0,
  vote2016 == "Trump" ~ 1,
  .default = NA
)

# logistic regression



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

lm()
model1 <- lm(vote2016_binary ~ white_adv, data = anes_data)
model1

lm_out <- lm(vote2016_binary ~ white_adv)
glm_out <- glm(vote2016_binary ~ x1 + x2)

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
    Role in Addressing Global Warming",
    x = "Opinion on Government Action About Global Warming",
    y = "Count",
    fill = "Vote in 2016"
  ) +
  theme_minimal()
dev.off()

model2 <- lm(vote2016_binary ~ warmdo, data = anes_data)
model2

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

model3 <- lm(vote2016_binary ~ immig_numb, data = anes_data)
model3

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

model4 <- lm(vote2016_binary ~ econnow, data = anes_data)
model4

model5 <- glm(vote2016_binary ~ white_adv + warmdo + immig_numb + econnow, 
             data = anes_data, 
             family = "binomial")
model5
stargazer(model1, model5)

model1 <- glm(vote2016_binary ~ white_adv, data = anes_data, family = "binomial")
model2 <- glm(vote2016_binary ~ warmdo, data = anes_data, family = "binomial")
model3 <- glm(vote2016_binary ~ immig_numb, data = anes_data, family = "binomial")
model4 <- glm(vote2016_binary ~ econnow, data = anes_data, family = "binomial")

model_all <- glm(vote2016_binary ~ white_adv + warmdo + immig_numb + econnow, 
                 data = anes_data, 
                 family = binomial(link = "logit"))

stargazer(model1, model2, model3, model4, model_all, type = "text", 
          title = "Regression Results for Political Variables")

predict(model_all, newdata = c(1, 1, 1, 1))

pred_data = anes_data |> select(white_adv, warmdo, immig_numb, econnow)

all_vote_preds <- predict(model_all, pred_data, type = "response")
df <- tibble(vote2016_binary, all_vote_preds)
df_complete <- na.omit(df)
binary_preds <- ifelse(df_complete$all_vote_preds > 0.5, 1, 0)
num_correct_preds <- sum(binary_preds == df_complete$vote2016_binary)
num_total_preds <- nrow(df_complete)
pct_correct <- num_correct_preds / num_total_preds
# expand.grid

indivs_example <- tibble(white_adv = c(4, 4), warmdo = c(1, 4), immig_numb = c(4, 4),
 econnow = c(3, 3))

indivs_pred <- predict(model_all, indiv_pred, type = "response")


