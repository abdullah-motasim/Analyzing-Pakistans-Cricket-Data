#### Preamble ####
# Purpose: Models Pakistans cricket data overtime
# Author: Muhammad Abdullah Motasim
# Date: 27 November 2024
# Contact: abdullah.motasim@mail.utoronto.ca
# License: MIT
# Pre-requisites: tidyverse, rstanarm, caret, dplyr, tidyr, and arrow libraries
# Any other information needed?  The cleaned match and player data should be saved 
# in 02-analysis_data folder


#### Workspace setup ####
library(tidyverse)
library(rstanarm)
library(arrow)
library(caret) # For data splitting and evaluation
library(dplyr)
library(tidyr)

#### Read data ####
player_data <- read_parquet("data/02-analysis_data/cleaned_player_data.parquet")
match_data <- read_parquet("data/02-analysis_data/cleaned_match_data.parquet")
pakistan_match_data <- read_parquet("data/02-analysis_data/cleaned_pakistan_match_data.parquet")

# Filter match data for Pakistan
pakistan_matches <- pakistan_match_data %>%
  mutate(pakistan_win = case_when(
    winner == "Pakistan" ~ 1,
    TRUE ~ 0))%>%
  mutate(toss_win = case_when(
    toss_winner == "Pakistan" ~ 1,
    TRUE ~ 0))%>%
  mutate(year = lubridate::year(date))

# Summarize match-level performance
pakistan_match_summary <- pakistan_matches %>%
  group_by(year = lubridate::year(date)) %>%
  summarize(
    total_matches = n(),
    wins = sum(pakistan_win),
    win_rate = mean(pakistan_win),
    tosses_won = sum(toss_win)  # Sum the toss_win column to count tosses won by Pakistan
  )

set.seed(853)
match_train_index <- createDataPartition(pakistan_matches$pakistan_win, p = 0.8, list = FALSE)
match_train_data <- pakistan_matches[match_train_index, ]
match_test_data <- pakistan_matches[-match_train_index, ]

first_model <-
  stan_glm(
    formula = pakistan_win ~  year + team2 + toss_win,
    data = match_train_data,
    family = binomial(link = "logit"),
    prior = normal(location = 0, scale = 2.5, autoscale = TRUE),
    prior_intercept = normal(location = 0, scale = 2.5, autoscale = TRUE),
    prior_aux = exponential(rate = 1, autoscale = TRUE),
  )
  
summary(first_model)

#Predict probabilities for test data
match_test_data$predicted_prob_first <- predict(first_model, newdata = match_test_data, type = "response")

# Generate predicted classes based on a threshold (e.g., 0.5)
match_test_data$predicted_class_first <- ifelse(match_test_data$predicted_prob_first > 0.5, 1, 0)
# Evaluate model performance
confusion_matrix_first <- table(
  Actual = match_test_data$pakistan_win,
  Predicted = match_test_data$predicted_class_first
)

print(confusion_matrix_first)

accuracy_first <- sum(diag(confusion_matrix_first)) / sum(confusion_matrix_first)
print(paste("Accuracy:", round(accuracy_first, 2)))

#### Save model ####
saveRDS(first_model, file = "models/win_chance.rds")
saveRDS(confusion_matrix_first, file = "models/confusion_matrix_win_chance.rds")
