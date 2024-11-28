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

# Filter match data for Pakistan and ensure Pakistan is always team1
pakistan_matches <- match_data %>%
  mutate(
    team1 = ifelse(team1 == "Pakistan", "Pakistan", team2),  # Assign Pakistan to team1
    team2 = ifelse(team1 == "Pakistan", team2, team1)         # Assign the other team to team2
  ) %>%
  filter(team1 == "Pakistan") %>%  # Filter for matches where Pakistan is team1
  mutate(pakistan_win = case_when(
    winner == "Pakistan" ~ 1,
    TRUE ~ 0
  ))

# Summarize match-level performance
pakistan_match_summary <- pakistan_matches %>%
  group_by(year = lubridate::year(date)) %>%
  summarize(
    total_matches = n(),
    wins = sum(pakistan_win),
    win_rate = mean(pakistan_win)
  )

pakistan_players <- player_data %>%
  filter(country == "Pakistan")

# Summarize player performance
pakistan_player_summary <- pakistan_players %>%
  group_by(player, years_played = end-start) %>%
  summarize(
    total_runs = sum(runs, na.rm = TRUE),
    total_wickets = sum(wickets, na.rm = TRUE),
    matches = n()
  )

# Merge with match data
pakistan_model_data <- pakistan_matches %>%
  mutate(year = lubridate::year(date))

set.seed(853)
match_train_index <- createDataPartition(pakistan_model_data$pakistan_win, p = 0.8, list = FALSE)
match_train_data <- pakistan_model_data[match_train_index, ]
match_test_data <- pakistan_model_data[-match_train_index, ]

first_model <-
  stan_glm(
    formula = pakistan_win ~  poly(year, 2) + team2,
    data = match_train_data,
    family = binomial(link = "logit"),
    prior = normal(location = 0, scale = 2.5, autoscale = TRUE),
    prior_intercept = normal(location = 0, scale = 2.5, autoscale = TRUE),
    prior_aux = exponential(rate = 1, autoscale = TRUE),
  )

second_model <-
  stan_glm(
    formula = pakistan_win ~ year + team2,
    data = match_train_data,
  family = binomial(link = "logit"),
  prior = normal(0, 2.5),
  prior_intercept = normal(0, 5),
  chains = 4,
  iter = 2000,
)

summary(first_model)
summary(second_model)

#Predict probabilities for test data
test_data$predicted_prob_first <- predict(first_model, newdata = test_data, type = "response")
test_data$predicted_prob_second <- predict(second_model, newdata = test_data, type = "response")

# Generate predicted classes based on a threshold (e.g., 0.5)
test_data$predicted_class_first <- ifelse(test_data$predicted_prob_first > 0.5, 1, 0)
test_data$predicted_class_second <- ifelse(test_data$predicted_prob_second > 0.5, 1, 0)

# Evaluate model performance
confusion_matrix_first <- table(
  Actual = test_data$pakistan_win,
  Predicted = test_data$predicted_class_first
)
confusion_matrix_second <- table(
  Actual = test_data$pakistan_win,
  Predicted = test_data$predicted_class_second
)
print(confusion_matrix_first)
print(confusion_matrix_second)

accuracy_first <- sum(diag(confusion_matrix_first)) / sum(confusion_matrix_first)
accuracy_second <- sum(diag(confusion_matrix_second)) / sum(confusion_matrix_second)
print(paste("Accuracy:", round(accuracy_first, 2)))
print(paste("Accuracy:", round(accuracy_second, 2)))

# Model for predicting players career? Not sure if needed
player_yearly_summary <- player_data %>%
  # Group by player to aggregate stats over their entire career
  group_by(player) %>%
  summarize(
    country = country,
    career_runs = sum(runs, na.rm = TRUE),
    career_matches = sum(matches, na.rm = TRUE),
    career_strike_rate = mean(strike_rate, na.rm = TRUE),
    career_average = mean(average, na.rm = TRUE),
    career_hundreds = sum(hundreds, na.rm = TRUE),
    career_fifties = sum(fifties, na.rm = TRUE),
    start_year = min(start),  # earliest year
    end_year = max(end)       # latest year
  ) %>%
  ungroup()

set.seed(853)
player_train_index <- createDataPartition(player_yearly_summary$yearly_runs, p = 0.8, list = FALSE)
player_train_data <- player_yearly_summary[train_index, ]
player_test_data <- player_yearly_summary[-train_index, ]

# Count the number of missing values per column
col_na_counts <- colSums(is.na(player_train_data))

# View the count of missing values in each column
print(col_na_counts)

player_future_runs_model <- stan_glm(
  formula = yearly_runs ~ lagged_yearly_runs + lagged_yearly_matches + lagged_yearly_strike_rate + lagged_yearly_average + year,
  data = player_train_data_clean,
  family = gaussian(),
  prior = normal(location = 0, scale = 2.5, autoscale = TRUE),
  prior_intercept = normal(location = 0, scale = 2.5, autoscale = TRUE),
  chains = 4,
  iter = 2000
)

#### Save model ####
saveRDS(
  first_model,
  file = "models/first_model.rds"
)


