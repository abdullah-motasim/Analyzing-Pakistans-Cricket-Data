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

#### Read data ####
player_data <- read_parquet("data/02-analysis_data/cleaned_player_data.parquet")
match_data <- read_parquet("data/02-analysis_data/cleaned_match_data.parquet")
pakistan_match_data <- read_parquet("data/02-analysis_data/cleaned_pakistan_match_data.parquet")

# Add binary column for Pakistan won match to train model
pakistan_matches <- pakistan_match_data %>%
  mutate(pakistan_win = case_when(
    winner == "Pakistan" ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(toss_win = case_when(
    toss_winner == "Pakistan" ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(year = lubridate::year(date)) # Extract year from date column into seperate column

# Summarize match-level performance
pakistan_match_summary <- pakistan_matches %>%
  group_by(year = lubridate::year(date)) %>% # Group data by the year of the match
  summarize(
    total_matches = n(), # Count the total number of matches in each year
    wins = sum(pakistan_win), # Sum the 'pakistan_win' column to get total wins in each year
    win_rate = mean(pakistan_win), # Calculate the win rate as the mean of 'pakistan_win'
    tosses_won = sum(toss_win) # Sum the 'toss_win' column to count tosses won by Pakistan
  )

set.seed(853)
# # Split data into training (80%) and test (20%) sets based on Pakistan's match outcome
match_train_index <- createDataPartition(pakistan_matches$pakistan_win, p = 0.8, list = FALSE)
match_train_data <- pakistan_matches[match_train_index, ]
match_test_data <- pakistan_matches[-match_train_index, ]

# Fit a logistic regression model (stan_glm) to predict Pakistan's win based on year, opponent, and toss result
first_model <-
  stan_glm(
    formula = pakistan_win ~ year + team2 + toss_win, # Predict 'pakistan_win' using 'year', 'team2' (opponent), and 'toss_win'
    data = match_train_data, # Use training data
    family = binomial(link = "logit"), # Logistic regression (binomial outcome)
    prior = normal(location = 0, scale = 2.5, autoscale = TRUE), # Normal prior for coefficients
    prior_intercept = normal(location = 0, scale = 2.5, autoscale = TRUE), # Normal prior for intercept
    prior_aux = exponential(rate = 1, autoscale = TRUE) # Exponential prior for auxiliary parameters (variance)
  )

# Predict probabilities for test data
match_test_data$predicted_prob_first <- predict(first_model, newdata = match_test_data, type = "response")

# Generate predicted classes based on a threshold (e.g., 0.5)
match_test_data$predicted_class_first <- ifelse(match_test_data$predicted_prob_first > 0.5, 1, 0)
# Evaluate model performance
confusion_matrix_first <- table(
  Actual = match_test_data$pakistan_win,
  Predicted = match_test_data$predicted_class_first
)

accuracy_first <- sum(diag(confusion_matrix_first)) / sum(confusion_matrix_first)
print(paste("Accuracy:", round(accuracy_first, 2)))

#### Save model ####
saveRDS(first_model, file = "models/win_chance.rds")
saveRDS(confusion_matrix_first, file = "models/confusion_matrix_win_chance.rds")
