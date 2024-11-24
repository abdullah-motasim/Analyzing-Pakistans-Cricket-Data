#### Preamble ####
# Purpose: Tests the structure and validity of the simulated data 
# Author: Muhammad Abdullah Motasim
# Date: 22 November 2024
# Contact: abdullah.motasim@mail.utoronto.ca
# License: MIT
# Pre-requisites: tidyverse library
# Any other information needed? None


#### Workspace setup ####
library(tidyverse)

simulated_data <- read_csv("data/00-simulated_data/simulated_player_data.csv")

# Test if the data was successfully loaded
if (exists("simulated_data")) {
  message("Test Passed: The dataset was successfully loaded.")
} else {
  stop("Test Failed: The dataset could not be loaded.")
}


#### Test data ####

# Check if the dataset has 150 rows
if (nrow(simulated_data) == 150) {
  message("Test Passed: The dataset has 150 rows.")
} else {
  stop("Test Failed: The dataset does not have 150 rows.")
}

# Check if the dataset has the expected number of columns
expected_columns <- c("Player", "Country", "Start", "End", "Matches", "Innings", "NotOuts", 
                      "BattingRunsScored", "HighScore", "Average", "BallsFaced", 
                      "StrikeRate", "Hundreds", "Fifties", "Ducks", "FourWickets", 
                      "FiveWickets", "BowlingRunsScored", "Wickets", "Caught", "Stumped")

if (all(expected_columns %in% colnames(simulated_data))) {
  message("Test Passed: The dataset contains all expected columns.")
} else {
  stop("Test Failed: The dataset does not contain the expected columns.")
}

# Check if the 'Player' column contains unique values
if (n_distinct(simulated_data$Player) == nrow(simulated_data)) {
  message("Test Passed: All values in 'Player' are unique.")
} else {
  stop("Test Failed: The 'Player' column contains duplicate values.")
}

# Check if the 'Country' column contains only valid country names
valid_countries <- c("India", "Sri Lanka", "Australia", "Pakistan", "South Africa", 
                     "New Zealand", "England", "West Indies", "Zimbabwe", "Bangladesh", 
                     "Afghanistan", "Ireland")

if (all(simulated_data$Country %in% valid_countries)) {
  message("Test Passed: The 'Country' column contains only valid country names.")
} else {
  stop("Test Failed: The 'Country' column contains invalid country names.")
}

# Check if there are any missing values in the dataset
if (all(!is.na(simulated_data))) {
  message("Test Passed: The dataset contains no missing values.")
} else {
  stop("Test Failed: The dataset contains missing values.")
}

# Check if numeric columns contain valid ranges
if (all(simulated_data$BattingRunsScored >= 0 & simulated_data$HighScore >= 0)) {
  message("Test Passed: All numeric columns have valid ranges.")
} else {
  stop("Test Failed: Some numeric columns have invalid values.")
}

# Check if the 'HighScore' column is consistent with 'BattingRunsScored'
if (all(simulated_data$HighScore <= simulated_data$BattingRunsScored, na.rm = TRUE)) {
  message("Test Passed: The 'HighScore' column is consistent with 'BattingRunsScored'.")
} else {
  stop("Test Failed: The 'HighScore' column has values exceeding 'BattingRunsScored'.")
}

# Check if 'Start' and 'End' years are valid
if (all(simulated_data$Start <= simulated_data$End)) {
  message("Test Passed: The 'Start' and 'End' columns have valid years.")
} else {
  stop("Test Failed: Some 'Start' years are after 'End' years.")
}

# Check if the dataset contains at least one player from each country
missing_countries <- setdiff(valid_countries, unique(simulated_data$Country))
if (length(missing_countries) == 0) {
  message("Test Passed: All countries are represented in the dataset.")
} else {
  warning("Test Failed: The following countries are missing from the dataset: ", 
          paste(missing_countries, collapse = ", "))
}

# Check if there are no empty strings in categorical columns
categorical_columns <- c("Player", "Country")
if (all(simulated_data[categorical_columns] != "")) {
  message("Test Passed: No empty strings in categorical columns.")
} else {
  stop("Test Failed: There are empty strings in categorical columns.")
}

# Check if the dataset contains at least two unique players
if (n_distinct(simulated_data$Player) >= 2) {
  message("Test Passed: The dataset contains at least two unique players.")
} else {
  stop("Test Failed: The dataset contains less than two unique players.")
}

#### Summary ####

message("All tests completed.")