#### Preamble ####
# Purpose: Tests the structure and validity of the simulated data
# Author: Muhammad Abdullah Motasim
# Date: 22 November 2024
# Contact: abdullah.motasim@mail.utoronto.ca
# License: MIT
# Pre-requisites: tidyverse, testthat, and lubridate libraries
# Any other information needed? None


#### Workspace setup ####
library(tidyverse)
library(testthat)
library(lubridate)

# Load the simulated datasets
match_data <- read_parquet("data/00-simulated_data/simulated_match_data.parquet")
player_data <- read_parquet("data/00-simulated_data/simulated_player_data.parquet")

#### Tests for simulated_match_data ####

# Test that there are 100 matches
test_that("simulated_match_data contains 100 matches", {
  expect_equal(nrow(match_data), 100)
})

# Test that match_data has 10 columns
test_that("simulated_match_data has 10 columns", {
  expect_equal(ncol(match_data), 10)
})

# Test that match_data columns are of the correct type
test_that("simulated_match_data column types are as expected", {
  expect_type(match_data$team1, "character")
  expect_type(match_data$team2, "character")
  expect_true(lubridate::is.Date(match_data$date)) # Check if 'date' is a lubridate Date
  expect_type(match_data$winner, "character")
  expect_type(match_data$team1_runs, "integer") # Numeric columns
  expect_type(match_data$team2_runs, "integer")
  expect_type(match_data$team1_wickets, "integer")
  expect_type(match_data$team2_wickets, "integer")
  expect_type(match_data$toss_winner, "character")
})

# Test that 'team1' and 'team2' are a valid country from the list
test_that("simulated_match_data team1 and team2 are valid countries", {
  valid_countries <- c(
    "India", "Australia", "England", "Pakistan", "South Africa", "Sri Lanka",
    "New Zealand", "West Indies", "Bangladesh", "Zimbabwe", "Afghanistan", "Ireland"
  )
  expect_true(all(match_data$team1 %in% valid_countries))
  expect_true(all(match_data$team2 %in% valid_countries))
})

# Test that simulated_match_data has no duplicate rows
test_that("simulated_match_data has no duplicate rows", {
  expect_equal(nrow(match_data), nrow(distinct(match_data)))
})

# Test that team1 and team2 are distinct
test_that("team1 and team2 are distinct in every match", {
  expect_true(all(match_data$team1 != match_data$team2))
})

# Test that winner is consistent with runs scored
test_that("winner is consistent with runs scored", {
  correct_winner <- ifelse(match_data$team1_runs > match_data$team2_runs, "team1", "team2")
  expect_true(all(match_data$winner == correct_winner))
})

# Test that wickets do not exceed 10 per team
test_that("wickets do not exceed 10 per team", {
  expect_true(all(match_data$team1_wickets <= 10))
  expect_true(all(match_data$team2_wickets <= 10))
})

# Test that toss_winner is either 'team1' or 'team2'
test_that("toss_winner is valid", {
  expect_true(all(match_data$toss_winner %in% c("team1", "team2")))
})

#### Tests for simulated_player_data ####

# Test that there are 150 players
test_that("simulated_player_data contains 150 players", {
  expect_equal(nrow(player_data), 150)
})

# Test that 'Country' is a valid country from the list
test_that("simulated_player_data Country is valid", {
  valid_countries <- c(
    "India", "Australia", "England", "Pakistan", "South Africa", "Sri Lanka",
    "New Zealand", "West Indies", "Bangladesh", "Zimbabwe", "Afghanistan", "Ireland"
  )
  expect_true(all(player_data$Country %in% valid_countries))
})

# Test that Start year is less than or equal to End year
test_that("Start year is less than or equal to End year", {
  expect_true(all(player_data$Start <= player_data$End))
})

# Test that BattingRunsScored is greater than or equal to HighScore
test_that("BattingRunsScored is greater than or equal to HighScore", {
  expect_true(all(player_data$BattingRunsScored >= player_data$HighScore))
})

# Test that no player has more centuries than half-centuries
test_that("Hundreds cannot exceed Fifties", {
  expect_true(all(player_data$Hundreds <= player_data$Fifties))
})

# Test: Ensure there are no negative values for key statistics
test_that("No negative values for key statistics", {
  expect_true(all(player_data$Bat_innings >= 0))
  expect_true(all(player_data$Bowl_innings >= 0))
  expect_true(all(player_data$BattingRunsScored >= 0))
  expect_true(all(player_data$BallsFaced >= 0))
  expect_true(all(player_data$StrikeRate >= 0))
  expect_true(all(player_data$Hundreds >= 0))
  expect_true(all(player_data$Fifties >= 0))
  expect_true(all(player_data$BowlingRunsScored >= 0))
  expect_true(all(player_data$Wickets >= 0))
  expect_true(all(player_data$Caught >= 0))
  expect_true(all(player_data$Stumped >= 0))
})

test_that("simulated_player_data contains at least one player from each country", {
  valid_countries <- c(
    "India", "Australia", "England", "Pakistan", "South Africa", "Sri Lanka",
    "New Zealand", "West Indies", "Bangladesh", "Zimbabwe", "Afghanistan", "Ireland"
  )
  missing_countries <- setdiff(valid_countries, unique(player_data$Country))
  expect_true(length(missing_countries) == 0)
})
