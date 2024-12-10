#### Preamble ####
# Purpose: Tests cleaned match and player data
# Author: Muhammad Abdullah Motasim
# Date: 23 November 2024
# Contact: abdullah.motasim@mail.utoronto.ca
# License: MIT
# Pre-requisites: tidyverse, dplyr, and arrow libraries
# Any other information needed? cleaned match and player data should be saved in 02-analysis_data


#### Workspace setup ####
library(tidyverse)
library(testthat)
library(arrow)

match_data <- read_parquet("data/02-analysis_data/cleaned_match_data.parquet")
player_data <- read_parquet("data/02-analysis_data/cleaned_player_data.parquet")


#### Test data ####

# Tests for `cleaned_match_data`

# Test to check if 'cleaned_match_data' has exactly 7 columns.
test_that("cleaned_match_data 7 columns", {
  expect_equal(ncol(match_data), 7)
})

# Test to ensure that the column types in 'cleaned_match_data' are as expected.
test_that("cleaned_match_data column types are as expected", {
  expect_type(match_data$team1, "character") # 'team1' should be a character (text) column
  expect_type(match_data$team2, "character") # 'team2' should be a character (text) column
  expect_true(lubridate::is.Date(match_data$date)) # 'date' should be a Date type, validated using lubridate
  expect_type(match_data$winner, "character") # 'winner' should be a character (text) column
  expect_type(match_data$winner_wickets, "double") # 'winner_wickets' should be a numeric (double) column
  expect_type(match_data$winner_runs, "double") # 'winner_runs' should be a numeric (double) column
  expect_type(match_data$toss_winner, "character") # 'toss_winner' should be a character (text) column
})

# Test to ensure that either 'winner_wickets' or 'winner_runs' is NA for each row, indicating that only one can have a value (mutually exclusive).
test_that("cleaned_match_data has mutually exclusive winner_wickets and winner_runs", {
  expect_true(all(is.na(match_data$winner_wickets) | is.na(match_data$winner_runs))) # Ensure only one column has a value at a time
})

# Test to check if there are any duplicate rows in 'cleaned_match_data'.
test_that("cleaned_match_data has no duplicate rows", {
  expect_equal(nrow(match_data), nrow(distinct(match_data))) # Check if the number of rows is equal to the number of unique rows
})

# Tests for `cleaned_player_data`

# Test to check if 'cleaned_player_data' has exactly 14 columns.
test_that("cleaned_player_data has 14 columns", {
  expect_equal(ncol(player_data), 14) # Ensure the number of columns is correct
})

# Test to ensure that column types in 'cleaned_player_data' are as expected (mainly numeric types for player statistics).
test_that("cleaned_player_data column types are as expected", {
  expect_type(player_data$player, "character") # 'player' should be a character (text) column
  expect_type(player_data$start, "double") # 'start' year should be numeric (double)
  expect_type(player_data$end, "double") # 'end' year should be numeric (double)
  expect_type(player_data$matches, "double") # 'matches' should be numeric (double)
  expect_type(player_data$bat_innings, "double") # 'bat_innings' should be numeric (double)
  expect_type(player_data$bowl_innings, "double") # 'bowl_innings' should be numeric (double)
  expect_type(player_data$field_innings, "double") # 'field_innings' should be numeric (double)
  expect_type(player_data$bat_runs, "double") # 'bat_runs' should be numeric (double)
  expect_type(player_data$not_outs, "double") # 'not_outs' should be numeric (double)
  expect_type(player_data$bat_average, "double") # 'bat_average' should be numeric (double)
  expect_type(player_data$bowl_runs, "double") # 'bowl_runs' should be numeric (double)
  expect_type(player_data$wickets, "double") # 'wickets' should be numeric (double)
  expect_type(player_data$economy, "double") # 'economy' should be numeric (double)
  expect_type(player_data$dismissals, "double") # 'dismissals' should be numeric (double)
})

# Test to ensure that there are no duplicate players in 'cleaned_player_data'.
test_that("cleaned_player_data does not have duplicate players", {
  expect_equal(length(unique(player_data$player)), nrow(player_data)) # Ensure each player is unique
})

# Test to check that numerical columns in 'cleaned_player_data' have reasonable (non-negative) values.
test_that("cleaned_player_data numerical columns have reasonable values", {
  expect_true(all(player_data$matches >= 0)) # Ensure 'matches' is non-negative
  expect_true(all(player_data$bat_innings >= 0)) # Ensure 'bat_innings' is non-negative
  expect_true(all(player_data$bowl_innings >= 0)) # Ensure 'bowl_innings' is non-negative
  expect_true(all(player_data$field_innings >= 0)) # Ensure 'field_innings' is non-negative
})

# Test to ensure that the 'bat_average' column is correctly calculated (non-negative and logically consistent).
test_that("cleaned_player_data 'bat_average' column has reasonable values", {
  expect_true(all(player_data$bat_average >= 0, na.rm = TRUE)) # Batting average should be non-negative
  expect_true(all(player_data$bat_average <= player_data$bat_runs, na.rm = TRUE)) # Batting average should not exceed total runs
})

# Test to ensure that bowling statistics (wickets) are consistent (non-negative).
test_that("cleaned_player_data bowling statistics are consistent", {
  expect_true(all(player_data$wickets >= 0)) # Ensure 'wickets' is non-negative
})

# Test to ensure that the start year is earlier than or equal to the end year for each player.
test_that("cleaned_player_data start year is earlier than or equal to end year", {
  expect_true(all(player_data$start <= player_data$end, na.rm = TRUE)) # Ensure start year is before or equal to end year
})

# Test to ensure that the 'dismissals' column has reasonable values, i.e., it is non-negative and does not exceed the number of matches.
test_that("cleaned_player_data dismissals column is reasonable", {
  expect_true(all(player_data$dismissals >= 0, na.rm = TRUE)) # Ensure dismissals are non-negative
  expect_true(all(player_data$dismissals <= player_data$matches * 10, na.rm = TRUE)) # Dismissals should not exceed 10 times the number of matches
})

# Test to ensure that batting runs ('bat_runs') are non-negative.
test_that("cleaned_player_data runs are non-negative", {
  expect_true(all(player_data$bat_runs >= 0, na.rm = TRUE)) # Ensure 'bat_runs' is non-negative
})
