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

# Tests for `cleaned_match_data

test_that("cleaned_match_data 7 columns", {
  expect_equal(ncol(match_data), 7)
})

test_that("cleaned_match_data column types are as expected", {
  expect_type(match_data$team1, "character")
  expect_type(match_data$team2, "character")
  expect_true(lubridate::is.Date(match_data$date))  # Check if 'date' is a lubridate Date
  expect_type(match_data$winner, "character")
  expect_type(match_data$winner_wickets, "double") # Numeric columns
  expect_type(match_data$winner_runs, "double")
  expect_type(match_data$toss_winner, "character")
})

# Test that `winner_wickets` and `winner_runs` columns in `cleaned_match_data` have at least one NA value (indicating mutually exclusive fields)
test_that("cleaned_match_data has mutually exclusive winner_wickets and winner_runs", {
  expect_true(all(is.na(match_data$winner_wickets) | is.na(match_data$winner_runs)))
})

# Test that `cleaned_match_data` has no duplicated rows
test_that("cleaned_match_data has no duplicate rows", {
  expect_equal(nrow(match_data), nrow(distinct(match_data)))
})

# Tests for `cleaned_player_data`
test_that("cleaned_player_data has 14 columns", {
  expect_equal(ncol(player_data), 14)
})

test_that("cleaned_player_data column types are as expected", {
  expect_type(player_data$player, "character")
  expect_type(player_data$start, "double") # Numeric columns
  expect_type(player_data$end, "double")
  expect_type(player_data$matches, "double")
  expect_type(player_data$bat_innings, "double")
  expect_type(player_data$bowl_innings, "double")
  expect_type(player_data$field_innings, "double")
  expect_type(player_data$bat_runs, "double")
  expect_type(player_data$not_outs, "double")
  expect_type(player_data$bat_average, "double")
  expect_type(player_data$bowl_runs, "double")
  expect_type(player_data$wickets, "double")
  expect_type(player_data$economy, "double")
  expect_type(player_data$dismissals, "double")
})

test_that("cleaned_player_data does not have duplicate players", {
  expect_equal(length(unique(player_data$player)), nrow(player_data))
})


test_that("cleaned_player_data numerical columns have reasonable values", {
  expect_true(all(player_data$matches >= 0))
  expect_true(all(player_data$bat_innings >= 0))
  expect_true(all(player_data$bowl_innings >= 0))
  expect_true(all(player_data$field_innings >= 0))
})

# Test that `cleaned_player_data` 'bat_average' column is correctly calculated when applicable
test_that("cleaned_player_data 'bat_average' column has reasonable values", {
  expect_true(all(player_data$bat_average >= 0, na.rm = TRUE))
  expect_true(all(player_data$bat_average <= player_data$bat_runs, na.rm = TRUE))
})

# Test that `cleaned_player_data` bowling statistics are consistent
test_that("cleaned_player_data bowling statistics are consistent", {
  expect_true(all(player_data$wickets >= 0))
})

test_that("cleaned_player_data start year is earlier than or equal to end year", {
  expect_true(all(player_data$start <= player_data$end, na.rm = TRUE))
})

test_that("cleaned_player_data dismissals column is reasonable", {
  expect_true(all(player_data$dismissals >= 0, na.rm = TRUE))
  expect_true(all(player_data$dismissals <= player_data$matches*10, na.rm = TRUE))
})

test_that("cleaned_player_data runs are non-negative", {
  expect_true(all(player_data$bat_runs >= 0, na.rm = TRUE))
})
