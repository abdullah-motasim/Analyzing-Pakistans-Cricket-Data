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

test_that("cleaned_match_data 8 columns", {
  expect_equal(ncol(match_data), 8)
})

test_that("cleaned_match_data column types are as expected", {
  expect_type(match_data$team1, "character")
  expect_type(match_data$team2, "character")
  expect_true(lubridate::is.Date(match_data$date))  # Check if 'date' is a lubridate Date
  expect_type(match_data$winner, "character")
  expect_type(match_data$winner_wickets, "double") # Numeric columns
  expect_type(match_data$winner_runs, "double")
  expect_type(match_data$toss_winner, "character")
  expect_type(match_data$toss_decision, "character")
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
test_that("cleaned_player_data has 34 columns", {
  expect_equal(ncol(player_data), 34)
})

test_that("cleaned_player_data column types are as expected", {
  expect_type(player_data$player, "character")
  expect_type(player_data$country, "character")
  expect_type(player_data$start, "double") # Numeric columns
  expect_type(player_data$end, "double")
  expect_type(player_data$matches, "double")
  expect_type(player_data$bat_innings, "double")
  expect_type(player_data$bowl_innings, "double")
  expect_type(player_data$field_innings, "double")
  expect_type(player_data$not_outs, "double")
  expect_type(player_data$runs, "double")
  expect_type(player_data$high_score, "double")
  expect_type(player_data$high_score_not_out, "logical")
  expect_type(player_data$average, "double")
  expect_type(player_data$balls_faced, "double")
  expect_type(player_data$strike_rate, "double")
  expect_type(player_data$hundreds, "double")
  expect_type(player_data$fifties, "double")
  expect_type(player_data$ducks, "double")
  expect_type(player_data$fours, "double")
  expect_type(player_data$sixes, "double")
  expect_type(player_data$overs, "double")
  expect_type(player_data$maidens, "double")
  expect_type(player_data$wickets, "double")
  expect_type(player_data$economy, "double")
  expect_type(player_data$best_bowling_innings, "character") # Best performance often stored as a string like "5/30"
  expect_type(player_data$four_wickets, "double")
  expect_type(player_data$five_wickets, "double")
  expect_type(player_data$ten_wickets, "double")
  expect_type(player_data$dismissals, "double")
  expect_type(player_data$caught, "double")
  expect_type(player_data$caught_fielder, "double")
  expect_type(player_data$caught_behind, "double")
  expect_type(player_data$stumped, "double")
  expect_type(player_data$max_dismissals_innings, "double")
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

# Test that `cleaned_player_data` 'average' column is correctly calculated when applicable
test_that("cleaned_player_data 'average' column has reasonable values", {
  expect_true(all(player_data$average >= 0, na.rm = TRUE))
  expect_true(all(player_data$average <= player_data$runs, na.rm = TRUE))
})

# Test that `cleaned_player_data` contains no players with more centuries than matches
test_that("cleaned_player_data 'hundreds' does not exceed 'matches'", {
  expect_true(all(player_data$hundreds <= player_data$matches))
})

# Test that `cleaned_player_data` 'overs' is consistent with 'maidens'
test_that("cleaned_player_data 'overs' is greater than or equal to 'maidens'", {
  expect_true(all(player_data$overs >= player_data$maidens))
})

# Test that `cleaned_player_data` bowling statistics are consistent
test_that("cleaned_player_data bowling statistics are consistent", {
  expect_true(all(player_data$wickets >= 0))
  expect_true(all(player_data$four_wickets <= player_data$wickets, na.rm = TRUE))
  expect_true(all(player_data$five_wickets <= player_data$wickets, na.rm = TRUE))
  expect_true(all(player_data$ten_wickets <= player_data$wickets, na.rm = TRUE))
})

test_that("cleaned_player_data start year is earlier than or equal to end year", {
  expect_true(all(player_data$start <= player_data$end, na.rm = TRUE))
})

test_that("cleaned_player_data has consistent country values for each player", {
  player_country_check <- player_data %>%
    group_by(player) %>%
    summarise(country_consistent = n_distinct(country) == 1) %>%
    filter(!country_consistent)
  
  expect_equal(nrow(player_country_check), 0, info = "Some players have inconsistent country entries.")
})

test_that("cleaned_player_data high scores are within a reasonable range", {
  expect_true(all(player_data$high_score >= 0 & player_data$high_score <= 400, na.rm = TRUE))
})

test_that("cleaned_player_data dismissals column is reasonable", {
  expect_true(all(player_data$dismissals >= 0, na.rm = TRUE))
  expect_true(all(player_data$dismissals <= player_data$matches*10, na.rm = TRUE))
})

test_that("cleaned_player_data runs are non-negative", {
  expect_true(all(player_data$runs >= 0, na.rm = TRUE))
})
