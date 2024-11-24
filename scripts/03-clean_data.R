#### Preamble ####
# Purpose: Cleans the raw cricket data
# Author: Muhammad Abdullah Motasim
# Date: 23 November 2024
# Contact: abdullah.motasim@mail.utoronto.ca
# License: MIT
# Pre-requisites: tidyverse and dplyr libraries
# Any other information needed? None

#### Workspace setup ####
library(tidyverse)
library(dplyr)

#### Clean data ####
raw_player_data <- read_csv("data/01-raw_data/cricsheet_player_data.csv")
raw_match_data <- read_csv("data/01-raw_data/cricsheet_match_data.csv")
raw_meta_data <- read_csv("data/01-raw_data/player_meta_data.csv")
raw_bat_data <- read_csv("data/01-raw_data/combined_bat_data.csv")
raw_bowl_data <- read_csv("data/01-raw_data/combined_bowl_data.csv")
raw_field_data <- read_csv("data/01-raw_data/combined_field_data.csv")

raw_bat_data <- raw_bat_data %>%
  mutate(bat_innings = Innings) %>%
  select(-Innings)

raw_bowl_data <- raw_bowl_data %>%
  mutate(bowl_innings = Innings) %>%
  select(-Innings)

raw_field_data <- raw_field_data %>%
  mutate(field_innings = Innings) %>%
  select(-Innings)

cleaned_match_data <-
  raw_match_data |>
  janitor::clean_names() |>
  select(team1, team2, date, winner, winner_wickets, winner_runs)

cleaned_player_data <- bind_rows(raw_bat_data, raw_bowl_data, raw_field_data) %>%
  group_by(Player) %>%
  summarise(
    Country = first(Country), # Assuming 'Country' remains consistent for each player
    Start = min(Start, na.rm = TRUE), # Take the earliest Start year
    End = max(End, na.rm = TRUE), # Take the latest End year
    Matches = first(Matches),
    bat_innings = sum(bat_innings, na.rm = TRUE), # Sum bat_innings
    bowl_innings = sum(bowl_innings, na.rm = TRUE), # Sum bowl_innings
    field_innings = sum(field_innings, na.rm = TRUE), # Sum field_innings
    
    #BAT
    NotOuts = sum(NotOuts, na.rm = TRUE),
    Runs = sum(Runs, na.rm = TRUE),
    HighScore = ifelse(all(is.na(HighScore)), NA, max(HighScore, na.rm = TRUE)), # Handle NA values
    HighScoreNotOut = any(HighScoreNotOut == TRUE, na.rm = TRUE), # TRUE if any instance is TRUE
    Average = mean(Average, na.rm = TRUE), # Average across all formats
    BallsFaced = sum(BallsFaced, na.rm = TRUE),
    StrikeRate = mean(StrikeRate, na.rm = TRUE), # Average strike rate
    Hundreds = sum(Hundreds, na.rm = TRUE),
    Fifties = sum(Fifties, na.rm = TRUE),
    Ducks = sum(Ducks, na.rm = TRUE),
    Fours = sum(Fours, na.rm=TRUE),
    Sixes = sum(Sixes, na.rm=TRUE),
    
    #BOWL
    Overs = sum(Overs, na.rm = TRUE),
    Maidens = sum(Maidens, na.rm = TRUE),
    Runs = sum(Runs, na.rm = TRUE),
    Wickets = sum(Wickets, na.rm = TRUE),
    Average = mean(Average, na.rm = TRUE), # Average across all formats
    Economy = mean(Economy, na.rm = TRUE), # Average economy
    StrikeRate = mean(StrikeRate, na.rm = TRUE), # Average strike rate
    BestBowlingInnings = ifelse(all(is.na(BestBowlingInnings)), NA, max(BestBowlingInnings, na.rm = TRUE)), # Best bowling performance
    FourWickets = sum(FourWickets, na.rm = TRUE),
    FiveWickets = sum(FiveWickets, na.rm = TRUE),
    TenWickets = sum(TenWickets, na.rm = TRUE),
    
    #FIELD
    Dismissals = sum(Dismissals, na.rm = TRUE),
    Caught = sum(Caught, na.rm = TRUE),
    CaughtFielder = sum(CaughtFielder, na.rm = TRUE),
    CaughtBehind = sum(CaughtBehind, na.rm = TRUE),
    Stumped = sum(Stumped, na.rm = TRUE),
    MaxDismissalsInnings = ifelse(all(is.na(MaxDismissalsInnings)), NA, max(MaxDismissalsInnings, na.rm = TRUE)),
    .groups = "drop" # Removes grouping after summarising
  ) |>
  janitor::clean_names()

#### Save data ####
write_csv(cleaned_match_data, "data/02-analysis_data/cleaned_match_data.csv")
write_csv(cleaned_player_data, "data/02-analysis_data/cleaned_player_data.csv")
