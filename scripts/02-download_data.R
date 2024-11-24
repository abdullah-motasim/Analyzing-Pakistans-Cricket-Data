#### Preamble ####
# Purpose: Downloads and saves the data from cricketdata package
# Author: Muhammad Abdullah Motasim
# Date: 22 November 2024
# Contact: abdullah.motasim@mail.utoronto.ca
# License: MIT
# Pre-requisites: cricketdata, tidyverse, and dplyr libraries
# Any other information needed? None


#### Workspace setup ####
library(cricketdata)
library(tidyverse)
library(dplyr) # For data wrangling

#### Download data ####

cricinfo_data1 <- fetch_cricinfo("test", "men", "batting", "career", country = NULL)
cricinfo_data2 <- fetch_cricinfo("test", "men", "bowling", "career", country = NULL)
cricinfo_data3 <- fetch_cricinfo("test", "men", "fielding", "career", country = NULL)
cricinfo_data4 <- fetch_cricinfo("odi", "men", "batting", "career", country = NULL)
cricinfo_data5 <- fetch_cricinfo("odi", "men", "bowling", "career", country = NULL)
cricinfo_data6 <- fetch_cricinfo("odi", "men", "fielding", "career", country = NULL)
cricinfo_data7 <- fetch_cricinfo("t20", "men", "batting", "career", country = NULL)
cricinfo_data8 <- fetch_cricinfo("t20", "men", "bowling", "career", country = NULL)
cricinfo_data9 <- fetch_cricinfo("t20", "men", "fielding", "career", country = NULL)

# Combine datasets by skill type (batting, bowling, fielding)
combined_bat_data <- bind_rows(cricinfo_data1, cricinfo_data5, cricinfo_data7) %>%
  group_by(Player) %>%
  summarise(
    Country = first(Country), # Assuming 'Country' remains consistent for each player
    Start = min(Start, na.rm = TRUE), # Take the earliest Start year
    End = max(End, na.rm = TRUE), # Take the latest End year
    Matches = sum(Matches, na.rm = TRUE),
    Innings = sum(Innings, na.rm = TRUE),
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
    .groups = "drop" # Removes grouping after summarising
  )


combined_bowl_data <- bind_rows(cricinfo_data2, cricinfo_data5, cricinfo_data8) %>%
  group_by(Player) %>%
  summarise(
    Country = first(Country), # Assuming 'Country' remains consistent for each player
    Start = min(Start, na.rm = TRUE), # Earliest Start year
    End = max(End, na.rm = TRUE), # Latest End year
    Matches = sum(Matches, na.rm = TRUE),
    Innings = sum(Innings, na.rm = TRUE),
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
    .groups = "drop" # Removes grouping after summarising
  )


combined_field_data <- bind_rows(cricinfo_data3, cricinfo_data6, cricinfo_data9) %>%
  group_by(Player) %>%
  summarise(
    Country = first(Country), # Assuming 'Country' remains consistent for each player
    Start = min(Start, na.rm = TRUE), # Earliest Start year
    End = max(End, na.rm = TRUE), # Latest End year
    Matches = sum(Matches, na.rm = TRUE),
    Innings = sum(Innings, na.rm = TRUE),
    Dismissals = sum(Dismissals, na.rm = TRUE),
    Caught = sum(Caught, na.rm = TRUE),
    CaughtFielder = sum(CaughtFielder, na.rm = TRUE),
    CaughtBehind = sum(CaughtBehind, na.rm = TRUE),
    Stumped = sum(Stumped, na.rm = TRUE),
    MaxDismissalsInnings = max(MaxDismissalsInnings, na.rm = TRUE),
    .groups = "drop" # Removes grouping after summarising
  )

cricsheet_data1 <- fetch_cricsheet("player", "male", "all")
cricsheet_data2 <- fetch_cricsheet("match", "male", "all")

fetch_cricsheet(
  type = c("bbb", "match", "player"),
  gender = c("female", "male"),
  competition = "tests"
)


player_meta_data <- update_player_meta(start_again = FALSE)

#### Save data ####
write_csv(cricinfo_data1, "data/01-raw_data/fetch_cricinfo_data/test_men_batting_career_all.csv")
write_csv(cricinfo_data2, "data/01-raw_data/fetch_cricinfo_data/test_men_bowling_career_all.csv")
write_csv(cricinfo_data3, "data/01-raw_data/fetch_cricinfo_data/test_men_fielding_career_all.csv")
write_csv(cricinfo_data4, "data/01-raw_data/fetch_cricinfo_data/odi_men_batting_career_all.csv")
write_csv(cricinfo_data5, "data/01-raw_data/fetch_cricinfo_data/odi_men_bowling_career_all.csv")
write_csv(cricinfo_data6, "data/01-raw_data/fetch_cricinfo_data/odi_men_fielding_career_all.csv")
write_csv(cricinfo_data7, "data/01-raw_data/fetch_cricinfo_data/t20_men_batting_career_all.csv")
write_csv(cricinfo_data8, "data/01-raw_data/fetch_cricinfo_data/t20_men_bowling_career_all.csv")
write_csv(cricinfo_data9, "data/01-raw_data/fetch_cricinfo_data/t20_men_fielding_career_all.csv")
          
write_csv(combined_bat_data, "data/01-raw_data/combined_bat_data.csv")
write_csv(combined_bowl_data, "data/01-raw_data/combined_bowl_data.csv")
write_csv(combined_field_data, "data/01-raw_data/combined_field_data.csv")

write_csv(cricsheet_data1, "data/01-raw_data/cricsheet_player_data.csv")
write_csv(cricsheet_data2, "data/01-raw_data/cricsheet_match_data.csv")
write_csv(player_meta_data, "data/01-raw_data/player_meta_data.csv")
