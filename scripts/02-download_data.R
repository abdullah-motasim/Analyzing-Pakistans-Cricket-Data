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
library(dplyr)

#### Download data ####

# Fetch cricket player data from Cricinfo for various skill categories (batting, bowling, fielding)
# for different formats (Test, ODI, T20). For each category, we fetch data for male players in career statistics.
cricinfo_data1 <- fetch_cricinfo("test", "men", "batting", "career", country = NULL)
cricinfo_data2 <- fetch_cricinfo("test", "men", "bowling", "career", country = NULL)
cricinfo_data3 <- fetch_cricinfo("test", "men", "fielding", "career", country = NULL)
cricinfo_data4 <- fetch_cricinfo("odi", "men", "batting", "career", country = NULL)
cricinfo_data5 <- fetch_cricinfo("odi", "men", "bowling", "career", country = NULL)
cricinfo_data6 <- fetch_cricinfo("odi", "men", "fielding", "career", country = NULL)
cricinfo_data7 <- fetch_cricinfo("t20", "men", "batting", "career", country = NULL)
cricinfo_data8 <- fetch_cricinfo("t20", "men", "bowling", "career", country = NULL)
cricinfo_data9 <- fetch_cricinfo("t20", "men", "fielding", "career", country = NULL)

# Data cleaning for the batting datasets
cricinfo_data1 <- cricinfo_data1 %>%
  mutate(Player = str_trim(Player)) %>% # Remove leading/trailing spaces
  mutate(Player = str_to_title(Player)) # Ensure consistent capitalization
cricinfo_data4 <- cricinfo_data4 %>%
  mutate(Player = str_trim(Player)) %>%
  mutate(Player = str_to_title(Player))
cricinfo_data7 <- cricinfo_data7 %>%
  mutate(Player = str_trim(Player)) %>%
  mutate(Player = str_to_title(Player))

# Remove duplicates from the batting datasets
cricinfo_data1 <- cricinfo_data1 %>% distinct()
cricinfo_data4 <- cricinfo_data4 %>% distinct()
cricinfo_data7 <- cricinfo_data7 %>% distinct()

# Combine the datasets for batting data across all formats (Test, ODI, T20)
combined_bat_data <- bind_rows(cricinfo_data1, cricinfo_data4, cricinfo_data7) %>%
  group_by(Player) %>%
  summarise(
    Country = first(Country), # Assuming 'Country' remains consistent for each player
    Start = min(Start, na.rm = TRUE), # Take the earliest Start year for each player
    End = max(End, na.rm = TRUE), # Take the latest End year for each player
    Matches = sum(Matches, na.rm = TRUE), # Sum total number of matches
    Innings = sum(Innings, na.rm = TRUE), # Sum total number of innings
    NotOuts = sum(NotOuts, na.rm = TRUE), # Sum total number of not outs
    Runs = sum(Runs, na.rm = TRUE), # Sum total runs scored
    HighScore = ifelse(all(is.na(HighScore)), NA, max(HighScore, na.rm = TRUE)), # Max high score across all formats
    HighScoreNotOut = any(HighScoreNotOut == TRUE, na.rm = TRUE), # If any HighScoreNotOut value is TRUE
    Average = mean(Average, na.rm = TRUE), # Mean batting average across all formats
    BallsFaced = sum(BallsFaced, na.rm = TRUE), # Total balls faced
    StrikeRate = mean(StrikeRate, na.rm = TRUE), # Average strike rate
    Hundreds = sum(Hundreds, na.rm = TRUE), # Total number of hundreds
    Fifties = sum(Fifties, na.rm = TRUE), # Total number of fifties
    Ducks = sum(Ducks, na.rm = TRUE), # Total number of ducks
    Fours = sum(Fours, na.rm = TRUE), # Total number of fours
    Sixes = sum(Sixes, na.rm = TRUE), # Total number of sixes
    .groups = "drop" # Drop grouping after summarization
  )

# Data cleaning for the bowling datasets
cricinfo_data2 <- cricinfo_data2 %>%
  mutate(Player = str_trim(Player)) %>% # Remove leading/trailing spaces
  mutate(Player = str_to_title(Player)) # Ensure consistent capitalization
cricinfo_data5 <- cricinfo_data5 %>%
  mutate(Player = str_trim(Player)) %>%
  mutate(Player = str_to_title(Player))
cricinfo_data8 <- cricinfo_data8 %>%
  mutate(Player = str_trim(Player)) %>%
  mutate(Player = str_to_title(Player))

# Remove duplicates from the bowling datasets
cricinfo_data2 <- cricinfo_data2 %>% distinct()
cricinfo_data5 <- cricinfo_data5 %>% distinct()
cricinfo_data8 <- cricinfo_data8 %>% distinct()

# Combine the datasets for bowling data across all formats (Test, ODI, T20)
combined_bowl_data <- bind_rows(cricinfo_data2, cricinfo_data5, cricinfo_data8) %>%
  group_by(Player) %>%
  summarise(
    Country = first(Country), # Assuming 'Country' remains consistent for each player
    Start = min(Start, na.rm = TRUE), # Take the earliest Start year for each player
    End = max(End, na.rm = TRUE), # Take the latest End year for each player
    Matches = sum(Matches, na.rm = TRUE), # Sum total number of matches
    Innings = sum(Innings, na.rm = TRUE), # Sum total number of innings
    Overs = sum(Overs, na.rm = TRUE), # Sum total overs bowled
    Maidens = sum(Maidens, na.rm = TRUE), # Sum total maidens bowled
    Runs = sum(Runs, na.rm = TRUE), # Sum total runs conceded
    Wickets = sum(Wickets, na.rm = TRUE), # Sum total wickets taken
    Average = mean(Average, na.rm = TRUE), # Mean bowling average
    Economy = mean(Economy, na.rm = TRUE), # Mean economy rate
    StrikeRate = mean(StrikeRate, na.rm = TRUE), # Mean bowling strike rate
    BestBowlingInnings = ifelse(all(is.na(BestBowlingInnings)), NA, max(BestBowlingInnings, na.rm = TRUE)), # Best bowling performance
    FourWickets = sum(FourWickets, na.rm = TRUE), # Total number of 4-wicket hauls
    FiveWickets = sum(FiveWickets, na.rm = TRUE), # Total number of 5-wicket hauls
    TenWickets = sum(TenWickets, na.rm = TRUE), # Total number of 10-wicket hauls
    .groups = "drop" # Drop grouping after summarization
  )

# Data cleaning for the fielding datasets
cricinfo_data3 <- cricinfo_data3 %>%
  mutate(Player = str_trim(Player)) %>% # Remove leading/trailing spaces
  mutate(Player = str_to_title(Player)) # Ensure consistent capitalization
cricinfo_data6 <- cricinfo_data6 %>%
  mutate(Player = str_trim(Player)) %>%
  mutate(Player = str_to_title(Player))
cricinfo_data9 <- cricinfo_data9 %>%
  mutate(Player = str_trim(Player)) %>%
  mutate(Player = str_to_title(Player))

# Remove duplicates from the fielding datasets
cricinfo_data3 <- cricinfo_data3 %>% distinct()
cricinfo_data6 <- cricinfo_data6 %>% distinct()
cricinfo_data9 <- cricinfo_data9 %>% distinct()

# Combine the datasets for fielding data across all formats (Test, ODI, T20)
combined_field_data <- bind_rows(cricinfo_data3, cricinfo_data6, cricinfo_data9) %>%
  group_by(Player) %>%
  summarise(
    Country = first(Country), # Assuming 'Country' remains consistent for each player
    Start = min(Start, na.rm = TRUE), # Take the earliest Start year for each player
    End = max(End, na.rm = TRUE), # Take the latest End year for each player
    Matches = sum(Matches, na.rm = TRUE), # Sum total number of matches
    Innings = sum(Innings, na.rm = TRUE), # Sum total number of innings
    Dismissals = sum(Dismissals, na.rm = TRUE), # Total number of dismissals
    Caught = sum(Caught, na.rm = TRUE), # Total number of catches
    CaughtFielder = sum(CaughtFielder, na.rm = TRUE), # Caught by fielders
    CaughtBehind = sum(CaughtBehind, na.rm = TRUE), # Caught behind dismissals
    Stumped = sum(Stumped, na.rm = TRUE), # Total number of stumpings
    MaxDismissalsInnings = ifelse(all(is.na(MaxDismissalsInnings)), NA, max(MaxDismissalsInnings, na.rm = TRUE)), # Maximum dismissals in an innings
    .groups = "drop" # Drop grouping after summarization
  )

# Fetch additional player and match data from Cricsheet
cricsheet_data1 <- fetch_cricsheet("player", "male", "all")
cricsheet_data2 <- fetch_cricsheet("match", "male", "all")

# Fetch Cricsheet data for Test matches
fetch_cricsheet(
  type = c("bbb", "match", "player"),
  gender = c("female", "male"),
  competition = "tests"
)

# Fetch player metadata
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

# Save the combined datasets for batting, bowling, and fielding
write_csv(combined_bat_data, "data/01-raw_data/combined_bat_data.csv")
write_csv(combined_bowl_data, "data/01-raw_data/combined_bowl_data.csv")
write_csv(combined_field_data, "data/01-raw_data/combined_field_data.csv")

# Save Cricsheet data for players and matches
write_csv(cricsheet_data1, "data/01-raw_data/cricsheet_player_data.csv")
write_csv(cricsheet_data2, "data/01-raw_data/cricsheet_match_data.csv")
write_csv(player_meta_data, "data/01-raw_data/player_meta_data.csv")
