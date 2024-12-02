#### Preamble ####
# Purpose: Cleans the raw cricket data
# Author: Muhammad Abdullah Motasim
# Date: 23 November 2024
# Contact: abdullah.motasim@mail.utoronto.ca
# License: MIT
# Pre-requisites: tidyverse, dplyr, and arrow libraries
# Any other information needed? Raw player_data, match_data, and combined bat/bowl/field data
# should be saved in the 01-raw_data folder

#### Workspace setup ####
library(tidyverse)
library(dplyr)
library(arrow)

#### Clean data ####
raw_player_data <- read_csv("data/01-raw_data/cricsheet_player_data.csv")
raw_match_data <- read_csv("data/01-raw_data/cricsheet_match_data.csv")
raw_meta_data <- read_csv("data/01-raw_data/player_meta_data.csv")
raw_bat_data <- read_csv("data/01-raw_data/combined_bat_data.csv")
raw_bowl_data <- read_csv("data/01-raw_data/combined_bowl_data.csv")
raw_field_data <- read_csv("data/01-raw_data/combined_field_data.csv")

raw_bat_data <- raw_bat_data %>%
  mutate(bat_innings = Innings, bat_runs = Runs) %>%
  select(-Innings, -Runs)

raw_bowl_data <- raw_bowl_data %>%
  mutate(bowl_innings = Innings, bowl_runs = Runs) %>%
  select(-Innings, -Runs)

raw_field_data <- raw_field_data %>%
  mutate(field_innings = Innings) %>%
  select(-Innings)

cleaned_match_data <-
  raw_match_data |>
  janitor::clean_names() |>
  select(team1,
         team2,
         date,
         winner,
         winner_wickets,
         winner_runs,
         toss_winner) |>
  mutate(date = as.character(date)) |>
  mutate(date = ymd(date)) |>
  mutate(winner = ifelse(is.na(winner), "Draw", winner))  # Replace NA with "Draw" in winner column

for (i in 1:nrow(cleaned_match_data)) {
  if (cleaned_match_data$team1[i] == "Pakistan" || cleaned_match_data$team2[i] == "Pakistan") {
    if(cleaned_match_data$team1[i] != "Pakistan"){
      temp <- cleaned_match_data$team1[i]
      cleaned_match_data$team1[i] <- "Pakistan"
      cleaned_match_data$team2[i] <- temp
    }
  }
}

# Filter match data for Pakistan
pakistan_matches <- cleaned_match_data %>%
  filter(team1 == "Pakistan")

cleaned_player_data <- bind_rows(
  raw_bat_data %>% mutate(dataset_type = "bat"),
  raw_bowl_data %>% mutate(dataset_type = "bowl"),
  raw_field_data %>% mutate(dataset_type = "field")
) %>%
  filter(Country == "Pakistan") %>%  # Filtering for Pakistani players
  group_by(Player) %>%
  summarise(
    # General player information
    Start = min(Start, na.rm = TRUE),
    # Take the earliest Start year
    End = max(End, na.rm = TRUE),
    # Take the latest End year
    Matches = first(Matches),
    # Assuming 'Matches' is consistent for each player
    
    # Batting stats
    bat_innings = sum(bat_innings[dataset_type == "bat"], na.rm = TRUE),
    # Sum bat_innings
    bowl_innings = sum(bowl_innings[dataset_type == "bowl"], na.rm = TRUE),
    # Sum bowl_innings
    field_innings = sum(field_innings[dataset_type == "field"], na.rm = TRUE),
    # Sum field_innings
    
    bat_runs = sum(bat_runs[dataset_type == "bat"], na.rm = TRUE),
    # Batting - Runs
    not_outs = sum(NotOuts[dataset_type == "bat"], na.rm = TRUE),
    # Batting - NotOuts
    bat_average = mean(Average[dataset_type == "bat"], na.rm = TRUE),
    # Batting - Average
    
    # Bowling stats
    bowl_runs = sum(bowl_runs[dataset_type == "bowl"], na.rm = TRUE),
    # Bowling - Runs
    Wickets = sum(Wickets[dataset_type == "bowl"], na.rm = TRUE),
    # Bowling - Wickets
    Economy = mean(Economy[dataset_type == "bowl"], na.rm = TRUE),
    # Bowling - Economy
    
    # Fielding stats
    Dismissals = sum(Dismissals[dataset_type == "field"], na.rm = TRUE),
    # Fielding - Dismissals
    
    .groups = "drop"  # Removes grouping after summarizing
  ) %>%
  mutate(
    bat_average = case_when(
      bat_innings - not_outs == 0 ~ NA_real_,
      # No outs, can't compute average
      is.infinite(bat_average) ~ bat_runs / (bat_innings - not_outs),
      # Calculate average if Inf
      TRUE ~ bat_average  # Else keep the calculated average
    )
  ) %>%
  janitor::clean_names()


#### Save data ####
write_parquet(cleaned_match_data,
              "data/02-analysis_data/cleaned_match_data.parquet")
write_parquet(pakistan_matches,
              "data/02-analysis_data/cleaned_pakistan_match_data.parquet")
write_parquet(cleaned_player_data,
              "data/02-analysis_data/cleaned_player_data.parquet")