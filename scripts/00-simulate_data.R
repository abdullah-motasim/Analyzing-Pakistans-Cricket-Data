#### Preamble ####
# Purpose: Simulates a dataset of cricket player career statistics and games
# Author: Muhammad Abdullah Motasim
# Date: 22 November 2024
# Contact: abdullah.motasim@mail.utoronto.ca
# License: MIT
# Pre-requisites: dplyr arrow, and tibble libraries
# Any other information needed? None


#### Workspace setup ####
library(tibble)
library(dplyr)
library(arrow)
set.seed(853)


#### Simulate data ####
players <- character(150)
for (i in 1:150) {
  players[i] <- paste0("Player ", i)
}


# Countries
countries <- c(
  "India",
  "Australia",
  "England",
  "Pakistan",
  "South Africa",
  "Sri Lanka",
  "New Zealand",
  "West Indies",
  "Bangladesh",
  "Zimbabwe",
  "Afghanistan",
  "Ireland"
)

# Simulated cricket data
simulated_player_data <- tibble(
  Player = players,
  Country = sample(
    countries,
    size = 150,
    replace = TRUE,
    prob = c(0.2, 0.15, 0.15, 0.1, 0.1, 0.1, 0.1, 0.05, 0.025, 0.025, 0.02, 0.02) # Rough country distribution
  ),
  Start = sample(1970:2023, size = 150, replace = TRUE),
  # Random start years
  End = sample(1985:2023, size = 150, replace = TRUE),
  # Random end years
  Matches = sample(1:500, size = 150, replace = TRUE),
  # Number of matches played
  Bat_innings = abs(round(rnorm(
    150,
    mean = 50, sd = 50
  ))),
  # Number of innings
  Bowl_innings = abs(round(rnorm(150, mean = 50, sd = 50))),
  # Number of not-outs
  BattingRunsScored = abs(round(rnorm(
    150,
    mean = 500, sd = 200
  ))),
  # Total runs scored
  HighScore = sample(50:150, size = 150, replace = TRUE),
  # Highest individual score
  Average = abs(round(runif(150, 20, 60), 2)),
  # Batting average
  BallsFaced = sample(1000:30000, size = 150, replace = TRUE),
  # Balls faced
  StrikeRate = abs(round(runif(150, 50, 150), 2)),
  # Strike rate
  Hundreds = abs(round(rnorm(
    150,
    mean = 5, sd = 10
  ))),
  # Number of centuries
  Fifties = abs(round(rnorm(
    150,
    mean = 20, sd = 20
  ))),
  # Number of half-centuries
  BowlingRunsScored = sample(0:2000, size = 150, replace = TRUE),
  # Number of bowling runs scored
  Wickets = abs(round(rnorm(
    150,
    mean = 50, sd = 50
  ))),
  # Number of wickets
  Caught = abs(round(rnorm(
    150,
    mean = 50, sd = 50
  ))),
  # Number of batters caught
  Stumped = abs(round(rnorm(
    150,
    mean = 50, sd = 50
  )))
  # Number of players stumped
)

simulated_player_data <- simulated_player_data %>%
  mutate(End = pmax(End, Start)) %>% # Ensure Start is earlier than End
  mutate(BattingRunsScored = pmax(BattingRunsScored, HighScore)) %>% # Ensure runs scored is higher than highscore
  mutate(
    # Ensure Matches is consistent with Bat_innings and Bowl_innings
    Bat_innings = pmin(Bat_innings, Matches),
    Bowl_innings = pmin(Bowl_innings, Matches)
  ) %>%
  mutate(Hundreds = pmin(Hundreds, Fifties)) # Ensure consistency of Hundreds and Fifties


# Simulated match data
num_matches <- 100 # Number of matches to simulate

simulated_match_data <- tibble(
  match_id = 1:num_matches,

  # Sample team1 from countries
  team1 = sample(countries, size = num_matches, replace = TRUE),

  # Ensure team2 is different from team1 by using setdiff
  team2 = purrr::map_chr(team1, ~ sample(setdiff(countries, .x), 1)),
  date = sample(seq(as.Date("2000-01-01"), as.Date("2023-12-31"), by = "days"), size = num_matches, replace = TRUE),
  toss_winner = sample(c("team1", "team2"), size = num_matches, replace = TRUE),

  # Generate runs for both teams
  team1_runs = sample(150:400, size = num_matches, replace = TRUE),
  team2_runs = sample(150:400, size = num_matches, replace = TRUE),

  # Generate wickets
  team1_wickets = sample(0:10, size = num_matches, replace = TRUE),
  team2_wickets = sample(0:10, size = num_matches, replace = TRUE)
) %>%
  mutate(
    # Determine winner based on runs
    winner = ifelse(team1_runs > team2_runs, "team1", "team2"),

    # Adjust wickets based on the winner
    team1_wickets = ifelse(winner == "team1" & team2_wickets == 10, team2_wickets, team1_wickets),
    team2_wickets = ifelse(winner == "team2" & team1_wickets == 10, team1_wickets, team2_wickets)
  )

# Save the simulated dataset
write_parquet(
  simulated_player_data,
  "data/00-simulated_data/simulated_player_data.parquet"
)

write_parquet(
  simulated_match_data,
  "data/00-simulated_data/simulated_match_data.parquet"
)
