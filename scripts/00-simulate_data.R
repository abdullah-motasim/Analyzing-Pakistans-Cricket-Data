#### Preamble ####
# Purpose: Simulates a dataset of cricket player career statistics and games
# Author: Muhammad Abdullah Motasim
# Date: 22 November 2024
# Contact: abdullah.motasim@mail.utoronto.ca
# License: MIT
# Pre-requisites: tibble and readr libraries
# Any other information needed? None


#### Workspace setup ####
library(tibble)
library(readr)
set.seed(853)


#### Simulate data ####
for (i in 1:150) {
  players[i] <- paste0("Player ", i)
}


# Countries
countries <- c("India", "Australia", "England", "Pakistan", "South Africa", "Sri Lanka", "New Zealand", "West Indies", "Bangladesh", "Zimbabwe", "Afghanistan", "Ireland")

# Simulated cricket data
simulated_player_data <- tibble(
  Player = players,
  Country = sample(
    countries,
    size = 150,
    replace = TRUE,
    prob = c(0.2, 0.15, 0.15, 0.1, 0.1, 0.1, 0.1, 0.05, 0.025, 0.025, 0.02, 0.02) # Rough country distribution
  ),
  Start = sample(1970:2023, size = 150, replace = TRUE), # Random start years
  End = sample(1985:2023, size = 150, replace = TRUE), # Random end years
  Matches = sample(1:500, size = 150, replace = TRUE), # Number of matches played
  Innings = sample(1:500, size = 150, replace = TRUE), # Number of innings
  NotOuts = sample(0:50, size = 150, replace = TRUE), # Number of not-outs
  BattingRunsScored = sample(0:20000, size = 150, replace = TRUE), # Total runs scored
  HighScore = sample(50:400, size = 150, replace = TRUE), # Highest individual score
  Average = round(runif(150, 20, 60), 2), # Batting average
  BallsFaced = sample(1000:30000, size = 150, replace = TRUE), # Balls faced
  StrikeRate = round(runif(150, 50, 150), 2), # Strike rate
  Hundreds = sample(0:100, size = 150, replace = TRUE), # Number of centuries
  Fifties = sample(0:200, size = 150, replace = TRUE), # Number of half-centuries
  Ducks = sample(0:50, size = 150, replace = TRUE), # Number of ducks (zero runs in an inning)
  FourWickets = sample(0:20, size = 150, replace = TRUE), # Number of 4-wicket hauls
  FiveWickets = sample(0:20, size = 150, replace = TRUE), # Number of 5-wicket hauls
  Maidens = sample(0:200, size = 150, replace = TRUE), # Number of maiden overs
  BowlingRunsScored = sample(0:2000, size=150, replace = TRUE),
  Wickets = sample(0:200, size=150, replace = TRUE),
  Caught = sample(0:200, size=150, replace = TRUE),
  Stumped = sample(0:300, size=150, replace = TRUE)
)

# Ensure Start is earlier than End
simulated_player_data <- simulated_player_data %>%
  mutate(End = pmax(End, Start))

simulated_player_data <- simulated_player_data %>%
  mutate(BattingRunsScored = pmax(BattingRunsScored, HighScore))



# Save the simulated dataset
write_csv(simulated_player_data, "data/00-simulated_data/simulated_player_data.csv")
