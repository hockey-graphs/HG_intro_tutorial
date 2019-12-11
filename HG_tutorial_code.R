# Intalling packages and getting set up ----
# This is how you install packages
# For this tutorial, we'll only use this package

install.packages("tidyverse")

# Once packages are installed on your computer, you don't have to do it again
# But you do have to load the package like below, for every R session

library(tidyverse)

# Load in our data from Github
# (At the bottom I'll show you how I scraped this data)
# This is four Philadelphia Flyers games from November 

PHI_tutorial_data <- 
  read_csv("https://github.com/hockey-graphs/HG_intro_tutorial/blob/master/PHI_tutorial_data.csv?raw=true")

# If you're using a file that's already on your computer (in the same folder as your project),
# you can use this code instead
# I have it commented out for now

# PHI_tutorial_data <- 
#   read_csv("PHI_tutorial_data.csv")

# Exploring your data ----

# Create a new data frame called goals and filter down to ONLY
# observations with event_type of GOAL
# Note the double equal sign, required when testing equality!

goals <- PHI_tutorial_data %>%
  filter(event_type == "GOAL")

# Can add more conditions with and (&)

goals_5v5 <- PHI_tutorial_data %>%
  filter(event_type == "GOAL" &
           game_strength_state == "5v5")

# Can add more conditions with or (|)

goals_special_teams <- PHI_tutorial_data %>%
  filter(event_type == "GOAL" &
           (game_strength_state == "5v4" |
              game_strength_state == "4v5"))

# When you have multiple options for a condition, it becomes easier to use %in% instead
# The c() notation just means that it's a list

goals_5v5_ST <- PHI_tutorial_data %>%
  filter(event_type == "GOAL" &
           game_strength_state %in% c("5v5", "5v4", "4v5"))

# The select() function can be used to drop and/or keep variables
# This will keep only the six selected variables

goals_small <- goals %>%
  select(game_id, game_date, event_type, 
         event_detail, event_team, event_player_1)

# This will remove the event_description variable

goals_drop <- goals %>%
  select(-c(event_description))

# You can also use select() to reorder variables
# If you wanted to pull home_score and away_score to the beginning

goals <- goals %>%
  select(home_score, away_score, everything())

# Mutate will create new variables
# Creating a new data frame with a goal variable
# ifelse() notation uses condition, value if true, value if false
# Note the single equal sign because we're NOT testing for equality

goal_variable <- PHI_tutorial_data %>%
  mutate(goal = ifelse(event_type == "GOAL", 1, 0))

# Double check that our variable creation worked with sum
# Now that goal is a 0/1 variable, you can use sum
# The result will show up in the console below

sum(goal_variable$goal)

# Double check that our variable creation worked with count
# This will show the frequencies of each unique value for the event_type variable
# The result will show up in the console below

count(goal_variable, event_type)

# Use group_by and summarize to find total goals per game

goals_by_game <- goal_variable %>%
  group_by(game_id) %>%
  summarize(total_goals = sum(goal))

# Add event_team so we have total goals per game per team
# But if you look at the resulting data frame, there are NA values

goals_by_game_team <- goal_variable %>%
  group_by(game_id, event_team) %>%
  summarize(goals = sum(goal))

# Let's try that again and remove the NAs first
# ! means "not" in R language
# is.na() identifies the null values

goals_by_game_team <- goal_variable %>%
  filter(!is.na(event_team)) %>%
  group_by(game_id, event_team) %>%
  summarize(goals = sum(goal))

# Arrange by number of goals

goals_by_game_team <- goals_by_game_team %>%
  arrange(desc(goals))

# Making a graph----

# Make a bar chart!

ggplot(data = PHI_tutorial_data) + 
  geom_bar(aes(x = event_zone))

# Make a bar chart with some color!

ggplot(data = PHI_tutorial_data) + 
  geom_bar(aes(x = event_zone, fill = event_zone))

# Add a label to the y-axis

ggplot(data = PHI_tutorial_data) + 
  geom_bar(aes(x = event_zone, fill = event_zone)) +
  labs(y = "Number of Events")



# Exercises----
# Try these first for yourself in the space provided before scrolling to the solution

# 1. What was the 5v5 shooting percentage for each team in each game?






# Create two variables, one for SOG (remember it includes goals as well as shots) and one for goals
# Filter down to 5v5 play only, group by game and team, then summarize
# Finish by creating a new shooting percentage variable

sh_perc <- PHI_tutorial_data %>%
  mutate(SOG = ifelse(event_type %in% c("SHOT", "GOAL"), 1, 0),
         goal = ifelse(event_type == "GOAL", 1, 0)) %>%
  filter(game_strength_state == "5v5" & !is.na(event_team)) %>%
  group_by(game_id, event_team) %>%
  summarize(SOG = sum(SOG),
            goal = sum(goal)) %>%
  mutate(sh_perc = goal / SOG)

# 2. Which team won each game? How many points did PHI get in each game?






# Group by game (and include home_team and away_team in the function so they're included in the data frame)
# Find the maximum home_score, away_score, and game_period (so we know whether the game went to OT and/or SO)
# Use an ifelse statement to find the winning team based on the scores
# Create a variable that gives 2 points if PHI won the game and 1 point if the game went to OT/SO

PHI_results <- PHI_tutorial_data %>%
  group_by(game_id, home_team, away_team) %>%
  summarize(max_home_score = max(home_score),
            max_away_score = max(away_score),
            max_period = max(game_period)) %>%
  mutate(winning_team = ifelse(max_home_score > max_away_score, home_team, away_team),
         PHI_points = ifelse(winning_team == "PHI", 2,
                             ifelse(max_period > 3, 1, 0)))

# 3. Which player generated the most shot attempts among all of these games?






indiv_corsi <- PHI_tutorial_data %>%
  mutate(shot_attempts = ifelse(event_type %in% c("SHOT", "BLOCK", "MISS", "GOAL"), 1, 0)) %>%
  filter(!is.na(event_player_1)) %>%
  group_by(event_player_1, event_team) %>%
  summarize(shot_attempts = sum(shot_attempts)) %>%
  arrange(desc(shot_attempts))

# 4. Create a bar chart showing the top 5 players with the most shot attempts






# The top_n() function will show the top 5 in shot attempts only (though we have to ungroup first)

indiv_corsi_top5 <- indiv_corsi %>%
  ungroup() %>%
  top_n(5, shot_attempts)

# Create a horizontal bar chart by using coord_flip
# The stat = "identity" part is necessary if you're creating a bar chart that has both x and y
# variables and *isn't* just frequency, like the one in our tutorial above

ggplot(data = indiv_corsi_top5) + 
  geom_bar(aes(x = event_player_1, y = shot_attempts), stat = "identity") +
  labs(y = "Number of Shot Attempts", x = "Player") +
  coord_flip()

# If you want to sort the player names by shot attempts, do this

ggplot(data = indiv_corsi_top5) + 
  geom_bar(aes(x = reorder(event_player_1, shot_attempts), y = shot_attempts), stat = "identity") +
  labs(y = "Number of Shot Attempts", x = "Player") +
  coord_flip()

# 5. Which players drew the most penalties?






# By examining the data, you can see that the player who DREW the penalty is event_player_2
# We could also use group_by() and summarize() but it's often easier to use count() if it's a simple request
# The sort = TRUE argument will sort in descending order

penl <- PHI_tutorial_data %>%
  filter(event_type == "PENL" & !is.na(event_player_2)) %>%
  count(event_player_2, sort = TRUE)

# 6. What was the faceoff win percentage for PHI in each game?






# Filter down to just faceoffs and create a new variable for a PHI faceoff win
# Group by game and summarize the number of faceoff wins as well as
# the number of total faceoffs (which is just the total number of rows in the data frame)
# And then calculate the win percentage

faceoffs <- PHI_tutorial_data %>%
  filter(event_type == "FAC") %>%
  mutate(PHI_FO_win = ifelse(event_team == "PHI", 1, 0)) %>%
  group_by(game_id) %>%
  summarize(FO_wins = sum(PHI_FO_win),
            FO_total = n()) %>%
  mutate(FO_win_perc = FO_wins / FO_total)

# How to scrape game data with the EW scraper----

# Load the necessary packages and source the scraper (this will load all the necessary functions)

library(RCurl)
library(xml2)
library(rvest)
library(jsonlite) 
library(foreach)
library(lubridate)
library(tidyverse)
source('https://raw.githubusercontent.com/evolvingwild/evolving-hockey/master/EH_scrape_functions.R')

# Create a vector with the game ids used in this data

games_vec <- c("2019020336","2019020349","2019020367","2019020384")

# Scrape games

pbp_scrape <- sc.scrape_pbp(games = games_vec)

# Save the data to our data frame

PHI_tutorial_data <- pbp_scrape$pbp_base

# This will save the csv to your computer (in the same directory as your project)

write_excel_csv(PHI_tutorial_data, "PHI_tutorial_data.csv")
