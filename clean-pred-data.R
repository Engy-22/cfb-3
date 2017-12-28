
# set working directory
setwd(here::here())

# load packages
library(tidyverse)
library(magrittr)

# read data
raw_df <- read_tsv("data/massey-scheduled-raw.txt", col_names = FALSE) %>%
  separate(X1, c("date", "team1_name", "team1_points", "team2_name", "team2_points", "city"), 
           sep = c(10, 37, 40, 66, 69)) %>%
  mutate(game_id = 1:n()) %>%
  mutate(team1_name = str_replace(team1_name, "@", ""),
         team2_name = str_replace(team2_name, "@", ""),
         team1_name = str_trim(team1_name),
         team2_name = str_trim(team2_name),
         team1_points = as.numeric(team1_points),
         team2_points = as.numeric(team2_points)) %>%
  select(-city) %>%
  glimpse()

# make team 1 the offense
df1 <- raw_df %>%
  select(game_id,
         points = team1_points,
         offense = team1_name, 
         defense = team2_name) %>%
  glimpse()

# make team 2 the offence
df2 <- raw_df %>%
  select(game_id,
         points = team2_points,
         offense = team2_name, 
         defense = team1_name)

# load the rankings data
rankings_df <- read_csv("data/ratings.csv") %>%
  select(team = Team,
         mean = Mean) %>%
  glimpse()

# combine the team 1 offense and team 2 offense data
off_def_sched_df <- bind_rows(df1, df2) %>%
  left_join(rankings_df, by = c("offense" = "team")) %>%
  rename("offense_mean" = mean) %>%
  left_join(rankings_df, by = c("defense" = "team")) %>%
  rename("defense_mean" = mean) %>%
  na.omit() %>%  # drop teams without rankings
  glimpse() %>%
  write_csv("data/off-def-sched.csv")

playoff_df <- off_def_sched_df %>%
  filter(offense %in% c("Georgia", "Alabama", "Oklahoma", "Clemson")) %>%
  complete(offense, defense) %>%
  glimpse()

