
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

# make team 2 the offense
df2 <- raw_df %>%
  select(game_id,
         points = team2_points,
         offense = team2_name, 
         defense = team1_name)

# combine the team 1 offense and team 2 offense data
ranks_df <- read_csv("data/ratings.csv") %>%
  select(team = Team, 
         mean = Mean, 
         median = Median, 
         trimmed = Trimmed) %>%
  glimpse()

points_df <- bind_rows(df1, df2) %>%
  left_join(ranks_df, by = c("offense" = "team")) %>%
  rename("offense_mean_rank" = mean,
         "offense_median_rank" = median,
         "offense_trimmed_rank" = trimmed) %>%
  left_join(ranks_df, by = c("defense" = "team")) %>%
  rename("defense_mean_rank" = mean,
         "defense_median_rank" = median,
         "defense_trimmed_rank" = trimmed) %>%
  mutate(mean_rank_diff = offense_mean_rank - defense_mean_rank,
         median_rank_diff = offense_median_rank - defense_median_rank,
         trimmed_rank_diff = offense_trimmed_rank - defense_trimmed_rank) %>%
  na.omit() %>%  # drop teams without rankings
  glimpse() %>%
  write_csv("data/points.csv")

# add random forest predictions
rf_pred <- predict(rf_fit, newdata = points_df)
points_pred_df <- points_df %>%
  mutate(rf_pred = rf_pred) %>%
  glimpse() %>%
  write_csv("data/points-rf-pred.csv")
