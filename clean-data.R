
# set working directory
setwd(here::here())

# load packages
library(tidyverse)
library(magrittr)
library(randomForest)

# read data
raw_df <- read_tsv("data/massey-raw.txt", col_names = FALSE) %>%
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
  #select(team = Team, mean = Mean) %>%
  glimpse()

# combine the team 1 offense and team 2 offense data
off_def_df <- bind_rows(df1, df2) %>%
  # left_join(rankings_df, by = c("offense" = "team")) %>%
  # rename("offense_mean" = mean) %>%
  # left_join(rankings_df, by = c("defense" = "team")) %>%
  # rename("defense_mean" = mean) %>%
  na.omit() %>%  # drop teams without rankings
  glimpse() %>%
  write_csv("data/off-def.csv")

# load the rankings data
rating_components_df <- read_csv("data/ratings.csv") %>%
  select(team = Team, ABC:YCM) %>%
  glimpse()

rf_df <- off_def_df %>%
  left_join(rating_components_df, by = c("offense" = "team")) %>%
  rename_at(.vars = vars(ABC:YCM), funs(paste0(., "_offense"))) %>%
  left_join(rating_components_df, by = c("defense" = "team")) %>%
  rename_at(.vars = vars(ABC:YCM), funs(paste0(., "_defense"))) %>%
  glimpse()

rf0_df <- select(rf_df, -game_id, -offense, -defense)
  
rf <- cforest(sqrt(points) ~ ., data = rf0_df)

fit_df <- rf_df %>%
  mutate(rf_pred = as.numeric(predict(rf, newdata = rf_df))) %>%
  select(game_id, points, offense, defense, rf_pred) %>%
  glimpse() %>%
  write_csv("data/fit.csv")