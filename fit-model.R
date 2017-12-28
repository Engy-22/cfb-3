
# load packages
library(tidyverse)
library(magrittr)
library(lme4)
library(rstanarm); options(mc.cores = parallel::detectCores() - 1)

# load data
off_def_df <- read_csv("data/off-def.csv")

# fit model
fit <- stan_lmer(sqrt(points) ~ offense_mean + defense_mean + (1 | offense) + (1 | defense), 
                data = off_def_df, iter = 2000, chains = 3)

# posterior predictions (points)
pred_df <- read_csv("data/off-def-sched.csv")
pp <- posterior_predict(fit, newdata = pred_df)

# convert posterior points to posterior wins
wins_df <- NULL
for (i in 1:nrow(pp)) {
  wins_df_i <- pred_df %>%
    mutate(points = pp[i, ]) %>%
    group_by(game_id) %>%
    summarize(winner = offense[which.max(points)]) %>%
    mutate(simulation = i) 
  wins_df <- rbind(wins_df, wins_df_i)
}

# calculate win probabilities by proportion of simulations won
win_pr_df <- wins_df %>%
  group_by(game_id) %>%
  summarize(w1 = prop.table(table(winner))[1],
            t1 = names(prop.table(table(winner)))[1],
            w2 = prop.table(table(winner))[2],
            t2 = names(prop.table(table(winner)))[2]) %>%
  glimpse() %>%
  mutate(t1_opp = paste0(t1, " (v. ", t2, ")"),
         t2_opp = paste0(t2, " (v. ", t1, ")")) %>%
  glimpse()

win_pr_df1 <- select(win_pr_df, game_id, win_pr = w1, team = t1_opp)
win_pr_df2 <- select(win_pr_df, game_id, win_pr = w2, team = t2_opp)  

win_pr_by_team_df <- bind_rows(win_pr_df1, win_pr_df2) %>%
  select(-game_id) %>%
  select(team, win_pr) %>%
  mutate(team = factor(team)) %>%
  arrange(team) %>%
  glimpse()

md_tab <- knitr::kable(win_pr_by_team_df, digits = 2)
capture.output(md_tab, file = "win-pr-table.md")

