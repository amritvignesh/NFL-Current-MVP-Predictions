library(tidyverse)
library(nflfastR)
library(vip)
library(ggimage)
library(ggplot2)
library(gt)
library(nflreadr)
library(dplyr)
library(nflplotR)
library(ggrepel)
library(rvest)
library(nflreadr)

new_train_data <- data.frame()
for (year in 2007:2022) {
  pbp_load <- load_pbp(year) %>%
    filter(season_type == "REG")
  add_train <- calculate_player_stats(pbp_load, weekly = FALSE) 
  add_train <- add_train %>%
    filter(position == "QB") %>%
    filter(attempts >= 225) %>%
    mutate(yards = passing_yards, cmppct = completions/attempts * 100, anyatt = (passing_yards - sack_yards + 20 * passing_tds - 45 * interceptions)/(attempts + sacks), passrtg = ((completions/attempts - 0.3) * 5 + (passing_yards/attempts - 3) * 0.25 + (passing_tds/attempts) * 20 + 2.375 - (interceptions/attempts * 25))/6 * 100, yards_r = rushing_yards, repa = rushing_epa) %>%
    select(name = player_display_name, team = recent_team, yards, cmppct, passing_tds, interceptions, sacks, pacr, adjepacpoe = dakota, passing_epa, anyatt, passrtg, yards_r, rushing_tds, repa)  
  add_train <- mutate(add_train, year = year)
  new_train_data <- bind_rows(add_train, new_train_data)
}

new_train_data <- new_train_data %>%
  filter(year != 2012)

pbp_23 = load_pbp(2023) %>%
  filter(season_type == "REG")
test_23 <- calculate_player_stats(pbp_23)
test_23 <- test_23 %>%
  filter(position == "QB") %>%
  filter(attempts >= 100) %>%
  mutate(yards = passing_yards, cmppct = completions/attempts * 100, anyatt = (passing_yards - sack_yards + 20 * passing_tds - 45 * interceptions)/(attempts + sacks), passrtg = ((completions/attempts - 0.3) * 5 + (passing_yards/attempts - 3) * 0.25 + (passing_tds/attempts) * 20 + 2.375 - (interceptions/attempts * 25))/6 * 100, yards_r = rushing_yards, repa = rushing_epa) %>%
  select(name = player_display_name, team = recent_team, yards, cmppct, passing_tds, interceptions, sacks, pacr, adjepacpoe = dakota, passing_epa, anyatt, passrtg, yards_r, rushing_tds, repa)  

schedules <- load_schedules(2007:2023)

schedules <- schedules %>%
  filter(game_type == "REG") %>%
  mutate(winningqb = ifelse(home_score > away_score, home_qb_name, ifelse(away_score > home_score, away_qb_name, "NONE"))) 

schedules$winningqb[schedules$game_id == "2023_06_DEN_KC"] <- "Patrick Mahomes"
schedules$home_qb_name[schedules$game_id == "2023_06_DEN_KC"] <- "Patrick Mahomes"
schedules$away_qb_name[schedules$game_id == "2023_06_DEN_KC"] <- "Russell Wilson"

schedules$winningqb[schedules$game_id == "2023_06_BAL_TEN"] <- "Lamar Jackson"
schedules$away_qb_name[schedules$game_id == "2023_06_BAL_TEN"] <- "Lamar Jackson"
schedules$home_qb_name[schedules$game_id == "2023_06_BAL_TEN"] <- "Ryan Tannehill"

schedules$winningqb[schedules$game_id == "2023_06_WAS_ATL"] <- "Sam Howell"
schedules$away_qb_name[schedules$game_id == "2023_06_WAS_ATL"] <- "Sam Howell"
schedules$home_qb_name[schedules$game_id == "2023_06_WAS_ATL"] <- "Desmond Ridder"

schedules$winningqb[schedules$game_id == "2023_06_MIN_CHI"] <- "Kirk Cousins"
schedules$away_qb_name[schedules$game_id == "2023_06_MIN_CHI"] <- "Kirk Cousins"
schedules$home_qb_name[schedules$game_id == "2023_06_MIN_CHI"] <- "Justin Fields"

schedules$winningqb[schedules$game_id == "2023_06_SEA_CIN"] <- "Joe Burrow"
schedules$home_qb_name[schedules$game_id == "2023_06_SEA_CIN"] <- "Joe Burrow"
schedules$away_qb_name[schedules$game_id == "2023_06_SEA_CIN"] <- "Geno Smith"

schedules$winningqb[schedules$game_id == "2023_06_NO_HOU"] <- "C.J. Stroud"
schedules$home_qb_name[schedules$game_id == "2023_06_NO_HOU"] <- "C.J. Stroud"
schedules$away_qb_name[schedules$game_id == "2023_06_NO_HOU"] <- "Derek Carr"

schedules$winningqb[schedules$game_id == "2023_06_IND_JAX"] <- "Trevor Lawrence"
schedules$home_qb_name[schedules$game_id == "2023_06_IND_JAX"] <- "Trevor Lawrence"
schedules$away_qb_name[schedules$game_id == "2023_06_IND_JAX"] <- "Gardner Minshew"

schedules$winningqb[schedules$game_id == "2023_06_CAR_MIA"] <- "Tua Tagovailoa"
schedules$home_qb_name[schedules$game_id == "2023_06_CAR_MIA"] <- "Tua Tagovailoa"
schedules$away_qb_name[schedules$game_id == "2023_06_CAR_MIA"] <- "Bryce Young"

home_team_games <- schedules %>%
  group_by(season, home_team) %>%
  filter(!is.na(winningqb)) %>%
  summarize(home_games = n())
away_team_games <- schedules %>%
  group_by(season, away_team) %>%
  filter(!is.na(winningqb)) %>%
  summarize(away_games = n())

team_games <- inner_join(home_team_games, away_team_games, by = c("season", "home_team" = "away_team")) %>%
  mutate(total_games = home_games + away_games) %>%
  select(season, team = home_team, total_games)

home <- schedules %>%
  group_by(season, home_qb_name, home_team) %>%
  filter(!is.na(winningqb)) %>%
  summarize(home_qb_games = n())
away <- schedules %>%
  group_by(season, away_qb_name, away_team) %>%
  filter(!is.na(winningqb)) %>%
  summarize(away_qb_games = n())
qb_comb <- full_join(home, away, by = c("season", "home_qb_name" = "away_qb_name", "home_team" = "away_team")) %>%
  mutate(home_qb_games = ifelse(is.na(home_qb_games), 0, home_qb_games)) %>%
  mutate(away_qb_games = ifelse(is.na(away_qb_games), 0, away_qb_games)) %>%
  mutate(total_qb_games = home_qb_games + away_qb_games) %>%
  select(season, team = home_team, qb = home_qb_name, total_qb_games)

qb_comb <- inner_join(qb_comb, team_games, by = c("season", "team"))

duplicates <- qb_comb[duplicated(qb_comb[, c("season", "qb")]) | duplicated(qb_comb[, c("season", "qb")]), ]

qb_comb <- qb_comb %>%
  group_by(season, qb) %>%
  summarize(team = paste(team, collapse = ", "), total_qb_games = sum(total_qb_games), total_games = mean(total_games))

wins <- schedules %>%
  group_by(season, winningqb) %>%
  summarize(qb_wins = n())

qb_comb <- left_join(qb_comb, wins, by = c("season", "qb"="winningqb")) %>%
  mutate(qb_wins = ifelse(is.na(qb_wins), 0, qb_wins))

qb_comb <- qb_comb %>%
  mutate(qb_win_pct = qb_wins/total_qb_games) %>%
  mutate(qb_play_pct = total_qb_games/total_games)

new_train_data <- inner_join(new_train_data, qb_comb, by = c("year"="season", "name"="qb")) %>%
  mutate(team = team.y)

qb_comb_23 <- qb_comb %>% filter(season == 2023)

test_23 <- inner_join(test_23, qb_comb_23, by = c("name"="qb")) %>%
  mutate(team = team.y)
  
new_train_data$mvp[new_train_data$year == 2007 & new_train_data$name == 'Tom Brady'] <- 1
new_train_data$mvp[new_train_data$year == 2008 & new_train_data$name == 'Peyton Manning'] <- 1
new_train_data$mvp[new_train_data$year == 2009 & new_train_data$name == 'Peyton Manning'] <- 1
new_train_data$mvp[new_train_data$year == 2010 & new_train_data$name == 'Tom Brady'] <- 1
new_train_data$mvp[new_train_data$year == 2011 & new_train_data$name == 'Aaron Rodgers'] <- 1
new_train_data$mvp[new_train_data$year == 2013 & new_train_data$name == 'Peyton Manning'] <- 1
new_train_data$mvp[new_train_data$year == 2014 & new_train_data$name == 'Aaron Rodgers'] <- 1
new_train_data$mvp[new_train_data$year == 2015 & new_train_data$name == 'Cam Newton'] <- 1
new_train_data$mvp[new_train_data$year == 2016 & new_train_data$name == 'Matt Ryan'] <- 1
new_train_data$mvp[new_train_data$year == 2017 & new_train_data$name == 'Tom Brady'] <- 1
new_train_data$mvp[new_train_data$year == 2018 & new_train_data$name == 'Patrick Mahomes'] <- 1
new_train_data$mvp[new_train_data$year == 2019 & new_train_data$name == 'Lamar Jackson'] <- 1
new_train_data$mvp[new_train_data$year == 2020 & new_train_data$name == 'Aaron Rodgers'] <- 1
new_train_data$mvp[new_train_data$year == 2021 & new_train_data$name == 'Aaron Rodgers'] <- 1
new_train_data$mvp[new_train_data$year == 2022 & new_train_data$name == 'Patrick Mahomes'] <- 1

new_train_data <- new_train_data %>%
  mutate(mvp = ifelse(is.na(mvp), 0, mvp))

test_23 <- test_23 %>%
  mutate(mvp = 0)

schedules_23 <- schedules %>%
  filter(season == 2023)

test_23 <- inner_join(test_23, schedules_23, by = c("name"="winningqb"))

test_23 <- distinct(test_23, name, .keep_all = TRUE)

pca <- prcomp(new_train_data[,c(3:5,8:15)], scale = TRUE)
weights <- pca$rotation[,1]
weighted_avg <- rowSums(new_train_data[,c(3:5,8:15)] * weights)
new_train_data$metric <- weighted_avg

pca_test <- prcomp(test_23[,c(3:5,8:15)], scale = TRUE)
weights_test <- pca_test$rotation[,1]
weighted_avg_test <- rowSums(test_23[,c(3:5,8:15)] * weights_test)
test_23$metric <- weighted_avg_test

try_train_reg <- glm(mvp ~ yards + cmppct + passing_tds + pacr + adjepacpoe + passing_epa + anyatt + passrtg + yards_r + rushing_tds + repa + interceptions + sacks + qb_win_pct + qb_play_pct, data = new_train_data, family = binomial) # no pca, all stats


new_train_reg <- glm(mvp ~ metric + interceptions + sacks + qb_win_pct + qb_play_pct, data = new_train_data, family = binomial) # pca utilized to combine most stats

test_23 <- test_23 %>%
  mutate(prediction = predict(new_train_reg, test_23, type = "response")) %>%
  mutate(mvp_prob = prediction/sum(prediction)) %>%
  arrange(-mvp_prob) %>%
  ungroup() 


test_23 <- test_23 %>%
  mutate(mvp_prob = round(mvp_prob, 3)) %>%
  select(name, team, mvp_prob)


test_23 %>% filter(row_number() <= 10) %>%  gt() %>% # gives output with pca
  cols_align(
    align = "center",
    columns = c(name, team, mvp_prob)
  ) %>%
  data_color(
    columns = mvp_prob,
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::blue_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  cols_label(
    name = md("**Player**"),
    team = md("**Team**"),
    mvp_prob = md("**MVP Probability**"),
  ) %>%
  tab_header(
    title = md("**2023 NFL MVP Probability**"),
    subtitle = "Based on NFL MVP Data from 2007 - 2022 Involving Basic and Advanced Statistics, After Week 10"
  )

test <- test_23 %>%
  mutate(prediction = predict(try_train_reg, test_23, type = "response")) %>%
  mutate(mvp_prob = prediction/sum(prediction)) %>%
  arrange(-mvp_prob) %>%
  ungroup() 


test <- test %>%
  mutate(mvp_prob = round(mvp_prob, 3)) %>%
  select(name, team, mvp_prob)


test %>% filter(row_number() <= 10) %>%  gt() %>% # gives output with all pure stats
  cols_align(
    align = "center",
    columns = c(name, team, mvp_prob)
  ) %>%
  data_color(
    columns = mvp_prob,
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::blue_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  cols_label(
    name = md("**Player**"),
    team = md("**Team**"),
    mvp_prob = md("**MVP Probability**"),
  ) %>%
  tab_header(
    title = md("**2023 NFL MVP Probability**"),
    subtitle = "Based on NFL MVP Data from 2007 - 2022 Involving Basic and Advanced Statistics, After Week 10"
  )

summary(try_train_reg)
summary(new_train_reg)
