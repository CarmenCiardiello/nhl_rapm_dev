library(tidyverse)
library(tidymodels)
library(finetune)
library(hockeyR)
library(duckdb)
library(dbplyr)
library(agua)
library(glue)
library(h2o)
library(bundle)
library(future)

# LOAD IN A SAMPLE OF THE PBP DATA TO FIGURE OUT WHAT ARE THE INITIAL FEATURES

# THINGS TO INCLUDE IN THE MODEL
# LOCATION, ANGLE, DELTA LOCATION, DELTA ANGLE, STRENGTH STATE, INDICATOR OF LEADING/TIED/TRAILING, RUSH/REBOUND/FLURRY INDICATOR, FLAGS FOR WEIRD LOCATIONS (BELOW GOALINE, OUT OF ZONE, ETC.)
# LOOK AT HOCKEYVIZ FOR SOME INSIGHT INTO FEATURE DEFINITIONS: https://hockeyviz.com/txt/xg6

########################### CREATE FUNCTION FOR FEATURE ENGINEERING ###################
add_features <- function(pbp_df){
  
  pbp_df <- pbp_df %>% 
    mutate(secondary_type = case_when(
      secondary_type == "Wrist Shot" ~ "wrist", 
      secondary_type == "Slap Shot" ~ "slap", 
      secondary_type == "Snap Shot" ~ "snap", 
      secondary_type == "Backhand" ~ "backhand", 
      secondary_type == "Deflected" ~ "deflected", 
      secondary_type == "Tip-In" ~ "tip-in", 
      secondary_type == "Wrap-around" ~ "wrap-around", 
      secondary_type == "Batted" ~ "bat", 
      secondary_type == "Poke" ~ "poke", 
      .default = secondary_type
    )) %>% 
    mutate(
      strength_state = as.factor(strength_state), 
      strength_code = as.factor(strength_code),
      period = as.factor(period), 
      score_margin = ifelse(event_team_type == "home", home_score - away_score, away_score - home_score), 
      score_margin = as.factor(score_margin),
      total_goals = as.factor(away_score + home_score),
      is_shot = ifelse(event_type %in% c("GOAL", "MISSED_SHOT", "BLOCKED_SHOT", "SHOT"), 1, 0),
      is_home = ifelse(event_team_type == "home", 1, 0), 
      event_zone = case_when(
        x >= -25 & x <= 25 ~ "NZ",
        (x_fixed < -25 & event_team == home_name) |
          (x_fixed > 25 & event_team == away_name) ~ "DZ",
        (x_fixed > 25 & event_team == home_name) |
          (x_fixed < -25 & event_team == away_name) ~ "OZ"
      ),
      last_event_zone = as.factor(lag(event_zone)),
      event_team_type = as.factor(event_team_type),
      last_event_team = as.factor(lag(event_team_type)),
      time_since_last = game_seconds - lag(game_seconds),
      prev_event = as.factor(lag(event_type)), 
      prev_event_x = lag(x), 
      prev_event_y = lag(y),
      event_dist_delta = sqrt((x - prev_event_x)^2 + (y - prev_event_y)^2),
      is_rebound = ifelse(is_shot == 1 & lag(is_shot) == 1 & abs(game_seconds_remaining - lag(game_seconds_remaining)) <= 3, 1, 0),
      angle_delta = ifelse(lag(is_shot) == 1 & is_shot == 1, shot_angle - lag(shot_angle), NA),
      is_flurry = ifelse(is_shot == 1 & lag(is_shot) == 1 & lag(is_shot, 2) == 1 & is_rebound == 1 & lag(is_rebound) == 1, 1, 0),
      out_of_zone = case_when(
        event_team_type == "home" & x_fixed < 25 ~ 1, 
        event_team_type == "away" & x_fixed > -25 ~ 1, 
        .default = 0
      ), 
      era_2011_2013 = ifelse(
        season %in% c("20102011","20112012","20122013"),
        1, 0
      ),
      era_2014_2018 = ifelse(
        season %in% c("20132014","20142015","20152016","20162017","20172018"),
        1, 0
      ),
      era_2019_2021 = ifelse(
        season %in% c("20182019","20192020","20202021"),
        1, 0
      ),
      era_2022_on = ifelse(
        era_2011_2013 != 1 & era_2014_2018 != 1 & era_2019_2021 != 1, 1, 0
      ),
      # these are only for the ST model
      # number of skaters for home team
      event_team_skaters = ifelse(event_team == home_name, home_skaters, away_skaters), 
      # number of skaters for away team
      opponent_team_skaters = ifelse(event_team == home_name, away_skaters, home_skaters),
      # total skaters on ice for both teams
      total_skaters_on = as.factor(event_team_skaters + opponent_team_skaters), 
      # number advantage for event team
      event_team_adv = as.factor(event_team_skaters - opponent_team_skaters),
      behind_net = ifelse(abs(x_fixed >= 89), 1, 0), 
      is_rush = ifelse(last_event_zone %in% c("NZ","DZ") & time_since_last <= 6, 1, 0), 
      empty_net = ifelse(is.na(empty_net) | empty_net == FALSE, 0, 1),
      shot_type = as.factor(secondary_type),
      # not sure if I am going to make this a regression or classification model
      goal = ifelse(event_type == "GOAL", 1, 0),
      is_goal = ifelse(goal == 1, "yes", "no"), 
      is_goal = as.factor(is_goal)
    ) %>% 
    mutate(season = as.character(season), 
           season_type = as.character(season_type), 
           game_date = as_date(game_date), 
           home_id = as.character(home_id), 
           away_id = as.character(away_id))
  
  
  return(pbp_df)
  
}

############# CREATE FUNCTION FOR FILTERING PBP FOR RELEVANT DATA ###############
# DONE AFTER 
filter_shots <- function(pbp_df){
  
  # filter pbp data frame for 
  pbp_df <- pbp_df %>% 
    filter(
      event_type %in% c("GOAL", "MISSED_SHOT", "SHOT"), 
      period_type != "SHOOTOUT"
    ) 
  
  return(pbp_df)
  
}

##################### MODEL TRAINING ###############################

xg_model_train <- function(sample_per_season = 40000){
  # add the necessary features to the pbp data
  
  plan(multisession)
  
  df <- 2011:2024 %>% 
    map_df(~{
      load_pbp(season = ., shift_events = FALSE) %>% 
      add_features() %>% 
      filter_shots() %>% 
      sample_n(sample_per_season)
    }) 
    
  
  plan(NULL)
  
  # create training and test data split
  set.seed(123)
  split_obj <- initial_split(df, prop = 0.75)
  train_data <- training(split_obj)
  test_data <- testing(split_obj)
  # recipe to prep the data for training
  rec <- df %>% 
    recipe(
      is_goal ~ strength_code + total_skaters_on + event_team_adv + period + event_team_type + 
        last_event_zone + last_event_team + prev_event + time_since_last + 
        shot_distance + shot_angle + event_dist_delta + is_rebound + is_flurry + 
        out_of_zone + era_2011_2013 + era_2014_2018 + era_2019_2021 + era_2022_on + 
        behind_net + is_rush + empty_net + shot_type
    ) %>% 
    step_novel(all_nominal_predictors()) %>% 
    step_other(all_nominal_predictors(), threshold = 0.005) %>% 
    step_dummy(all_nominal_predictors(), one_hot = TRUE)
  
  # Make AutoML model spec and workflow
  model_spec <- auto_ml() %>% 
    set_engine("h2o", max_runtime_secs = 300, seed = 1, validation = 0.20) %>% 
    set_mode("classification")
  
  auto_workflow <- workflow() %>% 
    add_model(model_spec) %>% 
    add_recipe(rec)
  
  # Train and save the model
  h2o.init()
  
  plan(multisession)
  
  auto_fit <- fit(auto_workflow, data = train_data)
  
  plan(NULL)
  
  auto_fit_bundle <- bundle(auto_fit)
  saveRDS(auto_fit_bundle, file = "xg_model.rds") #save the object
  h2o_end()
  
  # Return the models and the data split with the training and testing data
  data_package <- list(auto_fit, split_obj)
  return(data_package)
  
}

apply_xg_model <- function(pbp_df){
  
  # pbp_df is the data from the function load_pbp() in hockeyR package
  
  # first need to split the data for the shot and non shot events, then apply model to shot events
  
  pbp_df <- pbp_df %>% 
    filter(period_type != "SHOOTOUT")
  
  change_df <- pbp_df %>% 
    filter(event_type == "CHANGE")
  
  pbp_df <- pbp_df %>% 
    anti_join(change_df) %>% 
    add_features()
  
  change_df <- change_df %>% add_features()
  
  non_shots <- pbp_df %>% 
    filter(
      event_type != "GOAL", event_type != "SHOT", event_type != "MISSED_SHOT"
      )
  
  # shots are events in pbp data that are not logged in non_shots data frame
  shots <- pbp_df %>% 
    anti_join(non_shots)
  
  # need to unbundle the xg model workflow first and start the h2o instance
  h2o_start()
  xg_mod <- readRDS("xg_model.rds") %>% unbundle()
  
  # apply to shots data frame and end h2o instance
  goal_probs <- predict(xg_mod, shots, type = "prob") %>% 
    rename(xg = .pred_yes) %>% 
    select(xg)
  
  h2o_end()
  
  if("xg" %in% colnames(shots)){
    
    shots <- shots %>% 
      select(-xg) %>% # get rid of native hockeyR xg model
      bind_cols(goal_probs) %>% 
      relocate(xg)
    
  } else{
    
    shots <- shots %>% 
      bind_cols(goal_probs) %>% 
      relocate(xg)
    
    non_shots <- non_shots %>% 
      mutate(xg = NA) %>% 
      relocate(xg)
    
  }
  
  # bind the shooting and non-shooting events back together
  df <- shots %>% 
    bind_rows(non_shots) %>% 
    bind_rows(change_df)
  
  return(df)
  
  
}


