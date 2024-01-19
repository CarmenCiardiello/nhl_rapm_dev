library(tidyverse)
library(hockeyR)
library(glmnet)
library(future)
library(bundle) 
library(tidymodels)
library(h2o)

source("xg_train_automl.R")

get_pbp_data <- function(season_start = 2023, season_end = 2023){
  
  # seasons formatted as follows: 2022-2023 season is 2023
  
  # load in the play by play information
  pbp_df <- season_start:season_end %>% 
    map_df(~{
      load_pbp(season = ., shift_events = TRUE) %>% 
        mutate(game_date = as_date(game_date), 
               game_score_state = paste(home_score, away_score, sep = "v"), 
               event_length = lead(game_seconds) - game_seconds) %>% 
        filter(period_type != "SHOOTOUT")
    })
  
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

add_shift_context_ev <- function(df){
  
  # df is the output get_pbp_data
  
  ### ADD IN YOUR EVENTS ###
  
  fenwick <- c("SHOT", "MISS", "GOAL")
  corsi <- c("SHOT", "MISS", "BLOCK", "GOAL")
  even_strength <- c("3v3", "4v4", "5v5")
  power_play <- c("5v4", "4v5", "5v3", "3v5", "4v3", "3v4")
  home_power_play <- c("5v4", "5v3", "4v3")
  away_power_play <- c("4v5", "3v5", "3v4")
  empty_net <- c("5vE", "Ev5", "4vE", "Ev4", "3vE", "Ev3")
  meaningful_events <- c("FACEOFF", "GOAL", "BLOCK", "SHOT", "MISS", "HIT", "TAKE", "GIVE", "CHANGE")
  stoppages <- c("PGSTR", "PGEND", "ANTHEM", "PSTR", "FACEOFF", "GOAL", "STOP", "PENL", 
                 "PEND", "CHL", "GEND", "GOZ", "EISTR", "EIEND", "EGT", "EGPID")
  
  ########### CREATE BACK-TO-BACKS ################
  teams <- df %>%
    group_by(game_date) %>%
    summarize(Yesterday_Home_Team = unique(home_name), Yesterday_Away_Team = unique(away_name))
  
  teams <- teams %>%
    group_by(game_date) %>%
    summarise_all(funs(paste(na.omit(.), collapse = " - "))) %>% 
    mutate(game_date = game_date + 1)
  
  df <- df %>% 
    left_join(teams)
  
  df$Home_Home_Yesterday <- mapply(grepl, pattern=df$home_name, x=df$Yesterday_Home_Team)
  df$Home_Away_Yesterday <- mapply(grepl, pattern=df$home_name, x=df$Yesterday_Away_Team)
  df$Away_Home_Yesterday <- mapply(grepl, pattern=df$away_name, x=df$Yesterday_Home_Team)
  df$Away_Away_Yesterday <- mapply(grepl, pattern=df$away_name, x=df$Yesterday_Away_Team)
  
  df$Home_BTB <- ifelse((df$Home_Home_Yesterday==1 | df$Home_Away_Yesterday==1), 1, 0)
  df$Away_BTB <- ifelse((df$Away_Home_Yesterday==1 | df$Away_Away_Yesterday==1), 1, 0)
  
  ### GET RID OF EVENTS WE DO NOT CARE ABOUT ###
  
  df <- df %>%
    filter(period != 5)
  
  ### CONVERT NAs TO "MISSING PLAYER" ###
  
  df$home_on_1 <- ifelse(is.na(df$home_on_1), "MISSING_PLAYER", df$home_on_1)
  df$home_on_2 <- ifelse(is.na(df$home_on_2), "MISSING_PLAYER", df$home_on_2)
  df$home_on_3 <- ifelse(is.na(df$home_on_3), "MISSING_PLAYER", df$home_on_3)
  df$home_on_4 <- ifelse(is.na(df$home_on_4), "MISSING_PLAYER", df$home_on_4)
  df$home_on_5 <- ifelse(is.na(df$home_on_5), "MISSING_PLAYER", df$home_on_5)
  df$home_on_6 <- ifelse(is.na(df$home_on_6), "MISSING_PLAYER", df$home_on_6)
  df$away_on_1 <- ifelse(is.na(df$away_on_1), "MISSING_PLAYER", df$away_on_1)
  df$away_on_2 <- ifelse(is.na(df$away_on_2), "MISSING_PLAYER", df$away_on_2)
  df$away_on_3 <- ifelse(is.na(df$away_on_3), "MISSING_PLAYER", df$away_on_3)
  df$away_on_4 <- ifelse(is.na(df$away_on_4), "MISSING_PLAYER", df$away_on_4)
  df$away_on_5 <- ifelse(is.na(df$away_on_5), "MISSING_PLAYER", df$away_on_5)
  df$away_on_6 <- ifelse(is.na(df$away_on_6), "MISSING_PLAYER", df$away_on_6)
  
  ### ADD SHIFT CHANGE INDEX ###
  
  df$shift_change <- ifelse(lag(df$home_on_1)==df$home_on_1 & lag(df$home_on_2)==df$home_on_2 & lag(df$home_on_3)==df$home_on_3 &
                              lag(df$home_on_4)==df$home_on_4 & lag(df$home_on_5)==df$home_on_5 & lag(df$home_on_6)==df$home_on_6 &
                              lag(df$away_on_1)==df$away_on_1 & lag(df$away_on_2)==df$away_on_2 & lag(df$away_on_3)==df$away_on_3 &
                              lag(df$away_on_4)==df$away_on_4 & lag(df$away_on_5)==df$away_on_5 & lag(df$away_on_6)==df$away_on_6 &
                              lag(df$home_score)==df$home_score & lag(df$away_score)==df$away_score & lag(df$period)==df$period &  
                              lag(df$home_goalie)==df$home_goalie & lag(df$away_goalie)==df$away_goalie & lag(df$game_id)==df$game_id, 
                            0, 1)
  
  df$shift_change <- ifelse(is.na(df$shift_change), 0, df$shift_change)
  
  df$shift_change_index <- cumsum(df$shift_change)
  
  ### ADD PP EXPIRY STATUS ###
  
  df$home_pp_expiry <- ifelse(df$strength_state %in% even_strength & lag(df$strength_state) %in% home_power_play &
                                df$shift_change==1, 1, 0)
  
  df$home_pp_expiry <- ifelse((lag(df$event_type) %in% stoppages & df$game_seconds==lag(df$game_seconds)) | 
                                (lag(lag(df$event_type)) %in% stoppages & lag(lag(df$game_seconds))==df$game_seconds) | 
                                (lag(lag(lag(df$event_type))) %in% stoppages & lag(lag(lag(df$game_seconds)))==df$game_seconds),
                              0, df$home_pp_expiry)
  
  df$home_pp_expiry[is.na(df$home_pp_expiry)] <- 0
  
  df$away_pp_expiry <- ifelse(df$strength_state %in% even_strength & lag(df$strength_state) %in% away_power_play &
                                df$shift_change==1, 1, 0)
  
  df$away_pp_expiry <- ifelse((lag(df$event_type) %in% stoppages & df$game_seconds==lag(df$game_seconds)) | 
                                (lag(lag(df$event_type)) %in% stoppages & lag(lag(df$game_seconds))==df$game_seconds) | 
                                (lag(lag(lag(df$event_type))) %in% stoppages & lag(lag(lag(df$game_seconds)))==df$game_seconds),
                              0, df$away_pp_expiry)
  
  df$away_pp_expiry[is.na(df$away_pp_expiry)] <- 0
  
  ### ADD IMPORTANT CONTEXT TO PBP DATA ###
  
  df$event_zone <- ifelse(df$event_zone=="DZ" & df$event_type=="BLOCK", "OZ", df$event_zone)
  df$home_zone <- ifelse(df$event_team==df$home_name, df$event_zone, NA)
  df$home_zone <- ifelse(df$event_team==df$away_name & df$event_zone=="OZ", "DZ", df$home_zone)
  df$home_zone <- ifelse(df$event_team==df$away_name & df$event_zone=="DZ", "OZ", df$home_zone)
  df$home_zone <- ifelse(df$event_zone=="NZ", "NZ", df$home_zone)
  
  df$home_ozs <- ifelse((df$event_type=="FACEOFF" & df$home_zone=="OZ") & (
    (df$shift_change==1) | 
      (df$game_seconds==lag(df$game_seconds) & lag(df$shift_change==1)) |
      (df$game_seconds==lag(lag(df$game_seconds)) & lag(lag((df$shift_change)==1))) |
      (df$game_seconds==lag(lag(lag(df$game_seconds))) & lag(lag(lag(df$shift_change)))==1) |
      (df$game_seconds==lag(lag(lag(lag(df$game_seconds)))) & lag(lag(lag(lag(df$shift_change))))==1)
  ), 1, 0)
  
  df$away_ozs <- ifelse((df$event_type=="FACEOFF" & df$home_zone=="DZ") & (
    (df$shift_change==1) | 
      (df$game_seconds==lag(df$game_seconds) & lag(df$shift_change==1)) |
      (df$game_seconds==lag(lag(df$game_seconds)) & lag(lag((df$shift_change)==1))) |
      (df$game_seconds==lag(lag(lag(df$game_seconds))) & lag(lag(lag(df$shift_change)))==1) |
      (df$game_seconds==lag(lag(lag(lag(df$game_seconds)))) & lag(lag(lag(lag(df$shift_change))))==1)
  ), 1, 0)
  
  df$nzs <- ifelse((df$event_type=="FACEOFF" & df$home_zone=="NZ") & (
    (df$shift_change==1) | 
      (df$game_seconds==lag(df$game_seconds) & lag(df$shift_change==1)) |
      (df$game_seconds==lag(lag(df$game_seconds)) & lag(lag((df$shift_change)==1))) |
      (df$game_seconds==lag(lag(lag(df$game_seconds))) & lag(lag(lag(df$shift_change)))==1) |
      (df$game_seconds==lag(lag(lag(lag(df$game_seconds)))) & lag(lag(lag(lag(df$shift_change))))==1)
  ), 1, 0)
  
  df$home_xgf <- ifelse(df$event_team==df$home_name, df$xg, 0)
  df$away_xgf <- ifelse(df$event_team!=df$home_name, df$xg, 0)
  df$home_ozs[is.na(df$home_ozs)] <- 0
  df$away_ozs[is.na(df$away_ozs)] <- 0
  df$tied <- ifelse(df$home_score==df$away_score, 1, 0)
  df$home_lead_1 <- ifelse(df$home_score-df$away_score==1, 1, 0)
  df$home_lead_2 <- ifelse(df$home_score-df$away_score==2, 1, 0)
  df$home_lead_3 <- ifelse(df$home_score-df$away_score>=3, 1, 0)
  df$away_lead_1 <- ifelse(df$home_score-df$away_score==(-1), 1, 0)
  df$away_lead_2 <- ifelse(df$home_score-df$away_score==(-2), 1, 0)
  df$away_lead_3 <- ifelse(df$home_score-df$away_score<=(-3), 1, 0)
  df$Five <- ifelse(df$strength_state=="5v5", 1, 0)
  df$Four <- ifelse(df$strength_state=="4v4", 1, 0)
  df$Three <- ifelse(df$strength_state=="3v3", 1, 0)
  df$home_xgf[is.na(df$home_xgf)] <- 0
  df$away_xgf[is.na(df$away_xgf)] <- 0
  df$period_1 <- ifelse(df$period=="1", 1, 0)
  df$period_2 <- ifelse(df$period=="2", 1, 0)
  df$period_3 <- ifelse(df$period=="3", 1, 0)
  df$Home_BTB[is.na(df$Home_BTB)] <- 0
  df$Away_BTB[is.na(df$Away_BTB)] <- 0
  
  # filter for even strength pbp
  dfev <- df %>%
    filter(strength_state %in% even_strength)
  
  return(dfev)
  
}

group_shifts <- function(dfev){
  
  # df is data from get_pbp_data piped through add_shift_context
  
  ### GROUP SHIFTS TO PREPARE FOR RAPM ###
  
  shifts_grouped <- dfev %>%
    group_by(game_id, shift_change_index, period, game_score_state, home_on_1, home_on_2, home_on_3, home_on_4, home_on_5, home_on_6, 
             away_on_1, away_on_2, away_on_3, away_on_4, away_on_5, away_on_6, home_goalie, away_goalie) %>%
    summarise(shift_length = sum(event_length), homexGF = sum(home_xgf), awayxGF = sum(away_xgf), 
              Home_OZS = max(home_ozs), Away_OZS = max(away_ozs), NZS = max(nzs), Home_Up_1 = max(home_lead_1), Home_Up_2 = max(home_lead_2), Home_Up_3 = max(home_lead_3),
              Away_Up_1 = max(away_lead_1), Away_Up_2 = max(away_lead_2), Away_Up_3 = max(away_lead_3), Tied = max(tied), 
              State_5v5 = max(Five), State_4v4 = max(Four), State_3v3 = max(Three), Period_1 = max(period_1), Period_2 = max(period_2), Period_3 = max(period_3),
              Home_BTB = max(Home_BTB), Away_BTB = max(Away_BTB), Home_PPx = max(home_pp_expiry), Away_PPx = max(away_pp_expiry)) %>%
    filter(shift_length > 0)
  
  ### BUILD SHIFTS FROM THE PERSPECTIVE OF OFFENSE AS HOME ###
  
  home_offense_all <- shifts_grouped %>%
    rename(offense_1 = home_on_1, offense_2 = home_on_2, offense_3 = home_on_3, offense_4 = home_on_4, offense_5 = home_on_5, offense_6 = home_on_6,
           defense_1 = away_on_1, defense_2 = away_on_2, defense_3 = away_on_3, defense_4 = away_on_4, defense_5 = away_on_5, defense_6 = away_on_6,
           Off_Zonestart = Home_OZS, Def_Zonestart = Away_OZS, Neu_Zonestart = NZS, Up_1 = Home_Up_1, Up_2 = Home_Up_2, Up_3 = Home_Up_3,
           Down_1 = Away_Up_1, Down_2 = Away_Up_2, Down_3 = Away_Up_3, xGF = homexGF, BTB = Home_BTB, Opp_BTB = Away_BTB, PPx = Home_PPx, PKx = Away_PPx) %>%
    select(game_id, shift_change_index, period, game_score_state,
           offense_1, offense_2, offense_3, offense_4, offense_5, offense_6, 
           defense_1, defense_2, defense_3, defense_4, defense_5, defense_6, 
           xGF, shift_length, shift_change_index, Tied, State_5v5, State_4v4, State_3v3, 
           Up_1, Up_2, Up_3, Down_1, Down_2, Down_3, Off_Zonestart, Def_Zonestart, Neu_Zonestart,
           offense_goalie = home_goalie, defense_goalie = away_goalie, Period_1, Period_2, Period_3, BTB, Opp_BTB, PPx, PKx) %>%
    mutate(xGF_60 = xGF*3600/shift_length, is_home = 1)
  
  ### BUILD SHIFTS FROM THE PERSPECTIVE OF OFFENSE AS AWAY ###
  
  away_offense_all <- shifts_grouped %>%
    rename(offense_1 = away_on_1, offense_2 = away_on_2, offense_3 = away_on_3, offense_4 = away_on_4, offense_5 = away_on_5, offense_6 = away_on_6,
           defense_1 = home_on_1, defense_2 = home_on_2, defense_3 = home_on_3, defense_4 = home_on_4, defense_5 = home_on_5, defense_6 = home_on_6,
           Off_Zonestart = Away_OZS, Def_Zonestart = Home_OZS, Neu_Zonestart = NZS, Up_1 = Away_Up_1, Up_2 = Away_Up_2, Up_3 = Away_Up_3,
           Down_1 = Home_Up_1, Down_2 = Home_Up_2, Down_3 = Home_Up_3, xGF = awayxGF, BTB = Away_BTB, Opp_BTB = Home_BTB, PPx = Away_PPx, PKx = Home_PPx) %>%
    select(game_id, shift_change_index, period, game_score_state,
           offense_1, offense_2, offense_3, offense_4, offense_5, offense_6, 
           defense_1, defense_2, defense_3, defense_4, defense_5, defense_6, 
           xGF, shift_length, shift_change_index, Tied, State_5v5, State_4v4, State_3v3, 
           Up_1, Up_2, Up_3, Down_1, Down_2, Down_3, Off_Zonestart, Def_Zonestart, Neu_Zonestart,
           offense_goalie = away_goalie, defense_goalie = home_goalie, Period_1, Period_2, Period_3, BTB, Opp_BTB, PPx, PKx) %>%
    mutate(xGF_60 = xGF*3600/shift_length, is_home = 0)
  
  ### MAKE NOTE OF GOALIES USING FUNNY NAME - THEY WILL BE REMOVED LATER ###
  
  home_offense_all$offense_1 <- ifelse(home_offense_all$offense_goalie==home_offense_all$offense_1, "GOALIE.GUY", home_offense_all$offense_1)
  home_offense_all$offense_2 <- ifelse(home_offense_all$offense_goalie==home_offense_all$offense_2, "GOALIE.GUY", home_offense_all$offense_2)
  home_offense_all$offense_3 <- ifelse(home_offense_all$offense_goalie==home_offense_all$offense_3, "GOALIE.GUY", home_offense_all$offense_3)
  home_offense_all$offense_4 <- ifelse(home_offense_all$offense_goalie==home_offense_all$offense_4, "GOALIE.GUY", home_offense_all$offense_4)
  home_offense_all$offense_5 <- ifelse(home_offense_all$offense_goalie==home_offense_all$offense_5, "GOALIE.GUY", home_offense_all$offense_5)
  home_offense_all$offense_6 <- ifelse(home_offense_all$offense_goalie==home_offense_all$offense_6, "GOALIE.GUY", home_offense_all$offense_6)
  
  home_offense_all$defense_1 <- ifelse(home_offense_all$defense_goalie==home_offense_all$defense_1, "GOALIE.GUY", home_offense_all$defense_1)
  home_offense_all$defense_2 <- ifelse(home_offense_all$defense_goalie==home_offense_all$defense_2, "GOALIE.GUY", home_offense_all$defense_2)
  home_offense_all$defense_3 <- ifelse(home_offense_all$defense_goalie==home_offense_all$defense_3, "GOALIE.GUY", home_offense_all$defense_3)
  home_offense_all$defense_4 <- ifelse(home_offense_all$defense_goalie==home_offense_all$defense_4, "GOALIE.GUY", home_offense_all$defense_4)
  home_offense_all$defense_5 <- ifelse(home_offense_all$defense_goalie==home_offense_all$defense_5, "GOALIE.GUY", home_offense_all$defense_5)
  home_offense_all$defense_6 <- ifelse(home_offense_all$defense_goalie==home_offense_all$defense_6, "GOALIE.GUY", home_offense_all$defense_6)
  
  away_offense_all$offense_1 <- ifelse(away_offense_all$offense_goalie==away_offense_all$offense_1, "GOALIE.GUY", away_offense_all$offense_1)
  away_offense_all$offense_2 <- ifelse(away_offense_all$offense_goalie==away_offense_all$offense_2, "GOALIE.GUY", away_offense_all$offense_2)
  away_offense_all$offense_3 <- ifelse(away_offense_all$offense_goalie==away_offense_all$offense_3, "GOALIE.GUY", away_offense_all$offense_3)
  away_offense_all$offense_4 <- ifelse(away_offense_all$offense_goalie==away_offense_all$offense_4, "GOALIE.GUY", away_offense_all$offense_4)
  away_offense_all$offense_5 <- ifelse(away_offense_all$offense_goalie==away_offense_all$offense_5, "GOALIE.GUY", away_offense_all$offense_5)
  away_offense_all$offense_6 <- ifelse(away_offense_all$offense_goalie==away_offense_all$offense_6, "GOALIE.GUY", away_offense_all$offense_6)
  
  away_offense_all$defense_1 <- ifelse(away_offense_all$defense_goalie==away_offense_all$defense_1, "GOALIE.GUY", away_offense_all$defense_1)
  away_offense_all$defense_2 <- ifelse(away_offense_all$defense_goalie==away_offense_all$defense_2, "GOALIE.GUY", away_offense_all$defense_2)
  away_offense_all$defense_3 <- ifelse(away_offense_all$defense_goalie==away_offense_all$defense_3, "GOALIE.GUY", away_offense_all$defense_3)
  away_offense_all$defense_4 <- ifelse(away_offense_all$defense_goalie==away_offense_all$defense_4, "GOALIE.GUY", away_offense_all$defense_4)
  away_offense_all$defense_5 <- ifelse(away_offense_all$defense_goalie==away_offense_all$defense_5, "GOALIE.GUY", away_offense_all$defense_5)
  away_offense_all$defense_6 <- ifelse(away_offense_all$defense_goalie==away_offense_all$defense_6, "GOALIE.GUY", away_offense_all$defense_6)
  
  combined_shifts <- full_join(home_offense_all, away_offense_all)
  
  ### REMOVE DUMMY VARIABLES THAT WE WILL NOT BE USING ###
  
  subsetted_shifts <- subset(
    combined_shifts, 
    select = -c(game_id:period, game_score_state, xGF, 
               Tied, State_5v5, Period_1:Period_3, 
               offense_goalie, defense_goalie)
  )
  
  return(subsetted_shifts)
  
}

create_shift_dummies <- function(subsetted_shifts){
  
  # input is the output of group_shifts
  ### CONVERT INTO DUMMY VARIABLES ###
  
  combined_shifts_dummies <- fastDummies::dummy_cols(subsetted_shifts)
  
  
  combined_shifts_dummies <- subset(combined_shifts_dummies, select = -c(offense_1:defense_6))
  
  ### CLEAN NAMES - THIS WAY WE CAN REDUCE TO EACH INDIVIDUAL SKATER ###
  
  colnames(combined_shifts_dummies) = gsub("offense_1", "offense_", colnames(combined_shifts_dummies))
  colnames(combined_shifts_dummies) = gsub("offense_2", "offense_", colnames(combined_shifts_dummies))
  colnames(combined_shifts_dummies) = gsub("offense_3", "offense_", colnames(combined_shifts_dummies))
  colnames(combined_shifts_dummies) = gsub("offense_4", "offense_", colnames(combined_shifts_dummies))
  colnames(combined_shifts_dummies) = gsub("offense_5", "offense_", colnames(combined_shifts_dummies))
  colnames(combined_shifts_dummies) = gsub("offense_6", "offense_", colnames(combined_shifts_dummies))
  colnames(combined_shifts_dummies) = gsub("defense_1", "defense_", colnames(combined_shifts_dummies))
  colnames(combined_shifts_dummies) = gsub("defense_2", "defense_", colnames(combined_shifts_dummies))
  colnames(combined_shifts_dummies) = gsub("defense_3", "defense_", colnames(combined_shifts_dummies))
  colnames(combined_shifts_dummies) = gsub("defense_4", "defense_", colnames(combined_shifts_dummies))
  colnames(combined_shifts_dummies) = gsub("defense_5", "defense_", colnames(combined_shifts_dummies))
  colnames(combined_shifts_dummies) = gsub("defense_6", "defense_", colnames(combined_shifts_dummies))
  
  ### REMOVE DUPLICATE SKATERS ###
  
  combined_shifts_dummies <- as.data.frame(
    lapply(
      split.default(combined_shifts_dummies, names(combined_shifts_dummies)), function(x) Reduce(`+`, x)
    )
  )
  
  ### REMOVE GOALIES AND CREATE TRAINING DATA###
  
  combined_shifts_dummies <- combined_shifts_dummies %>% select(-contains("Goalie"))
  combined_shifts_dummies <- combined_shifts_dummies %>% select(-contains("Missing"))
  
  return(combined_shifts_dummies)
  
  
}

train_rapm <- function(combined_shifts_dummies){
  
  # innput is output from create_shift_dummies
  
  ######## CREATE RAPM MODEL #####################
  
  xGF60 <- as.numeric(c(combined_shifts_dummies$xGF_60))
  shift_length <- as.numeric(c(combined_shifts_dummies$shift_length))
  subsetted_dummies <- subset(combined_shifts_dummies, select = -c(shift_length, xGF_60))
  RAPM_xGF <- as.matrix(subsetted_dummies)
  RAPM_xGF[!is.finite(RAPM_xGF)] <- 0
  
  ### CONVERT MATRIX TO SPARSE - MAKES CROSS VALIDATION TEN TIMES FASTER!!! ###
  
  Sparse_RAPM_xGF <- Matrix(RAPM_xGF, sparse = TRUE)
  
  plan(multisession)
  
  Cross_Validated_Results <- cv.glmnet(
    x=Sparse_RAPM_xGF, y=xGF60, weights=shift_length, 
    alpha=0, nfolds=10, standardize=FALSE, parallel=TRUE
  )
  
  ### INSERT LAMBDA OBTAINED FROM CROSS VALIDATION INTO FULL RUN ###
  
  Run_RAPM <- glmnet(
    x=Sparse_RAPM_xGF, y=xGF60, weights=shift_length, 
    lambda = Cross_Validated_Results[["lambda.min"]], 
    alpha=0, nfolds=10, standardize=FALSE, parallel=TRUE
  )
  
  plan(NULL)
  
  ### OBTAIN COEFFICIENTS FROM THE REGRESSION ###
  
  RAPM_coefficients <- as.data.frame(as.matrix(coef(Run_RAPM)))
  
  ### CLEAN DATA AND MAKE IT PRESENTABLE ###
  
  Binded_Coefficients <- cbind(rownames(RAPM_coefficients), RAPM_coefficients) %>%
    rename(Player = `rownames(RAPM_coefficients)`, xGF_60 = s0)
  
  offense_RAPM <- Binded_Coefficients %>%
    filter(grepl("offense", Binded_Coefficients$Player))
  
  offense_RAPM$Player = str_replace_all(offense_RAPM$Player, "offense__", "")
  
  defense_RAPM <- Binded_Coefficients %>%
    filter(grepl("defense", Binded_Coefficients$Player)) %>%
    rename(xGA_60 = xGF_60)
  
  defense_RAPM$Player = str_replace_all(defense_RAPM$Player, "defense__", "")
  
  joined_RAPM <- inner_join(offense_RAPM, defense_RAPM, by="Player")
  
  joined_RAPM$xGPM_60 <- joined_RAPM$xGF_60 - joined_RAPM$xGA_60
  
  joined_rapm <- joined_RAPM %>%
    arrange(desc(xGPM_60))
  
  # return RAPM values and the combined shifts data to calculate ice time for players
  return(joined_rapm)
  
}
