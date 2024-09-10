library(nflfastR)
library(RPostgres)
library(dplyr)
library(tidyverse)
library(nflreadr)
library(gganimate)
library(ggplot2)
library(gt)
library(gtExtras)
library(tidyr)
library(patchwork)
library(magrittr)
library(reactable)




pbp_data<-load_pbp()
teams<-load_teams()


pbp_data$play_type[pbp_data$play_type == "no_play"] <- "penalty"



drive_data<-pbp_data%>%
  filter(( !is.na(drive)))%>%left_join(teams_colors_logos,by=c("posteam"="team_abbr"))%>%
  mutate(drive_time_of_possession_seconds = sapply(drive_time_of_possession, convert_to_seconds),
         desc = sub("\\([^\\)]+\\) \\([^\\)]+\\) ", "", desc))


game_data<-pbp_data%>% filter(!is.na(drive))%>%
  left_join(teams_colors_logos,by=c("posteam"="team_abbr"))%>%
  mutate(drive_time_of_possession_seconds = sapply(drive_time_of_possession, convert_to_seconds),
         penalty_yards = ifelse(penalty_team == posteam,-penalty_yards,penalty_yards),
         run_yards = ifelse(play_type=='run',yards_gained,0),
         pass_yards = ifelse(play_type=='pass',yards_gained,0),
         kickoff_epa = ifelse(play_type=='kickoff',yards_gained,0),
         run_epa = ifelse(play_type=='run',epa,0),
         pass_epa = ifelse(play_type=='pass',epa,0),
         punt_epa = ifelse(play_type=='punt',epa,0),
         penalty_epa = ifelse(play_type=='penalty',epa,0),
         ep_epa = ifelse(play_type=='extra_point',epa,0),
         fg_epa = ifelse(play_type=='field_goal',epa,0),
         qb_kneel_epa = ifelse(play_type=='qb_kneel',epa,0),
         qb_spike_epa = ifelse(play_type=='qb_spike',epa,0),
         total_epa = qb_spike_epa + qb_kneel_epa + pass_epa + run_epa +fg_epa + ep_epa + kickoff_epa + punt_epa + penalty_epa)

game_summary<-game_data%>%
  select(game_id,
         season,
         week,
         posteam,
         drive,
         home_team,
         away_team,
         yards_gained,
         run_yards,
         pass_yards,
         air_yards,
         yards_after_catch,
         penalty_yards,
         posteam_score_post,
         drive_play_count,
         epa,
         rush_attempt,
         pass_attempt,
         team_wordmark,
         team_logo_espn,
         play_type,
         qb_spike_epa,
         qb_kneel_epa,
         pass_epa,
         run_epa,
         fg_epa,
         ep_epa,
         punt_epa,
         kickoff_epa,
         penalty_epa,
         total_epa)%>%
  group_by(
           season,
           week,
           game_id,
           posteam,
           home_team,
           away_team,
           team_wordmark,
           team_logo_espn)%>%
  mutate(total_plays= n(),
         total_drives = n_distinct(drive),
         posteam_score_post = max(posteam_score_post),
         yards_gained = sum(yards_gained,na.rm = TRUE),
         pass_yds = sum(pass_yards, na.rm = TRUE),
         run_yds = sum(run_yards, na.rm = TRUE),
         penalty_yards = sum(penalty_yards,na.rm = TRUE),
         air_yards = sum(air_yards,na.rm = TRUE),
         yac = sum(yards_after_catch,na.rm = TRUE),
         rush_attempts = sum(rush_attempt,na.rm = TRUE),
         pass_attempts = sum(pass_attempt,na.rm = TRUE),
         pass_success_ct = sum(epa > 0 & play_type == "pass", na.rm = TRUE),
         rush_success_ct = sum(epa > 0 & play_type == "run", na.rm = TRUE),
         impact_run = sum(run_yards>=10, na.rm = TRUE),
         impact_pass = sum(pass_yards>=10 & play_type == "pass", na.rm = TRUE), 
         game_epa = round(sum(total_epa,na.rm = TRUE),1),
         epa_per_play = round(game_epa/total_plays,2),
         pass_epa = round(sum(epa[play_type == "pass"], na.rm = TRUE),2),
         run_epa = round(mean(epa[play_type == "run"], na.rm = TRUE),2),
         pass_rate = round(pass_attempts/total_plays,3),
         pass_success_rate = round(pass_success_ct/pass_attempts,3),
         run_success_rate = round(rush_success_ct/rush_attempts,3))%>%mutate(pass_epa = replace(pass_epa,is.na(pass_epa),0),
                                                                             run_epa = replace(run_epa,is.na(run_epa),0),
                                                                             pts_per_drive = posteam_score_post/total_drives)%>%select(-play_type,
                                                                                                                                       -rush_attempt,
                                                                                                                                       -pass_attempt,
                                                                                                                                       -yards_after_catch,
                                                                                                                                       -epa,
                                                                                                                                       -run_yards,
                                                                                                                                       -pass_yards,
                                                                                                                                       -home_team,
                                                                                                                                       -away_team,
                                                                                                                                       -drive_play_count,
                                                                                                                                       -qb_spike_epa,
                                                                                                                                       -qb_kneel_epa,
                                                                                                                                       -pass_epa,
                                                                                                                                       -run_epa,
                                                                                                                                       -fg_epa,
                                                                                                                                       -ep_epa,
                                                                                                                                       -punt_epa,
                                                                                                                                       -kickoff_epa,
                                                                                                                                       -penalty_epa,
                                                                                                                                       -total_epa,
                                                                                                                                       -drive)%>%ungroup()%>%distinct()%>%filter(posteam != 'NA')

game_summary%>%View()


drive_summaries<-game_data%>%
  select(game_id,
         season,
         week,
         posteam,
         home_team,
         away_team,
         drive,
         qtr,
         yards_gained,
         run_yards,
         pass_yards,
         air_yards,
         yards_after_catch,
         penalty_yards,
         posteam_score_post,
         defteam_score_post,
         epa,
         wp,
         home_wp_post,
         total_epa,
         away_wp_post,
         rush_attempt,
         pass_attempt,
         fixed_drive_result,
         drive_play_count,
         drive_time_of_possession,
         drive_time_of_possession_seconds,
         drive_first_downs,
         drive_start_yard_line,
         drive_end_yard_line,
         drive_ended_with_score,
         drive_yards_penalized,
         drive_start_transition,
         drive_end_transition,
         team_wordmark,
         team_logo_espn,
         play_type)%>%
  group_by(season,
           week,
          game_id,
          posteam,
           home_team,
           away_team,
           drive,
           fixed_drive_result,
           drive_play_count,
           drive_time_of_possession,
           drive_time_of_possession_seconds,
           drive_first_downs,
           drive_start_yard_line,
           drive_end_yard_line,
           drive_ended_with_score,
           drive_yards_penalized,
           drive_start_transition,
           drive_end_transition,
           team_wordmark,
           team_logo_espn)%>%
  mutate(posteam_score_post = max(posteam_score_post),
         defteam_score_post = max(posteam_score_post),
         yards_gained = sum(yards_gained,na.rm = TRUE),
         pass_yds = sum(pass_yards, na.rm = TRUE),
         run_yds = sum(run_yards, na.rm = TRUE),
         penalty_yards = sum(penalty_yards,na.rm = TRUE),
         air_yards = sum(air_yards,na.rm = TRUE),
         yac = sum(yards_after_catch,na.rm = TRUE),
         rush_attempts = sum(rush_attempt,na.rm = TRUE),
         pass_attempts = sum(pass_attempt,na.rm = TRUE),
         pass_success_ct = sum(epa > 0 & play_type == "pass"),
         rush_success_ct = sum(epa > 0 & play_type == "run"),
         impact_run = sum(run_yards>=10, na.rm = TRUE),
         impact_pass = sum(pass_yards>=10 & play_type == "pass", na.rm = TRUE), 
         drive_epa = round(sum(total_epa,na.rm = TRUE),1),
         epa_per_play = round(drive_epa/drive_play_count,2),
         pass_epa = round(sum(epa[play_type == "pass"], na.rm = TRUE),2),
         run_epa = round(mean(epa[play_type == "run"], na.rm = TRUE),2),
         pass_rate = round(pass_attempts/drive_play_count,3),
         pass_success_rate = round(pass_success_ct/pass_attempts,3),
         run_success_rate = round(rush_success_ct/rush_attempts,3),
         wp_start = round(first(wp),3),
         wp_end = ifelse(home_team==posteam,round(last(home_wp_post),3),round(last(away_wp_post),3)),
         pace_of_play = round(drive_time_of_possession_seconds/drive_play_count,1))%>%mutate(pass_epa = replace(pass_epa,is.na(pass_epa),0),
                                                                                             run_epa = replace(run_epa,is.na(run_epa),0),
                                                                                             wp_difference = round(wp_end - wp_start ,3),
                                                                                             qtr = min(qtr))%>%select(-play_type,
                                                                                                                      -rush_attempt,
                                                                                                                      -pass_attempt,
                                                                                                                      -yards_after_catch,
                                                                                                                      -epa,
                                                                                                                      -wp,
                                                                                                                      -total_epa,
                                                                                                                      -run_yards,
                                                                                                                      -pass_yards,
                                                                                                                      -away_wp_post,
                                                                                                                      -home_wp_post,
                                                                                                                      -home_team,
                                                                                                                      -away_team)%>%ungroup()%>%distinct()%>%filter(posteam != 'NA')


drive_summaries%>%View()

drive_summaries <- drive_summaries %>%
group_by(season, week, game_id,posteam) %>%
mutate(posteam_drive_num = dense_rank(drive)) %>%
ungroup()



















rush_pass_att<-drive_summaries%>%select(drive,rush_attempts,pass_attempts,game_id,posteam,season,week)%>%pivot_longer(cols = c(rush_attempts,pass_attempts))%>%group_by(game_id,
                                                                                                                                            season,
                                                                                                                                            week,posteam,drive) %>%
  summarize(list_data = list(value))

rush_pass_yds<-drive_summaries%>%select(drive,run_yds,pass_yds,game_id,season,posteam,week)%>%pivot_longer(cols = c(run_yds,pass_yds))%>%group_by(game_id,
                                                                                                                      season,
                                                                                                                      week,posteam,drive) %>%
  summarize(list_data2 = list(value))

drive_summaries<-drive_summaries%>%left_join(rush_pass_att,by=c('game_id','season','week','posteam','drive'))%>%left_join(rush_pass_yds,by=c('game_id','season','week','posteam','drive'))




#drive_summaries<-drive_summaries%>%mutate(drive_num = row_number())

drive_summaries%>%View()