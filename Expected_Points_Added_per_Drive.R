# import necessary libraries and data
library(tidyverse)
library(nflfastR)
library(nflplotR)
library(ggplot2)
library(ggimage)
library(bestNormalize)
library(PerformanceAnalytics)

pbp <- nflfastR::load_pbp(2011:2022)


### Create EPA per Drive

# Modify the EPA so the sums will equal the point differential
EPApD_plays <- pbp %>%
  # Remove the unnecessary 'plays'
  filter(!is.na(posteam),!is.na(ep),quarter_end==0) %>%
  
  # Group all of the plays in each half
  group_by(game_id,game_half) %>%
  
  dplyr::mutate(
    # If the EPA is NA, it is because there is no EP for after the play even though that EP should just be 0
    # therefore, the EPA should equal 0 minus the EP before the play
    epa = if_else(is.na(epa), -ep, epa),
    
    # Make sure that non-attempts are FALSE, not NA
    kickoff_attempt = if_else(is.na(kickoff_attempt),FALSE,kickoff_attempt==1),
    two_point_attempt = if_else(is.na(two_point_attempt),FALSE,two_point_attempt==1),
    extra_point_attempt = if_else(is.na(extra_point_attempt),FALSE,extra_point_attempt==1),
    
    # Adjust attempts so that they is based on the unit on the field, not the action taken
    true_field_goal_attempt = if_else(str_detect(desc,"formation"), if_else(str_detect(desc, "Field Goal"),TRUE,FALSE), if_else(str_detect(desc, "field goal"),TRUE,FALSE)),
    true_punt_attempt = if_else(str_detect(desc,"formation"), if_else(str_detect(desc, "Punt"),TRUE,FALSE), if_else(str_detect(desc, "punt"),TRUE,FALSE))&!shotgun&!true_field_goal_attempt, #unless shotgun
    true_kickoff_attempt = if_else(true_punt_attempt,FALSE,kickoff_attempt),
    true_two_point_attempt = (two_point_attempt)&!str_detect(desc,"Kick"),
    true_extra_point_attempt = extra_point_attempt|((two_point_attempt)&str_detect(desc,"Kick")),
    true_special_teams = true_punt_attempt|true_field_goal_attempt|true_extra_point_attempt|true_kickoff_attempt,
    true_after_touchdown = true_extra_point_attempt|true_two_point_attempt,
    
    # Get the EP of future plays in order to adjust scoring drives
    lead1ep = if_else(is.na(lead(ep,1)),0,if_else(lead(game_half,1)!=game_half,0,lead(ep,1))),
    lead2ep = if_else(is.na(lead(ep,2)),0,if_else(lead(game_half,2)!=game_half,0,lead(ep,2))),
    lead3ep = if_else(is.na(lead(ep,3)),0,if_else(lead(game_half,3)!=game_half,0,lead(ep,3))),
    lead4ep = if_else(is.na(lead(ep,4)),0,if_else(lead(game_half,4)!=game_half,0,lead(ep,4))),
    
    # If a field goal is made, don't credit the full value to the Offense, the kicker could have missed it
    field_goal_adj = if_else(is.na(field_goal_result),0,if_else(field_goal_result=='made',-lead1ep,0)),
    
    # Fix the rare case that a safety is scored against the defending team
    safety_adj = if_else(is.na(safety),0,(if_else(safety==TRUE,if_else(((score_differential_post - score_differential) > 0),lead1ep,-lead1ep),0))), 
    
    # The Offense must subtract the value of the next kickoff from their contribution on scoring drives
    kickoff_adj = if_else(lead(true_kickoff_attempt,2)==1,lead2ep,
                          if_else(lead(true_kickoff_attempt,3)==1,lead3ep,
                                  if_else(lead(true_kickoff_attempt,4)==1,lead4ep,0))),
    kickoff_adj = if_else(is.na(kickoff_adj),0,kickoff_adj),
    
    # If an extra point is made, don't credit the full value to the Offense, the kicker could have missed it
    touchdown_adj = if_else(is.na(touchdown),0,if_else(touchdown==TRUE,(lead1ep - 1 - kickoff_adj)*if_else(td_team==posteam,1,-1),0))
  )

# Attribute total EPA per half to each unit
EPApD_half <- EPApD_plays %>%
  group_by(game_id,game_half) %>%
  dplyr::summarise(
    # Import relevant information
    season = first(season),
    week = first(week),
    home_team = first(home_team),
    away_team = first(away_team),
    num_drives_per_side = last(fixed_drive)/2,
    roof = first(roof),
    surface = first(surface),
    is_playoffs = (first(season_type)=="POST"),
    score_dif = last(score_differential_post)*if_else(last(posteam)==home_team,1,-1) - if_else(is.na(first(score_differential)),0,first(score_differential))*if_else(first(posteam)==home_team,1,-1),
    
    # Distribute EPA and the adjustments to each unit
    home_off =  sum(((epa + touchdown_adj + safety_adj)*(true_special_teams==FALSE) + field_goal_adj)*(posteam==home_team)) - first(ep)*(first(posteam)!=home_team) - last(ep+epa)*(last(posteam)==home_team)*(last(sp)==0),
    home_def = -sum(((epa + touchdown_adj + safety_adj)*(true_special_teams==FALSE) + field_goal_adj)*(posteam!=home_team)) + first(ep)*(first(posteam)==home_team) + last(ep+epa)*(last(posteam)!=home_team)*(last(sp)==0),
    
    home_kickoff_rec =   sum((epa + touchdown_adj + safety_adj)*(true_kickoff_attempt==TRUE)*(posteam==home_team)),
    home_kickoff_kick = -sum((epa + touchdown_adj + safety_adj)*(true_kickoff_attempt==TRUE)*(posteam!=home_team)),
    
    home_punt_kick = sum((epa + touchdown_adj + safety_adj)*(true_punt_attempt==TRUE)*(posteam==home_team)),
    home_punt_rec = -sum((epa + touchdown_adj + safety_adj)*(true_punt_attempt==TRUE)*(posteam!=home_team)),
    
    home_FG_XP_kick = sum((epa + touchdown_adj)*(true_field_goal_attempt==TRUE|true_extra_point_attempt==TRUE)*(posteam==home_team)),
    home_FG_XP_def = -sum((epa + touchdown_adj)*(true_field_goal_attempt==TRUE|true_extra_point_attempt==TRUE)*(posteam!=home_team)),
  )

# Adjust EPA per drive for Offense and Defense
adjustment = (sum(EPApD_half$home_off) - sum(EPApD_half$home_def))/sum(EPApD_half$num_drives_per_side)/2

# Aggregate each game's halves
EPApD_game <- EPApD_half %>%
  group_by(game_id) %>%
  dplyr::summarise(
    # Import relevant information
    season = first(season),
    week = first(week),
    home_team = first(home_team),
    away_team = first(away_team),
    roof = first(roof),
    surface = first(surface),
    is_playoffs = first(is_playoffs),
    
    score_dif = sum(score_dif),
    num_drives_per_side = sum(num_drives_per_side),
    
    # Normalize the units' EPA by the number of drives
    home_off = sum(home_off)/num_drives_per_side - adjustment,
    home_def = sum(home_def)/num_drives_per_side + adjustment,
    
    home_kickoff_rec = sum(home_kickoff_rec)/num_drives_per_side,
    home_kickoff_kick = sum(home_kickoff_kick)/num_drives_per_side,
    
    home_punt_kick = sum(home_punt_kick)/num_drives_per_side,
    home_punt_rec = sum(home_punt_rec)/num_drives_per_side,
    
    home_FG_XP_kick = sum(home_FG_XP_kick)/num_drives_per_side,
    home_FG_XP_def = sum(home_FG_XP_def)/num_drives_per_side,
    
    # Test if the contributions add up to the score differential
    total = home_off+home_def + home_kickoff_rec+home_kickoff_kick + 
                  home_punt_kick+home_punt_rec + home_FG_XP_kick+home_FG_XP_def,
    dif = abs(score_dif-total*num_drives_per_side),
    same = dif < .00001,
    same = if_else(is.na(same),FALSE,same)  
    )

# Check how many halves don't meet the above test (output should be 0)
sum(!EPApD_game$same)

# Save the data if you want
#write.csv(EPApD_game,"Expected Points Added per Drive.csv")


### Calculate EPA per Drive for each season
EPApD_seasons_home <- EPApD_game %>%
  filter(!is_playoffs) %>%
  group_by(season,home_team) %>%
  dplyr::summarise(num_games_home = n(),
                   home_off = mean(home_off),
                   home_def = mean(home_def),
                   home_kickoff_rec = mean(home_kickoff_rec),
                   home_kickoff_kick = mean(home_kickoff_kick),
                   home_punt_kick = mean(home_punt_kick),
                   home_punt_rec = mean(home_punt_rec),
                   home_FG_XP_kick = mean(home_FG_XP_kick),
                   home_FG_XP_def = mean(home_FG_XP_def)
  )

EPApD_seasons_away <- EPApD_game %>%
  filter(!is_playoffs) %>%
  group_by(season,away_team) %>%
  dplyr::summarise(num_games_away = n(),
                   away_off = -mean(home_def),
                   away_def = -mean(home_off),
                   away_kickoff_rec = -mean(home_kickoff_kick),
                   away_kickoff_kick = -mean(home_kickoff_rec),
                   away_punt_kick = -mean(home_punt_rec),
                   away_punt_rec = -mean(home_punt_kick),
                   away_FG_XP_kick = -mean(home_FG_XP_def),
                   away_FG_XP_def = -mean(home_FG_XP_kick)
  )

EPApD_seasons <- full_join(EPApD_seasons_home, EPApD_seasons_away, by = c("season","home_team" = "away_team")) 
EPApD_seasons[is.na(EPApD_seasons)] <- 0
EPApD_seasons <- EPApD_seasons %>%
  rename("team"="home_team") %>%
  dplyr::mutate(
    num_games_home = ifelse(is.na(num_games_home),0,num_games_home),
    num_games_away = ifelse(is.na(num_games_away),0,num_games_away),
    num_games = num_games_home + num_games_away,
    off = (home_off*num_games_home + away_off*num_games_away)/num_games,
    def = (home_def*num_games_home + away_def*num_games_away)/num_games,
    kickoff_rec = (home_kickoff_rec*num_games_home + away_kickoff_rec*num_games_away)/num_games,
    kickoff_kick = (home_kickoff_kick*num_games_home + away_kickoff_kick*num_games_away)/num_games,
    punt_kick = (home_punt_kick*num_games_home + away_punt_kick*num_games_away)/num_games,
    punt_rec = (home_punt_rec*num_games_home + away_punt_rec*num_games_away)/num_games,
    FG_XP_kick = (home_FG_XP_kick*num_games_home + away_FG_XP_kick*num_games_away)/num_games,
    FG_XP_def = (home_FG_XP_def*num_games_home + away_FG_XP_def*num_games_away)/num_games,
    special_teams_possessing = kickoff_rec + punt_kick + FG_XP_kick,
    special_teams_defending = kickoff_kick + punt_rec + FG_XP_def)


### Graph EPA per Drive for 2021

# Change this for different seasons
SEASON = 2021
EPApD_season <-  filter(EPApD_seasons, season==SEASON)


# Offense and Defense
x_min = min(EPApD_season$off)
y_min = min(EPApD_season$def)
x_range = max(EPApD_season$off) - x_min
y_range = max(EPApD_season$def) - y_min
x_center = x_min + x_range/2
y_center = y_min + y_range/2
range = if_else(x_range>y_range,x_range,y_range)*1.001

EPApD_season %>%
  ggplot(aes(x = off, y = def)) +
  scale_x_continuous(breaks = seq(-2, 2, by = 0.4), lim = c(x_center-range/2, x_center+range/2)) +
  scale_y_continuous(breaks = seq(-2, 2, by = 0.4), lim = c(y_center-range/2, y_center+range/2)) +
  geom_hline(yintercept=0, alpha = .5) +
  geom_vline(xintercept=0, alpha = .5) +
  coord_fixed() +
  geom_nfl_logos(aes(team_abbr = team, width = 0.06)) +
  labs(x = "Offensive EPA per Drive",
       y = "Defensive EPA per Drive",
       caption = "Data from nflscrapR",
       title = "Offensive and Defensive EPA per Drive",
       subtitle = paste(as.character(SEASON), " Season", sep = " ")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 8),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(size = 8)) +
  theme(plot.background = element_rect(fill = "#e8e8e8", color = "#e8e8e8"))

# Save the plot if you want
#ggsave(paste("Off_Def_EPApD_",as.character(SEASON),".png", sep = ""))


# Kickoffs
x_min = min(EPApD_season$kickoff_rec)
y_min = min(EPApD_season$kickoff_kick)
x_range = max(EPApD_season$kickoff_rec) - x_min
y_range = max(EPApD_season$kickoff_kick) - y_min
x_center = x_min + x_range/2
y_center = y_min + y_range/2
range = if_else(x_range>y_range,x_range,y_range)*1.001

EPApD_season %>%
  ggplot(aes(x = kickoff_rec, y = kickoff_kick)) +
  scale_x_continuous(breaks = seq(-.2, .2, by = 0.05), lim = c(x_center-range/2, x_center+range/2)) +
  scale_y_continuous(breaks = seq(-.2, .2, by = 0.05), lim = c(y_center-range/2, y_center+range/2)) +
  geom_hline(yintercept=0, alpha = .5) +
  geom_vline(xintercept=0, alpha = .5) +
  coord_fixed() +
  geom_nfl_logos(aes(team_abbr = team, width = 0.08)) +
  labs(x = "Receiving EPA per Drive",
       y = "Kicking EPA per Drive",
       caption = "Data from nflscrapR",
       title = "Kickoff EPA per Drive",
       subtitle = paste(as.character(SEASON), " Season", sep = " ")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 8),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(size = 8)) +
  theme(plot.background = element_rect(fill = "#e8e8e8", color = "#e8e8e8"))

# Save the plot if you want
#ggsave(paste("Kickoff_EPApD_",as.character(SEASON),".png", sep = ""))


# Punts
x_min = min(EPApD_season$punt_kick)
y_min = min(EPApD_season$punt_rec)
x_range = max(EPApD_season$punt_kick) - x_min
y_range = max(EPApD_season$punt_rec) - y_min
x_center = x_min + x_range/2
y_center = y_min + y_range/2
range = if_else(x_range>y_range,x_range,y_range)*1.001

EPApD_season %>%
  ggplot(aes(x = punt_kick, y = punt_rec)) +
  scale_x_continuous(breaks = seq(-.2, .2, by = 0.05), lim = c(x_center-range/2, x_center+range/2)) +
  scale_y_continuous(breaks = seq(-.2, .2, by = 0.05), lim = c(y_center-range/2, y_center+range/2)) +
  geom_hline(yintercept=0, alpha = .5) +
  geom_vline(xintercept=0, alpha = .5) +
  coord_fixed() +
  geom_nfl_logos(aes(team_abbr = team, width = 0.08)) +
  labs(x = "Kicking EPA per Drive",
       y = "Receiving EPA per Drive",
       caption = "Data from nflscrapR",
       title = "Punt EPA per Drive",
       subtitle = paste(as.character(SEASON), " Season", sep = " ")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 8),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(size = 8)) +
  theme(plot.background = element_rect(fill = "#e8e8e8", color = "#e8e8e8"))

# Save the plot if you want
#ggsave(paste("Punt_EPApD_",as.character(SEASON),".png", sep = ""))


# Field Goals and Extra Points
x_min = min(EPApD_season$FG_XP_kick)
y_min = min(EPApD_season$FG_XP_def)
x_range = max(EPApD_season$FG_XP_kick) - x_min
y_range = max(EPApD_season$FG_XP_def) - y_min
x_center = x_min + x_range/2
y_center = y_min + y_range/2
range = if_else(x_range>y_range,x_range,y_range)*1.001
  
EPApD_season %>%
  ggplot(aes(x = FG_XP_kick, y = FG_XP_def)) +
  scale_x_continuous(breaks = seq(-.2, .2, by = 0.05), lim = c(x_center-range/2, x_center+range/2)) +
  scale_y_continuous(breaks = seq(-.2, .2, by = 0.05), lim = c(y_center-range/2, y_center+range/2)) +
  geom_hline(yintercept=0, alpha = .5) +
  geom_vline(xintercept=0, alpha = .5) +
  coord_fixed() +
  geom_nfl_logos(aes(team_abbr = team, width = 0.08)) +
  labs(x = "Kicking EPA per Drive",
       y = "Defending EPA per Drive",
       caption = "Data from nflscrapR",
       title = "Field Goal/Extra Point EPA per Drive",
       subtitle = paste(as.character(SEASON), " Season", sep = " ")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) + 
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 8),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(size = 8)) +
  theme(plot.background = element_rect(fill = "#e8e8e8", color = "#e8e8e8"))

# Save the plot if you want
#ggsave(paste("FG_XP_EPApD_",as.character(SEASON),".png", sep = ""))


# Show overall EPA per Drive by shifting the Off/Def points by the total of the special teams contributions
width = 0.005
plot_data <-as.data.frame(matrix(nrow=4*nrow(EPApD_season),ncol=5))
for (i in 1:nrow(EPApD_season)) {
  shift = (EPApD_season$special_teams_possessing[i] + EPApD_season$special_teams_defending[i])*2^(-.5)
  off = EPApD_season$off[i]
  def = EPApD_season$def[i]
  team = EPApD_season$team[i]
  season = EPApD_season$season[i]
  
  # Main point
  plot_data[4*i-3,1:4] = c(off+shift,def+shift,0,season)
  
  # Triangle to show shift
  plot_data[4*i-2,1:4] = c(off,def,i,season)
  plot_data[4*i-1,1:4] = c(off+shift+width,def+shift-width,i,season)
  plot_data[4*i,1:4] = c(off+shift-width,def+shift+width,i,season)
  
  # must add team strings separately since they are a different data type
  plot_data[4*i-3,5] = team
  plot_data[4*i-2,5] = team
  plot_data[4*i-1,5] = team
  plot_data[4*i,5] = team
}
colnames(plot_data) = c("x","y","id","season","team")

x_min = min(filter(plot_data,id==0)$x)
y_min = min(filter(plot_data,id==0)$y)
x_range = max(filter(plot_data,id==0)$x) - x_min
y_range = max(filter(plot_data,id==0)$y) - y_min
x_center = x_min + x_range/2
y_center = y_min + y_range/2
range = if_else(x_range>y_range,x_range,y_range)*1.001

plot_data %>%
  ggplot(aes(x = x, y = y)) +
  scale_x_continuous(breaks = seq(-2, 2, by = 0.4), lim = c(x_center-range/2, x_center+range/2)) +
  scale_y_continuous(breaks = seq(-2, 2, by = 0.4), lim = c(y_center-range/2, y_center+range/2)) +
  geom_abline(slope = -1, intercept = seq(2, -2, -.4), alpha = .5) +
  coord_fixed() +
  geom_polygon(data = filter(plot_data,id!=0), aes(group = id)) +
  geom_nfl_logos(data = filter(plot_data,id==0), aes(team_abbr = team, width = 0.06)) +
  labs(x = "Offense EPA per Drive",
       y = "Defense EPA per Drive",
       caption = "\nComet tail shows only offense and defense, comet head adds special teams.       Data from nflscrapR",
       title = "Team EPA per Drive",
       subtitle = paste(as.character(SEASON), " Season", sep = " ")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 9),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(size = 9)) +
  theme(plot.background = element_rect(fill = "#e8e8e8", color = "#e8e8e8"))

# Save the plot if you want
#ggsave(paste("Off_Def_Adj_EPApD_",as.character(SEASON),".png", sep = ""))


