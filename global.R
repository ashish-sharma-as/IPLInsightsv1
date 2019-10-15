## global.R ##

# Loading libraries
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(shiny)
library(tidyr)
library(DT)

# Set working directory
setwd('/Users/ashishsharma/Documents/DataScience/NYCDSA/R/Shiny/ShinyProject/IPLInsightsv1')

# ---~---~---~---~---~---~---~---~---~---~---~---~---~---~
# LOAD - Data Sources for Project Summary
datasourcesdf <- read.csv('DataSourcesPrjSummaryTab.csv')

# ---~---~---~---~---~---~---~---~---~---~---~---~---~---~
# Data Load - WRVP (Winner-RunnerUp-Venue-PlayerOfSeries)
  # This is being loaded mainly due to Player of series. Rest are computed from original datasets
wrvpPerYear <- read.csv('w_r_v_p_per_year.csv')

# ---~---~---~---~---~---~---~---~---~---~---~---~---~---~
# LOAD - Matches
matches <- read.csv(file = 'matches.csv', 
                    stringsAsFactors = F)

# mutate year as factors and add it to the matches DF
matches %>% 
  mutate(year = as.factor(season)) -> matches


# ---~---~---~---~---~---~---~---~---~---~---~---~---~---~
# CLEAN - Matches 
# Clean Supergiant to Supergiant"s" in team 1
matches[matches$team1 == 'Rising Pune Supergiant',5] <- 'Rising Pune Supergiants'
matches[matches$team2 == 'Rising Pune Supergiant',6] <- 'Rising Pune Supergiants'
matches[matches$winner == 'Rising Pune Supergiant',11] <- 'Rising Pune Supergiants'

# Team1 short name
matches %>% 
  mutate(team1shortname = case_when(
    team1 == 'Royal Challengers Bangalore'  ~ "RCB",
    team1 == 'Kolkata Knight Riders' ~ 'KKR',
    team1 == 'Mumbai Indians' ~ 'MI',
    team1 == 'Rajasthan Royals' ~ 'RR',
    team1 == 'Kings XI Punjab' ~ 'KXIP',
    team1 == 'Delhi Daredevils' ~ 'DD',
    team1 == 'Deccan Chargers' ~ 'DC',
    team1 == 'Chennai Super Kings' ~ 'CSK',
    team1 == 'Pune Warriors' ~ 'PW',
    team1 == 'Kochi Tuskers Kerala' ~ 'KTK',
    team1 == 'Sunrisers Hyderabad' ~ 'SRH',
    team1 == 'Rising Pune Supergiants' ~ 'RPS',
    team1 == 'Gujarat Lions' ~ 'GL',
  )) -> matches

# Team2 short name
matches %>% 
  mutate(team2shortname = case_when(
    team2 == 'Royal Challengers Bangalore'  ~ "RCB",
    team2 == 'Kolkata Knight Riders' ~ 'KKR',
    team2 == 'Mumbai Indians' ~ 'MI',
    team2 == 'Rajasthan Royals' ~ 'RR',
    team2 == 'Kings XI Punjab' ~ 'KXIP',
    team2 == 'Delhi Daredevils' ~ 'DD',
    team2 == 'Deccan Chargers' ~ 'DC',
    team2 == 'Chennai Super Kings' ~ 'CSK',
    team2 == 'Pune Warriors' ~ 'PW',
    team2 == 'Kochi Tuskers Kerala' ~ 'KTK',
    team2 == 'Sunrisers Hyderabad' ~ 'SRH',
    team2 == 'Rising Pune Supergiants' ~ 'RPS',
    team2 == 'Gujarat Lions' ~ 'GL',
  )) -> matches


# Winner short name
matches %>% 
  mutate(winnershortname = case_when(
    winner == 'Royal Challengers Bangalore'  ~ "RCB",
    winner == 'Kolkata Knight Riders' ~ 'KKR',
    winner == 'Mumbai Indians' ~ 'MI',
    winner == 'Rajasthan Royals' ~ 'RR',
    winner == 'Kings XI Punjab' ~ 'KXIP',
    winner == 'Delhi Daredevils' ~ 'DD',
    winner == 'Deccan Chargers' ~ 'DC',
    winner == 'Chennai Super Kings' ~ 'CSK',
    winner == 'Pune Warriors' ~ 'PW',
    winner == 'Kochi Tuskers Kerala' ~ 'KTK',
    winner == 'Sunrisers Hyderabad' ~ 'SRH',
    winner == 'Rising Pune Supergiants' ~ 'RPS',
    winner == 'Gujarat Lions' ~ 'GL',
  )) -> matches


# keep this query handy. change Team1 as per need of query
matches %>% 
  distinct(team1, team1shortname)


# Data for Table of winners, runnerups, venue per year
inner_join(matches, wrvpPerYear, by='season') %>% 
  group_by(season) %>% 
  filter(id == max(id)) %>% 
  arrange(year) %>% 
  mutate(runnerup = ifelse(team1 == winner, team2shortname, team1shortname)) %>% 
  select(Year = season, Winner = winnershortname, 'Runner Up' = runnerup, 'Man of the match'=player_of_match, 'Player of the series'=player_of_the_series, Venue=city) -> dfWinRunVenuePl


# ---~---~---~---~---~---~---~---~---~---~---~---~---~---~
# Data Load - Archive (Winner-RunnerUp-Top4-RoundRobin)
dfarchive <- read.csv('archive.csv',check.names = F, stringsAsFactors = F)

# ---~---~---~---~---~---~---~---~---~---~---~---~---~---~
# Data Load - Deliveries

# LOAD - Deliveries
deliveries <- read.csv(file = 'deliveries.csv', 
                    stringsAsFactors = F)





