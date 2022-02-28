

library(worldfootballR)

library(dplyr)
library(purrr)
library(tibble)
library(readr)

epl_results <- understat_league_match_results(league = "EPL", season_start_year = 2021)

oneline_result <- function(.data, team_abbr) {
  
  home <- .data %>%  
    select(home_abbr, away_abbr, home_goals, away_goals) %>% 
    filter(home_abbr == team_abbr) %>% 
    mutate(goal_diff = home_goals-away_goals) %>% 
    mutate(team = team_abbr) %>% 
    relocate(team) %>% 
    select(team, goal_diff)
  
  away <- .data %>% 
    select(home_abbr, away_abbr, home_goals, away_goals) %>% 
    filter(away_abbr == team_abbr) %>% 
    mutate(goal_diff = away_goals-home_goals) %>% 
    mutate(team = team_abbr) %>% 
    relocate(team) %>% 
    select(team, goal_diff)
  
  results <- bind_rows(home, away) %>% 
    rownames_to_column("match_num") %>% 
    mutate(match_num = as.numeric(match_num)) %>% 
    arrange(match_num) %>% 
    rowid_to_column("match_order")
  
  oneline <- results %>% 
    select(match_order, goal_diff, team) %>% 
    pivot_wider(names_from = match_order, values_from = goal_diff)
  
  oneline
  
}


team_list <- epl_results %>% distinct(home_abbr)

team_list <- team_list %>% unname() %>% unlist()

current_table <- map_dfr(team_list, oneline_result, .data = epl_results) %>% arrange(team)

write_csv(epl_results, "data/epl_results.csv")
write_csv(current_table, "data/current_table.csv")
