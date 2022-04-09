library(dplyr)
library(purrr)
library(tibble)
library(readr)
library(tidyr)
library(rvest)
library(qdapRegex)
library(ggplot2)
library(forcats)


.get_clean_understat_json <- function(page_url, script_name) {
  main_url <- "https://understat.com/"
  page <-  tryCatch( xml2::read_html(page_url), error = function(e) NA)

  if(!is.na(page)) {
    # locate script tags
    clean_json <- page %>% rvest::html_nodes("script") %>% as.character()
    clean_json <- clean_json[grep(script_name, clean_json)] %>% stringi::stri_unescape_unicode()
    clean_json <- qdapRegex::rm_square(clean_json, extract = TRUE, include.markers = TRUE) %>% unlist() %>% stringr::str_subset("\\[\\]", negate = TRUE)

    out_df <- lapply(clean_json, jsonlite::fromJSON) %>% do.call("rbind", .)
    # some outputs don't come with the season present, so add it in if not
    if(!any(grepl("season", colnames(out_df)))) {
      season_element <- page %>% rvest::html_nodes(xpath = '//*[@name="season"]') %>%
        rvest::html_nodes("option")
      season_element <- season_element[grep("selected", season_element)]
      # season <- season_element %>% rvest::html_attr("value") %>% .[1] %>% as.numeric()
      season <- season_element %>% rvest::html_text()
      out_df <- cbind(season, out_df)
    }

  } else {
    out_df <- data.frame()
  }

  out_df <- do.call(data.frame, out_df)

  return(out_df)

}


understat_league_match_results <- function(league, season_start_year) {
  # .pkg_message("Scraping match results data for {league} {season_start_year} season. Please acknowledge understat.com as the data source")
  main_url <- "https://understat.com/"

  leagues <- c("EPL", "La liga", "Bundesliga", "Serie A", "Ligue 1", "RFPL")
  if(!league %in% leagues) stop("Check league name")

  if(league == "La liga") {
    league <- "La_liga"
  } else if (league == "Serie A") {
    league <- "Serie_A"
  } else if (league == "Ligue 1") {
    league <- "Ligue_1"
  }

  league_url <- paste0(main_url, "league/", league, "/", season_start_year)

  # to get available seasons:
  # league_page <- xml2::read_html(league_url)
  # avail_seasons <- league_page %>% rvest::html_nodes(xpath = '//*[@name="season"]') %>%
  #   rvest::html_nodes("option") %>% rvest::html_text() %>% gsub("/.*", "", .)

  match_results <- .get_clean_understat_json(page_url = league_url, script_name = "datesData") %>%
    dplyr::filter(.data$isResult)

  match_results <- cbind(league, match_results)

  match_results <- match_results %>%
    dplyr::rename(match_id=.data$id, home_id=.data$h.id, home_team=.data$h.title, home_abbr=.data$h.short_title, away_id=.data$a.id, away_team=.data$a.title, away_abbr=.data$a.short_title,
                  home_goals=.data$goals.h, away_goals=.data$goals.a, home_xG=.data$xG.h, away_xG=.data$xG.a,
                  forecast_win=.data$forecast.w, forecast_draw=.data$forecast.d, forecast_loss=.data$forecast.l)

  match_results <- match_results %>%
    dplyr::mutate_at(c("home_goals", "away_goals", "home_xG", "away_xG", "forecast_win", "forecast_draw", "forecast_loss"), as.numeric)


  return(match_results)
}




tidy_fun <- function(.data) {
  data <- .data %>% 
  pivot_longer(!team, names_to = "match", values_to = "count") %>% 
  mutate(match = as.numeric(match)) %>% 
  mutate(team3 = fct_inorder(team, ordered = TRUE))

  data
}

plot_fun <- function(.data) {
  
epl_plot <- ggplot(.data, aes(factor(match), team3, fill = count)) +
  geom_tile(color = "white",
            lwd = .5,
            linetype = 1,
             height = 0.4) +
  coord_fixed() +
  theme_minimal() +
  scale_x_discrete(position = "top") +
  xlab("Match") +
  ylab("") +
  theme(legend.position="none") +
  theme(text=element_text(family="mono")) +
  labs(caption = paste("Updated:", Sys.time(), Sys.timezone())) + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank()) 


epl_plot
}






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


current_table2 <- current_table %>% 
  pivot_longer(!team, names_to = "match", values_to = "count") %>% 
  mutate(match = as.numeric(match)) %>% 
  mutate(team3 = fct_inorder(team, ordered = TRUE))



current_table_simple <- current_table %>% 
  mutate_if(is.numeric, funs(case_when(
      . < 0 ~ -1,
      . == 0 ~ 0,
      . > 0 ~ 1
    )))



points <- current_table %>% 
  mutate_if(is.numeric, funs(case_when(
      . < 0 ~ -1,
      . == 0 ~ 0,
      . > 0 ~ 1
    ))) %>% 
  mutate_if(is.numeric, funs(case_when(
    . == -1 ~ 0,
    . == 0 ~ 1,
    . == 1 ~ 3
  ))) %>% 
   mutate(Total = rowSums(select_if(., is.numeric), na.rm = TRUE)) %>% 
  relocate(Total, .after = 1) %>% 
  arrange(desc(Total))







epl_plot1 <- tidy_fun(current_table_simple) %>% 
  plot_fun() +
  scale_y_discrete(limits = levels(fct_reorder(points$team, points$Total))) +
  scale_fill_gradient2(low = "#fa7645", mid = "#F5F5F5", high = "#a0bcd6", na.value = "white")


epl_plot2 <- tidy_fun(current_table_simple) %>% 
  plot_fun() +
  scale_y_discrete(limits = levels(fct_reorder(points$team, points$Total))) +
  scale_fill_gradient2(low = "#FFD0C0", mid = "#F5F5F5", high = "#A0BCD6", na.value = "white")


epl_plot3 <- tidy_fun(current_table_simple) %>% 
  plot_fun() +
  scale_y_discrete(limits = levels(fct_reorder(points$team, points$Total))) +
  scale_fill_gradient2(low = "#ef9a9a", mid = "#F5F5F5", high = "#A0BCD6", na.value = "white")

ggsave("graphs/heatmap1.png", plot = epl_plot1,
       width = 8, height = 6, units = "in")

ggsave("graphs/heatmap2.png", plot = epl_plot2,
       width = 8, height = 6, units = "in")
                    
ggsave("graphs/heatmap3.png", plot = epl_plot3,
       width = 8, height = 6, units = "in")
