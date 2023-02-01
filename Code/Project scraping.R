# R Scraping

library(xml2)
library(rvest)
library(tidyverse)
library(rjson)
library(dplyr)

# Code reads in the last 10 years of data for major league athletes
mlb_batters_dfs = list()
mlb_pitchers_dfs = list()
for(year in c(2012:2021)) {
  print(year)
  mlb_batters_html = glue::glue("https://www.baseball-reference.com/leagues/majors/{year}-standard-batting.shtml")
  mlb_pitchers_html = glue::glue("https://www.baseball-reference.com/leagues/majors/{year}-standard-pitching.shtml")
    
  mlb_batters_list = rvest::read_html(mlb_batters_html)
  mlb_pitchers_list = rvest::read_html(mlb_pitchers_html)

  
  mlb_batters_df = mlb_batters_list %>% html_nodes(xpath = '//comment()') %>%    # select comment nodes
    html_text() %>%    # extract comment text
    paste(collapse = '') %>%   # collapse to a single string 
    read_html() %>%    # reparse to HTML
    html_node('table#players_standard_batting')  %>%
    html_table()   # parse table
  
  
  mlb_pitchers_df = mlb_pitchers_list %>% html_nodes(xpath = '//comment()') %>%    # select comment nodes
    html_text() %>%    # extract comment text
    paste(collapse = '') %>%   # collapse to a single string 
    read_html() %>%    # reparse to HTML
    html_node('table#players_standard_pitching')  %>%
    html_table()   # parse table
  
  
  mlb_batters_dfs[[length(mlb_batters_dfs) + 1]] = mlb_batters_df
  mlb_batters_dfs[[length(mlb_batters_dfs)]]$year = year
  mlb_pitchers_dfs[[length(mlb_pitchers_dfs) + 1]] = mlb_pitchers_df
  mlb_pitchers_dfs[[length(mlb_pitchers_dfs)]]$year = year
  print("done")
  
}

milb_batter_dfs = list()
milb_pitchers_dfs = list()
for (year in c(2012:2019, 2021)) {
  print(year)
  milb_html = glue::glue(
    "https://www.baseball-reference.com/register/league.cgi?group=Minors&year={year}"
  )
  milb_league_list = rvest::read_html(milb_html)
  milb_league_table = milb_league_list %>%
    html_table() %>%
    as.data.frame()
  milb_league_href = milb_league_list %>%
    rvest::html_nodes("td") %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href")
  number_division = length(milb_league_href)
  index = 0
 
  for (i in c(1:number_division)) {

    if (is.na(milb_league_table[index + 1,]$Rk)) {
      index = index + 2
    }
    else {
      index = index + 1
    }
    print(milb_league_table$Rk)
    print(index)
    league = milb_league_table[index, 3]
    print(league)
    league_url = "https://www.baseball-reference.com"
    milb_team_list = league_url %>%
      paste0(milb_league_href[i]) %>%
      rvest::read_html()
    if (year == 2021) {
      milb_team_href = milb_team_list %>%
        html_nodes(xpath = '//comment()') %>%    # select comment nodes
        html_text() %>%    # extract comment text
        paste(collapse = '') %>%   # collapse to a single string
        read_html() %>%    # reparse to HTML
        html_node('table#standings_pitching')  %>%
        rvest::html_nodes("th") %>%
        rvest::html_nodes("a") %>%
        rvest::html_attr("href")
    }
    
    else {
      milb_team_href = milb_team_list %>%
        rvest::html_nodes(xpath = '//*[@id="regular_season"]') %>%
        rvest::html_nodes("th") %>%
        rvest::html_nodes("a") %>%
        rvest::html_attr("href")
    }
    
    team_size = length(milb_team_href)
    for (j in c(1:team_size)) {
      print(league)
      milb_league_team_list = league_url %>%
        paste0(milb_team_href[j]) %>%
        rvest::read_html()
      tryCatch({
        milb_batters = milb_league_team_list %>%
          html_table() %>%
          as.data.frame()
        milb_pitchers = milb_league_team_list %>%
          html_nodes(xpath = '//comment()') %>%    # select comment nodes
          html_text() %>%    # extract comment text
          paste(collapse = '') %>%   # collapse to a single string
          read_html() %>%    # reparse to HTML
          html_node('table#team_pitching')  %>%
          html_table()
        milb_batter_dfs[[length(milb_batter_dfs) + 1]] = milb_batters
        milb_batter_dfs[[length(milb_batter_dfs)]]$year = year
        milb_batter_dfs[[length(milb_batter_dfs)]]$league = league
        milb_pitchers_dfs[[length(milb_pitchers_dfs) + 1]] = milb_pitchers
        milb_pitchers_dfs[[length(milb_pitchers_dfs)]]$year = year
        milb_pitchers_dfs[[length(milb_pitchers_dfs)]]$league = league
        print(j)
      }, error = function(e)
        message('Skipping url', league_url))
    }
    print("New Division")
  }
  print("done")
}

saveRDS(milb_batter_dfs, file='milb_batter_dfs.RData')
saveRDS(milb_pitchers_dfs, file='milb_pitchers_dfs.RData')
saveRDS(mlb_batters_dfs, file='mlb_batters_dfs.RData')
saveRDS(mlb_pitchers_dfs, file='mlb_pitchers_dfs.RData')

