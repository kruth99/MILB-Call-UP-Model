# R Scraping

library(xml2)
library(rvest)
library(tidyverse)
library(rjson)
library(dplyr)

#nba player data
nba_html = "https://www.basketball-reference.com/players/a/"
nba_player_list = rvest::read_html(nba_html)

nba_player_df = nba_player_list %>%
  rvest::html_table()

# List of the A players
nba_player_href = nba_player_list %>%
  rvest::html_nodes("th") %>%
  rvest::html_nodes("a") %>%
  rvest::html_attr("href")
nba_player_href

sportref_url = "https://www.basketball-reference.com"

# Steven Adams = 24
player_page = sportref_url %>%
  paste0(nba_player_href[24]) %>%
  rvest::read_html()

player_season_summary = player_page %>%
  rvest::html_table()

steven_adams_per_game = player_season_summary[[1]] %>%
  data.frame()

 # This comes from the inspect tab
player_seasons = player_page %>%
  rvest::html_nodes("tr") %>%
  rvest::html_nodes("th") %>%
  rvest::html_nodes("a") %>%
  rvest::html_attr("href")

season_games = sportref_url %>%
  paste0(player_seasons[1]) %>%
  rvest::read_html()

season_df = season_games %>%
  rvest::html_table(fill=T) %>%
  # grab every element of the list that has more than 3 columns
  .[[which(sapply(., ncol) > 3)]]

# grab all player data
player_dfs = list()
season_dfs = list()
game_dfs = list()
season_couner = game_counter = 1
for (i in c(1:23, 25:26)) {
  
  # Go get each letter of players
  player_url = "https://www.basketball-reference.com/players/"
  nba_player_list = player_url %>%
    paste0(letters[i], "/") %>%
    rvest::read_html()
  
  # Put the first element of the list
  #nba_player_df = nba_player_list %>%
    player_dfs[[i]] = nba_player_df[[1]]
  
  # List of the player href
  nba_player_href = nba_player_list %>%
    rvest::html_nodes("th") %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href") 
  
  # Filter out players that havent played in the last 6 years
  # Subsets the href 
  nba_recent_href = nba_player_href[nba_player_df[[1]]$To >= 2016]
  
  # Filters only the ones since 2016
  nba_recent_name = nba_player_df[[1]] %>%
    dplyr::filter(To >= 2016) %>%
    dplyr::pull(Player)
  
  # Grab each players season stats
  for (j in 1:length(nba_recent_href)) {
    
    # Follow steps a steven adams
    player_page = sportref_url %>%
      paste0(nba_recent_href[j]) %>%
      rvest::read_html()
    
    player_season_summary = player_page %>%
      rvest::html_table()
    
    # This comes from the inspect tab
    player_season_href = player_page %>%
      rvest::html_nodes("tr") %>%
      rvest::html_nodes("th") %>%
      rvest::html_nodes("a") %>%
      rvest::html_attr("href")
      unique() # sports-reference repeats years for traded players
      
      seasons_dfs[[season_couner]] = data.frame(player_name=nba_recent_name[j],
                                                player_href = nba_recent_href[i],
                                                player_seasons_df[[1]])
      
      season_counter = season_counter + 1
      
      # Goes through season of the specific player
      for (k in 1:length(player_season_href)) {
        
        # Gets game by game data of each season
        season_game_page = sportref_url %>%
          paste0(player_season_href[k]) %>%
          rvest::read_html()
        
        season_df = season_game_page %>%
          rvest::html_table(fill = TRUE) %>%
          # grab only tables with more than 3 columns
          .[[which(sapply(., ncol)>3)]]
        
        
        game_dfs[[game_counter]] = data.frame(player_name = nba_recent_name[j],
                                              player_href = nba_recent_href[j],
                                              season_df)
        game_counter = game_counter + 1
      }
      print(j)
  }
  print(letters[i])
}
saveRDS(player_dfs, file='~/Documents/Personal/player_df_list.RData')
saveRDS(season_dfs, file='~/Documents/Personal/season_df_list.RData')
saveRDS(game_dfs, file='~/Documents/Personal/game_df_list.RData')

# saveRDS saves the data so you don't need to run it each time in R format





























