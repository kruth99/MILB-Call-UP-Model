
library(xml2)
library(rvest)
library(tidyverse)
library(rjson)
library(dplyr)




# Data Cleaning
game_dfs = readRDS("game_df_list.RData")

# try to combine all the dfs into one big data frame
all_games = do.call(rbind, game_dfs)

# lets see how many tables of X number of columns
game_dfs %>% 
  sapply(ncol) %>%
  table()

# looking for the specific dataframes in the list that have 31 columns
which(sapply(game_dfs, ncol) == 31)

# look at the column names for the 31 column dfs vs the 32 column dfs
colnames31 = colnames(game_dfs[[1335]])

colnames32 = colnames(game_dfs[[1]])

colnames32[!(colnames32 %in% colnames31)]

# final solution to combine all dfs with plus minus data
game_full_df = do.call(rbind, 
                       game_dfs[which(sapply(game_dfs, ncol) == 32)])

# game_full_df = read.csv("~/Documents/Personal/nba_player_game_stats.csv",stringsAsFactors = F)

head(game_full_df,30)
dim(game_full_df)

# get rid of column header names that turned into rows
game_clean1 = game_full_df[!grepl('Rk', game_full_df$Rk), ]

-grep('Rk', game_full_df$Rk)
!grepl('Rk',game_full_df$Rk)
game_full_df$Rk!='Rk'

# figure out what to do about DNP
game_clean1 = game_clean1[!grepl('Did Not Play', game_clean1$GS), ]

# check minutes - does this work?
summary(as.numeric(game_clean1$MP))

# garbage time
# how do we set the line for garbage time

# data type cleanup
apply(game_clean1, 2, class)

# setup new df for more clean storage
game_clean2 = game_clean1

# identifying the numeric columns
num_cols = c(3, 4, 11, 13:32)

# make text columns numeric
game_clean2[, num_cols] = apply(game_clean2[, num_cols], 2, as.numeric)

# minutes cleanup
# splitting up the minutes and seconds pieces of minutes played
min_temp = strsplit(as.character(game_clean1$MP), ":")

# grab the 2nd element of each list
sapply(min_temp[1:10], "[", 2)

# creating new column to show when people didn't play
# game_clean2$dnp_reason = game_clean1$MP

# making the minutes a numeric decimal variable
game_clean2$MP = as.numeric(sapply(min_temp, "[", 1)) +
  as.numeric(sapply(min_temp, "[", 2))/60

# setting players with minutes actually played as ACTIVE in the DNP reason column
game_clean2$dnp_reason[!is.na(game_clean2$MP)] = "Active"

# date cleanup
library(lubridate)
game_clean2$Date = lubridate::ymd(game_clean2$Date)
game_clean2$Year = lubridate::year(game_clean2$Date)
game_clean2$Month = lubridate::month(game_clean2$Date)
game_clean2$Week = lubridate::week(game_clean2$Date)
game_clean2$Weekday = lubridate::wday(game_clean2$Date, label=T)

# duplicated data
# check for duplicated data
dim(game_clean2)
dim(unique(game_clean2[, c("player_href", "Date")]))

# new object where we're starting dupe cleanup
game_clean3 = game_clean2[order(game_clean2$player_href), ]
game_clean3[duplicated(game_clean3[, c('player_href', 'Date')]), ]

# make column with T/F indicator for it being a duplicate row
game_clean3$dupe_row = duplicated(game_clean3[, c('player_href', 'Date')])

# fixing some columns with weird formatting
# splitting before and after dash on age
age_temp = strsplit(as.character(game_clean3$Age), "-")

# add the first element before dash to second element divided by 365
game_clean3$Age = as.numeric(sapply(age_temp,"[",1)) +
  as.numeric(sapply(age_temp,"[",2))/365

# 0/1 flag for if the game was a home game for the player
game_clean3$Home = ifelse(grepl('@', game_clean3$Var.8), 0, 1)

# 0/1 flag for if they won the game or not
game_clean3$Win = ifelse(grepl('W', game_clean3$Var.10), 1, 0)

# let's parse out the margin of victory
mov_temp = gsub("[(]", "", as.character(game_clean3$Var.10))
mov_temp1 = gsub("[)]", "", mov_temp)
mov_temp2 = gsub("W ", "", mov_temp1)
mov_temp3 = gsub("L ", "", mov_temp2)
mov_temp4 = gsub("[+]", "", mov_temp3)

# create a margin of victory column using as.numeric
game_clean3$MOV = as.numeric(mov_temp4)

# rename X.. as plusminus
game_clean3$plusminus = game_clean3$X...

# Data Simulation
# ad simulation
user_id = 1:10000
ads_seen = rpois(10000, 100)
ad_seen1 = rbinom(10000, 10, prob=ads_seen/max(ads_seen))
ad_interax1 = rbinom(10000, 1, prob=(ads_seen/max(ads_seen)/10))
ad_type1 = "pepsi"
ad_seen2 = rbinom(10000, 5, prob=ads_seen/max(ads_seen))
ad_interax2 = rbinom(10000, 1, prob=(ads_seen/max(ads_seen)/5))
ad_type2 = "fanatics"


ad_df = data.frame(user_id, 
                   ad_seen1, 
                   ad_interax1, 
                   ad_type1,
                   ad_seen2, 
                   ad_interax2, 
                   ad_type2)

#mean(ad$ad_interax2[ad$])