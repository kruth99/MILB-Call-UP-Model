library(xml2)
library(rvest)
library(tidyverse)
library(rjson)
library(dplyr)
library(corrplot)

milb_batters_dfs = readRDS("milb_batter_dfs.RData")
milb_pitchers_dfs = readRDS("milb_pitchers_dfs.RData")
mlb_batters_dfs = readRDS("mlb_batters_dfs.RData")
mlb_pitchers_dfs = readRDS("mlb_pitchers_dfs.RData")

# Combine all the data for each into one table
all_milb_batters_dfs = do.call(rbind, milb_batters_dfs)
all_milb_pitchers_dfs = do.call(rbind, milb_pitchers_dfs)
all_mlb_batters_dfs =  do.call(rbind, mlb_batters_dfs)
all_mlb_pitchers_dfs = do.call(rbind, mlb_pitchers_dfs)


# Convert the tables to data frames
all_milb_batters_dfs = as.data.frame(all_milb_batters_dfs)
all_milb_pitchers_dfs = as.data.frame(all_milb_pitchers_dfs)
all_mlb_batters_dfs = as.data.frame(all_mlb_batters_dfs)
all_mlb_pitchers_dfs = as.data.frame(all_mlb_pitchers_dfs)

head(all_milb_batters_dfs, 30)
head(all_milb_pitchers_dfs, 30)
head(all_mlb_pitchers_dfs, 30)
head(all_mlb_batters_dfs, 30)


# Clean out the rows with a NA in the rank columns that equal to the totals
all_milb_batters_clean1 = all_milb_batters_dfs[complete.cases(all_milb_batters_dfs$Rk),]
all_milb_pitchers_clean1 = all_milb_pitchers_dfs[complete.cases(all_milb_pitchers_dfs$Rk),]

#These ones have Rk columns that equal Rk
all_mlb_batters_clean1 = all_mlb_batters_dfs[!grepl('Rk', all_mlb_batters_dfs$Rk),]
all_mlb_pitchers_clean1 = all_mlb_pitchers_dfs[!grepl('Rk', all_mlb_pitchers_dfs$Rk),]


# Also notice that there are some that duplicates in the pitchers and batters that have a empty league column
all_milb_batters_clean2 = all_milb_batters_clean1[all_milb_batters_clean1$league != "",]
all_milb_pitchers_clean2 = all_milb_pitchers_clean1[all_milb_pitchers_clean1$league != "",]

#Look at all the possible leagues in the minor league data
print(unique(all_milb_batters_clean2$league))
print(unique(all_milb_pitchers_clean2$league))

# Clean out all data that is lower than A. I want to just focus on A, AA, AAA since those are standardized levels within the last few years
all_milb_batters_clean3 = all_milb_batters_clean2[!grepl("Short|Rookie", all_milb_batters_clean2$league),]
print(unique(all_milb_batters_clean3$league))

all_milb_pitchers_clean3 = all_milb_pitchers_clean2[!grepl("Short|Rookie", all_milb_pitchers_clean2$league),]
print(unique(all_milb_pitchers_clean3$league))

# filter out the notes and RK column
all_milb_pitchers_clean3$Notes = NULL
all_milb_pitchers_clean3$Rk = NULL
all_milb_batters_clean3$Notes = NULL
all_milb_batters_clean3$Rk = NULL

#Clean out all special characters in the names
all_milb_batters_clean3$Name = str_replace_all(all_milb_batters_clean3$Name, "[^[:alnum:]]", " ")
all_milb_pitchers_clean3$Name = str_replace_all(all_milb_pitchers_clean3$Name, "[^[:alnum:]]", " ")
all_mlb_batters_clean1$Name = str_replace_all(all_mlb_batters_clean1$Name, "[^[:alnum:]]", " ")
all_mlb_pitchers_clean1$Name = str_replace_all(all_mlb_pitchers_clean1$Name, "[^[:alnum:]]", " ")

#Clean out all whitespace in the names to make it easier to compare
all_milb_batters_clean3$Name = gsub(" ", "", all_milb_batters_clean3$Name, fixed = TRUE)
all_milb_pitchers_clean3$Name = gsub(" ", "", all_milb_pitchers_clean3$Name, fixed = TRUE)
all_mlb_batters_clean1$Name = gsub(" ", "", all_mlb_batters_clean1$Name, fixed = TRUE)
all_mlb_pitchers_clean1$Name = gsub(" ", "", all_mlb_pitchers_clean1$Name, fixed = TRUE)

#Filter out all batters with less then 50 at bats and pitchers with less than 5 G. I thought this would filter out many players
# Who didn't have a huge amount of time at the level, and also rehab assignments

all_milb_batters_clean4 = filter(all_milb_batters_clean3, AB > 50)
all_milb_pitchers_clean4 = filter(all_milb_pitchers_clean3, G > 5)

#Convert these all to numeric. I realized it helps with the future
all_milb_batters_clean4[] = lapply(all_milb_batters_clean4, function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else x
})
all_milb_pitchers_clean4[] = lapply(all_milb_pitchers_clean4, function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else x
})
# Some of the pitching columns have NAs
all_milb_pitchers_clean5 =all_milb_pitchers_clean4[complete.cases(all_milb_pitchers_clean4),]

# Create tables for each league - batters
all_AAA_batters = filter(all_milb_batters_clean4, league == "AAA")
print(unique(all_AAA_batters$league))

all_AA_batters = filter(all_milb_batters_clean4, league == "AA")
print(unique(all_AA_batters$league))

#I'm including Adv A and A together
all_A_batters = filter(all_milb_batters_clean4, league != "AA" & league != "AAA")
print(unique(all_A_batters$league))

print(nrow(all_AAA_batters) + nrow(all_AA_batters) + nrow(all_A_batters))

# Do the same for pitchers
all_AAA_pitchers = filter(all_milb_pitchers_clean5, league == "AAA")
print(unique(all_AAA_pitchers$league))

all_AA_pitchers = filter(all_milb_pitchers_clean5, league == "AA")
print(unique(all_AA_pitchers$league))

#I'm including Adv A and A together
all_A_pitchers = filter(all_milb_pitchers_clean5, league != "AA" & league != "AAA")
print(unique(all_A_pitchers$league))

print(nrow(all_AAA_pitchers) + nrow(all_AA_pitchers) + nrow(all_A_pitchers))

# Delete the league column now that they are organized
all_A_batters$league =NULL
all_AA_batters$league = NULL
all_AAA_batters$league = NULL
all_A_pitchers$league = NULL
all_AA_pitchers$league = NULL
all_AAA_pitchers$league = NULL

# Add columns to see if the player progresses

for(i in 1: nrow(all_A_batters)){
  name = all_A_batters[i, "Name"]
  if(name %in% all_AA_batters$Name) {
    all_A_batters[i, "Promoted"] = 1
  }
  else {
    all_A_batters[i, "Promoted"] = 0
  }
}

for(i in 1: nrow(all_AA_batters)){
  name = all_AA_batters[i, "Name"]
  if(name %in% all_AAA_batters$Name) {
    all_AA_batters[i, "Promoted"] = 1
  }
  else {
    all_AA_batters[i, "Promoted"] = 0
  }
}

for(i in 1: nrow(all_AAA_batters)){
  name = all_AAA_batters[i, "Name"]
  if(name %in% all_mlb_batters_clean1$Name) {
    all_AAA_batters[i, "Promoted"] = 1
  }
  else {
    all_AAA_batters[i, "Promoted"] = 0
  }
}

for(i in 1: nrow(all_A_pitchers)){
  name = all_A_pitchers[i, "Name"]
  if(name %in% all_AA_pitchers$Name) {
    all_A_pitchers[i, "Promoted"] = 1
  }
  else {
    all_A_pitchers[i, "Promoted"] = 0
  }
}

for(i in 1: nrow(all_AA_pitchers)){
  name = all_AA_pitchers[i, "Name"]
  if(name %in% all_AAA_pitchers$Name) {
    all_AA_pitchers[i, "Promoted"] = 1
  }
  else {
    all_AA_pitchers[i, "Promoted"] = 0
  }
}

for(i in 1: nrow(all_AAA_pitchers)){
  name = all_AAA_pitchers[i, "Name"]
  if(name  %in% all_mlb_pitchers_clean1$Name) {
    all_AAA_pitchers[i, "Promoted"] = 1
  }
  else {
    all_AAA_pitchers[i, "Promoted"] = 0
  }
}

A_batters = all_A_batters[,-which(names(all_A_batters) == "Name" | names(all_A_batters) == "year")]
AA_batters = all_AA_batters[,-which(names(all_AA_batters) == "Name" | names(all_AA_batters) == "year")]
AAA_batters = all_AAA_batters[,-which(names(all_AAA_batters) == "Name" | names(all_AAA_batters) == "year")]
A_pitchers = all_A_pitchers[,-which(names(all_A_pitchers) == "Name" | names(all_A_pitchers) == "year")]
AA_pitchers = all_AA_pitchers[,-which(names(all_AA_pitchers) == "Name" | names(all_AA_pitchers) == "year")]
AAA_pitchers = all_AAA_pitchers[,-which(names(all_AAA_pitchers) == "Name" | names(all_AAA_pitchers) == "year")]

saveRDS(A_batters, file='A_batters.RData')
saveRDS(AA_batters, file='AA_batters.RData')
saveRDS(AAA_batters, file='AAA_batters.RData')
saveRDS(A_pitchers, file='A_pitchers.RData')
saveRDS(AA_pitchers, file='AA_pitchers.RData')
saveRDS(AAA_pitchers, file='AAA_pitchers.RData')


# EDA
## What factors are most important to promotion at the different levels?
A_batters_cor = cor(A_batters, use = "complete.obs")
corrplot(A_batters_cor, title = "A Batters correlation", mar=c(0,0,1,0))

AA_batters_cor = cor(AA_batters, use = "complete.obs")
corrplot(AA_batters_cor, title = "AA Batters correlation", mar=c(0,0,1,0))

AAA_batters_cor = cor(AAA_batters, use = "complete.obs")
corrplot(AAA_batters_cor, title = "AAA Batters correlation", mar=c(0,0,1,0))

A_pitchers_cor = cor(A_pitchers, use = "complete.obs")
corrplot(A_pitchers_cor, title = "A Pitchers correlation", mar=c(0,0,1,0))

AA_pitchers_cor = cor(AA_pitchers, use = "complete.obs")
corrplot(AA_pitchers_cor, title = "AA Pitchers correlation", mar=c(0,0,1,0))

AAA_pitchers_cor = cor(AAA_pitchers, use = "complete.obs")
corrplot(AAA_pitchers_cor, title = "AAA Pitchers correlation", mar=c(0,0,1,0))

# It looks like most things are similar between the levels, For batters, everything seems to be very similar points in the different levels. Interesting note for the batters,
# There is nothing that is negative, everything is positive

# Interested to see the specific correlation between Age and level
cor(A_batters$Promoted, A_batters$Age)
cor(AA_batters$Promoted, AA_batters$Age)
cor(AAA_batters$Promoted, AAA_batters$Age)
cor(A_pitchers$Promoted, A_pitchers$Age)
cor(AA_batters$Promoted, AA_batters$Age)
cor(AAA_batters$Promoted, AAA_batters$Age)
# Barley no impact all. Age does not have any factor

# See if the stats have changed over the last five years. Filter the last 5 years and 4 before that
all_A_batters_filtered1 = filter(all_A_batters, year > 2015)
all_A_batters_filtered2 = filter(all_A_batters, year < 2015)

A_batters_eda_filtered1 = all_A_batters_filtered1[,-which(names(all_A_batters_filtered1) == "Name" | names(all_A_batters_filtered1) == "year")]
A_batters_cor1 = cor(A_batters_eda_filtered1, use = "complete.obs")
corrplot(A_batters_cor1, title = "A Batters correlation year 2016-2021", mar=c(0,0,1,0))

cor(A_batters_eda_filtered1$Promoted, A_batters_eda_filtered1$HR)
cor(A_batters_eda_filtered1$Promoted, A_batters_eda_filtered1$OBP)
cor(A_batters_eda_filtered1$Promoted, A_batters_eda_filtered1$H)
A_batters_eda_filtered2= all_A_batters_filtered2[,-which(names(all_A_batters_filtered2) == "Name" | names(all_A_batters_filtered2) == "year")]
A_batters_cor2 = cor(A_batters_eda_filtered2, use = "complete.obs")
corrplot(A_batters_cor2, title = "A Batters correlation year 2012-2015", mar=c(0,0,1,0))

cor(A_batters_eda_filtered2$Promoted, A_batters_eda_filtered2$HR)
cor(A_batters_eda_filtered2$Promoted, A_batters_eda_filtered2$OBP)
cor(A_batters_eda_filtered2$Promoted, A_batters_eda_filtered2$H)
# All the factors seem to be less correlated in the last 5 years vs the previous 4



# See if the stats have changed over the last five years. Filter the last 5 years and 4 before that
all_AA_batters_filtered1 = filter(all_AA_batters, year > 2015)
all_AA_batters_filtered2 = filter(all_AA_batters, year < 2015)

AA_batters_eda_filtered1 = all_AA_batters_filtered1[,-which(names(all_AA_batters_filtered1) == "Name" | names(all_AA_batters_filtered1) == "year")]
AA_batters_cor1 = cor(AA_batters_eda_filtered1, use = "complete.obs")
corrplot(AA_batters_cor1, title = "A Batters correlation year 2016-2021", mar=c(0,0,1,0))

cor(AA_batters_eda_filtered1$Promoted, AA_batters_eda_filtered1$HR)
cor(AA_batters_eda_filtered1$Promoted, AA_batters_eda_filtered1$OBP)
cor(AA_batters_eda_filtered1$Promoted, AA_batters_eda_filtered1$H)
AA_batters_eda_filtered2= all_AA_batters_filtered2[,-which(names(all_AA_batters_filtered2) == "Name" | names(all_AA_batters_filtered2) == "year")]
AA_batters_cor2 = cor(AA_batters_eda_filtered2, use = "complete.obs")
corrplot(AA_batters_cor2, title = "A Batters correlation year 2012-2015", mar=c(0,0,1,0))

cor(AA_batters_eda_filtered2$Promoted, AA_batters_eda_filtered2$HR)
cor(AA_batters_eda_filtered2$Promoted, AA_batters_eda_filtered2$OBP)
cor(AA_batters_eda_filtered2$Promoted, AA_batters_eda_filtered2$H)
# All the factors seem to be less correlated in the last 5 years vs the previous 4



# See if the stats have changed over the last five years. Filter the last 5 years and 4 before that
all_AAA_batters_filtered1 = filter(all_AAA_batters, year > 2015)
all_AAA_batters_filtered2 = filter(all_AAA_batters, year < 2015)

AAA_batters_eda_filtered1 = all_AAA_batters_filtered1[,-which(names(all_AAA_batters_filtered1) == "Name" | names(all_AAA_batters_filtered1) == "year")]
AAA_batters_cor1 = cor(AAA_batters_eda_filtered1, use = "complete.obs")
corrplot(AAA_batters_cor1, title = "A Batters correlation year 2016-2021", mar=c(0,0,1,0))

cor(AAA_batters_eda_filtered1$Promoted, AAA_batters_eda_filtered1$HR)
cor(AAA_batters_eda_filtered1$Promoted, AAA_batters_eda_filtered1$OBP)
cor(AAA_batters_eda_filtered1$Promoted, AAA_batters_eda_filtered1$H)
AAA_batters_eda_filtered2= all_AAA_batters_filtered2[,-which(names(all_AAA_batters_filtered2) == "Name" | names(all_AAA_batters_filtered2) == "year")]
AAA_batters_cor2 = cor(AAA_batters_eda_filtered2, use = "complete.obs")
corrplot(AAA_batters_cor2, title = "A Batters correlation year 2012-2015", mar=c(0,0,1,0))

cor(AAA_batters_eda_filtered2$Promoted, AAA_batters_eda_filtered2$HR)
cor(AAA_batters_eda_filtered2$Promoted, AAA_batters_eda_filtered2$OBP)
cor(AAA_batters_eda_filtered2$Promoted, AAA_batters_eda_filtered2$H)
# All the factors seem to be less correlated in the last 5 years vs the previous 4


# See if the stats have changed over the last five years. Filter the last 5 years and 4 before that
all_A_pitchers_filtered1 = filter(all_A_pitchers, year > 2015)
all_A_pitchers_filtered2 = filter(all_A_pitchers, year < 2015)

A_pitchers_eda_filtered1 = all_A_pitchers_filtered1[,-which(names(all_A_pitchers_filtered1) == "Name" | names(all_A_pitchers_filtered1) == "year")]
A_pitchers_cor1 = cor(A_pitchers_eda_filtered1, use = "complete.obs")
corrplot(A_pitchers_cor1, title = "A Pitchers correlation year 2016-2021", mar=c(0,0,1,0))

cor(A_pitchers_eda_filtered1$Promoted, A_pitchers_eda_filtered1$ERA)
cor(A_pitchers_eda_filtered1$Promoted, A_pitchers_eda_filtered1$SO)
cor(A_pitchers_eda_filtered1$Promoted, A_pitchers_eda_filtered1$W)
A_pitchers_eda_filtered2= all_A_pitchers_filtered2[,-which(names(all_A_pitchers_filtered2) == "Name" | names(all_A_pitchers_filtered2) == "year")]
A_pitchers_cor2 = cor(A_pitchers_eda_filtered2, use = "complete.obs")
corrplot(A_pitchers_cor2, title = "A Pitchers correlation year 2012-2015", mar=c(0,0,1,0))

cor(A_pitchers_eda_filtered2$Promoted, A_pitchers_eda_filtered2$ERA)
cor(A_pitchers_eda_filtered2$Promoted, A_pitchers_eda_filtered2$SO)
cor(A_pitchers_eda_filtered2$Promoted, A_pitchers_eda_filtered2$W)
# All the factors seem to be less correlated in the last 5 years vs the previous 4

# See if the stats have changed over the last five years. Filter the last 5 years and 4 before that
all_AA_pitchers_filtered1 = filter(all_AA_pitchers, year > 2015)
all_AA_pitchers_filtered2 = filter(all_AA_pitchers, year < 2015)

AA_pitchers_eda_filtered1 = all_AA_pitchers_filtered1[,-which(names(all_AA_pitchers_filtered1) == "Name" | names(all_AA_pitchers_filtered1) == "year")]
AA_pitchers_cor1 = cor(AA_pitchers_eda_filtered1, use = "complete.obs")
corrplot(AA_pitchers_cor1, title = "AA Pitchers correlation year 2016-2021", mar=c(0,0,1,0))

cor(AA_pitchers_eda_filtered1$Promoted, AA_pitchers_eda_filtered1$ERA)
cor(AA_pitchers_eda_filtered1$Promoted, AA_pitchers_eda_filtered1$SO)
cor(AA_pitchers_eda_filtered1$Promoted, AA_pitchers_eda_filtered1$W)
AA_pitchers_eda_filtered2= all_AA_pitchers_filtered2[,-which(names(all_AA_pitchers_filtered2) == "Name" | names(all_AA_pitchers_filtered2) == "year")]
AA_pitchers_cor2 = cor(AA_pitchers_eda_filtered2, use = "complete.obs")
corrplot(AA_pitchers_cor2, title = "AA Pitchers correlation year 2012-2015", mar=c(0,0,1,0))

cor(AA_pitchers_eda_filtered2$Promoted, AA_pitchers_eda_filtered2$ERA)
cor(AA_pitchers_eda_filtered2$Promoted, AA_pitchers_eda_filtered2$SO)
cor(AA_pitchers_eda_filtered2$Promoted, AA_pitchers_eda_filtered2$W)


# See if the stats have changed over the last five years. Filter the last 5 years and 4 before that
all_AAA_pitchers_filtered1 = filter(all_AAA_pitchers, year > 2015)
all_AAA_pitchers_filtered2 = filter(all_AAA_pitchers, year < 2015)

AAA_pitchers_eda_filtered1 = all_AAA_pitchers_filtered1[,-which(names(all_AAA_pitchers_filtered1) == "Name" | names(all_AAA_pitchers_filtered1) == "year")]
AAA_pitchers_cor1 = cor(AAA_pitchers_eda_filtered1, use = "complete.obs")
corrplot(AAA_pitchers_cor1, title = "AAA Pitchers correlation year 2016-2021", mar=c(0,0,1,0))

cor(AAA_pitchers_eda_filtered1$Promoted, AAA_pitchers_eda_filtered1$ERA)
cor(AAA_pitchers_eda_filtered1$Promoted, AAA_pitchers_eda_filtered1$SO)
cor(AAA_pitchers_eda_filtered1$Promoted, AAA_pitchers_eda_filtered1$W)
AAA_pitchers_eda_filtered2= all_AAA_pitchers_filtered2[,-which(names(all_AAA_pitchers_filtered2) == "Name" | names(all_AAA_pitchers_filtered2) == "year")]
AAA_pitchers_cor2 = cor(AAA_pitchers_eda_filtered2, use = "complete.obs")
corrplot(AAA_pitchers_cor2, title = "AAA Pitchers correlation year 2012-2015", mar=c(0,0,1,0))

cor(AAA_pitchers_eda_filtered2$Promoted, AAA_pitchers_eda_filtered2$ERA)
cor(AAA_pitchers_eda_filtered2$Promoted, AAA_pitchers_eda_filtered2$SO)
cor(AAA_pitchers_eda_filtered2$Promoted, AAA_pitchers_eda_filtered2$W)
