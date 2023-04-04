library(xml2)
library(rvest)
library(tidyverse)
library(rjson)
library(dplyr)
library(randomForest)

# Get all players 2021-2022 season data
nba_key <- "dka7dfucuryhqa7ch4r92dcc"

schedule <- glue::glue("https://api.sportradar.us/nba/trial/v7/eng/games/2021/REG/schedule.json?api_key={nba_key}")
nba_schedule <- schedule %>%
  rjson::fromJSON(file = .)

team_ids <- NULL

#loop through data, get all of team id's
for(i in 1:length(nba_schedule)) {
    team_ids <- c(team_ids, nba_schedule$games[[i]]$home$id)
}
team_ids <- unique(team_ids)

# get team roster using team id
team_roster <- list()
for(i in 1:30) {
  team_api <- glue::glue("https://api.sportradar.us/nba/trial/v7/en/teams/{team_ids[i]}/profile.json?api_key={nba_key}")
  team_roster[[i]] <- rjson::fromJSON(file = team_api)
  Sys.sleep(1)
}

nba_players <- list()

# get only players from the team roster
for(i in 1:length(team_roster)) {
  nba_players <- c(nba_players, team_roster[[i]]$players)

}

# get only players who have played 3-7 full seasons in the NBA
qualified_players <- list()
for(i in 1:length(nba_players)) {
  if (2 <= nba_players[[i]]$experience &  nba_players[[i]]$experience <= 6 & nba_players[[i]]$status == "ACT") {
    qualified_players <- c(qualified_players, nba_players[i])
  }

}

# get player statistics
player_stats <- list()
for(i in 1:length(qualified_players)) {
  player_api <- glue::glue("https://api.sportradar.us/nba/trial/v7/en/players/{qualified_players[[i]]$id}/profile.json?api_key={nba_key}")
  player_stats[[i]] <- rjson::fromJSON(file = player_api)
  Sys.sleep(1)
}


saveRDS(qualified_players,file="qualified_players.RData")
saveRDS(player_stats, file = "players_stats.RData")


players_stats <- readRDS("players_stats.RData")

# flatten into data frame
player_season_df<- NULL
for (i in 1:length(player_stats)){
  player_season_df[[i]] <- player_stats[[i]]$seasons %>%
  lapply(data.frame) %>%
  do.call(dplyr::bind_rows, .)
}

# Get rid of extra columns for some players
player_season_df <- lapply(player_season_df, function(x) x[-c(99:383)])
# create data frame
player_season_df <- do.call(rbind, player_season_df)
# only want last two seasons of data from players
player_season_df <- filter(player_season_df, player_season_df$year >= 2019)
player_season_df <- unique(player_season_df)
rownames(player_season_df) <- NULL
# Get rid of players who had missing seasons in last three years or who are injured
# this year
player_season_df <- player_season_df[-c(163:165,178,179,189:191,354,355,485,489,490,527,528, 568:570),]



# get player name and ID for player_season_df
player_name <- NULL
for (i in 1:length(player_stats)){
  player_name[i] <- player_stats[[i]]$full_name
}

# put player in 3 times for each season
player_name <- rep(player_name, each = 3)



# create new data frame with players names and ID's
new_player_df <- cbind(player_season_df, player_name)

clean_player_df <- new_player_df

# get rid of unwanted columns
data.frame(colnames(clean_player_df))

# Keep only minutes, points, rebounds, assists, free throw percentage
# effective_fg_pct true_shooting_pct, teams.average.efficiency
clean_player_df2 <- clean_player_df[c(2,3,5,6,15,21,25,41,48,65,66,69,70,85,99)]
# reorder to get it in better format with names and ID's near front
clean_player_df3 <- clean_player_df2[, c(15,1,2,3,4,10,5,6,9,7,12,13,11,14,8)]
#make column names easier to read
colnames(clean_player_df3) <- c('player_name','year','type','team_name',
                                'team_market','MP','FG.', 'X3P.',
                                'eFG.', 'FT.', 'TRB', 'AST', 'PTS',
                                'PER','TS.')

saveRDS(clean_player_df3,file="cleaned_players_data.RData")

cleaned_players_2022 <- readRDS("cleaned_players_data.RData")

## function to get previous seasons data ##
collect_seasons <- function(html_1, html_2) {
  num_cols <- c(2:11)

  regular_list <- rvest::read_html(html_1)
  regular_player_df <- regular_list %>%
    rvest::html_table() %>%
    data.frame()


  advanced_list <- rvest::read_html(html_2)
  advanced_player_df <- advanced_list %>%
    rvest::html_table() %>%
    data.frame() %>%
    select(c(8:9))

  players_df <- cbind(regular_player_df, advanced_player_df)
  players_df <- players_df[!grepl('Rk', players_df$Rk), ] %>%
    distinct(Rk, .keep_all = TRUE) %>%
    select(c('Player', 'MP', 'FG.', 'X3P.', 'eFG.', 'FT.', 'TRB', 'AST', 'PTS', 'PER', "TS." ))
  players_df[, num_cols] <- apply(players_df[, num_cols], 2, as.numeric)
  # add column for receiving vote for MIP
  players_df$MIP <- 0
  return(players_df)
}

#collect previous seasons data
players_2022 <- collect_seasons("https://www.basketball-reference.com/leagues/NBA_2022_per_game.html",
                                "https://www.basketball-reference.com/leagues/NBA_2022_advanced.html")

players_2021 <- collect_seasons("https://www.basketball-reference.com/leagues/NBA_2021_per_game.html",
                                "https://www.basketball-reference.com/leagues/NBA_2021_advanced.html")

players_2020 <- collect_seasons("https://www.basketball-reference.com/leagues/NBA_2020_per_game.html",
                                "https://www.basketball-reference.com/leagues/NBA_2020_advanced.html")

players_2019 <- collect_seasons("https://www.basketball-reference.com/leagues/NBA_2019_per_game.html",
                                "https://www.basketball-reference.com/leagues/NBA_2019_advanced.html")

players_2018 <- collect_seasons("https://www.basketball-reference.com/leagues/NBA_2018_per_game.html",
                                "https://www.basketball-reference.com/leagues/NBA_2018_advanced.html")

players_2017 <- collect_seasons("https://www.basketball-reference.com/leagues/NBA_2017_per_game.html",
                                "https://www.basketball-reference.com/leagues/NBA_2017_advanced.html")

players_2016 <- collect_seasons("https://www.basketball-reference.com/leagues/NBA_2016_per_game.html",
                                "https://www.basketball-reference.com/leagues/NBA_2016_advanced.html")


saveRDS(players_2016,file="players_2016.RData")
saveRDS(players_2017,file="players_2017.RData")
saveRDS(players_2018,file="players_2018.RData")
saveRDS(players_2019,file="players_2019.RData")
saveRDS(players_2020,file="players_2020.RData")
saveRDS(players_2021,file="players_2021.RData")

players_2016 <- readRDS("players_2016.RData")
players_2017 <- readRDS("players_2017.RData")
players_2018 <- readRDS("players_2018.RData")
players_2019 <- readRDS("players_2019.RData")
players_2020 <- readRDS("players_2020.RData")
players_2021 <- readRDS("players_2021.RData")


# change MIP column to 1 for those who received MIP votes
players_2021$MIP[c(421,185,410,534,286,59,74,65,528,91,102,131,171,407,11,31,66,
                   27,177,228,329,441,522,53,504)] <- 1
players_2020$MIP[c(241,2,464,181,176,449,521,525,488,72,165,132,424,134,41)] <- 1
players_2019$MIP[c(449,447,434,171,225,435,209,430,108,18,41,178,184,79,97,140,
                   169,195,294,421,507,57,493)] <- 1
players_2018$MIP[c(16,382,85,133,140,421,3,70,361,130,436,520,349,439,492,152,
                   239,360,237,142,180,197,415,427,437,449,484,503,264,411,441)] <- 1
players_2017$MIP[c(16,156,361,425,229,36,53,172,87,451,186,28,97,178,437,55,71,
                   108,214,453,461,234,333,363,395)] <- 1

# create data frame of improvement for players of this season, from 2021 to 2022)
temp <- unique(cleaned_players_2022$player_name, .keep_all = TRUE)
improvement_2022 <- cleaned_players_2022[cleaned_players_2022$year == 2021,]
improvement_2021 <- cleaned_players_2022[cleaned_players_2022$year == 2020,]
improvement_2022 <- select(improvement_2022, c(6:15))
improvement_2021 <- select(improvement_2021, c(6:15))

improvement_df_2022 <- improvement_2022 - improvement_2021
improvement_df_2022 <- cbind(temp, improvement_df_2022)
colnames(improvement_df_2022)[colnames(improvement_df_2022) == "temp"] <- "player_name"
row.names(improvement_df_2022) <- NULL
improvement_df_2022$MIP <- 0



### CREATE IMPROVEMENT DF'S FOR PREVIOUS SEASONS ###

## function to create improvement DF's ##
improvement <- function(season1, season2) {

  diff_1 <- setdiff(season1$Player, season2$Player)
  ID_1 <- season1$Player %in% diff_1
  combined_1 <- season1[!ID_1,]
  combined_1[is.na(combined_1)] <- 0
  diff_2 <- setdiff(season2$Player, season1$Player)
  ID_2 <- season2$Player %in% diff_2
  combined_2 <- season2[!ID_2,]
  combined_2[is.na(combined_2)] <- 0

  improvement_1 <- select(combined_1, c(2:11))
  improvement_2 <- select(combined_2, c(2:11))
  improvement_2 <- improvement_2 - improvement_1
  improvement_2 <- cbind(combined_2$Player, improvement_2)
  improvement_2 <- cbind(improvement_2, combined_2$MIP)
  colnames(improvement_2)[colnames(improvement_2) == "combined_2$Player"] <- "player_name"
  colnames(improvement_2)[colnames(improvement_2) == "combined_2$MIP"] <- "MIP"
  row.names(improvement_2) <- NULL
  return(improvement_2)
}

# Create improvement data frames
improvement_df_2017 <- improvement(players_2016, players_2017)
improvement_df_2018 <- improvement(players_2017, players_2018)
improvement_df_2019 <- improvement(players_2018, players_2019)
improvement_df_2020 <- improvement(players_2019, players_2020)
improvement_df_2021 <- improvement(players_2020, players_2021)


saveRDS(improvement_df_2017,file="improvement_df_2017.RData")
saveRDS(improvement_df_2018,file="improvement_df_2018.RData")
saveRDS(improvement_df_2019,file="improvement_df_2019.RData")
saveRDS(improvement_df_2020,file="improvement_df_2020.RData")
saveRDS(improvement_df_2021,file="improvement_df_2021.RData")
saveRDS(improvement_df_2022,file="improvement_df_2022.RData")

improvement_df_2017 <- readRDS("improvement_df_2017.RData")
improvement_df_2018 <- readRDS("improvement_df_2018.RData")
improvement_df_2019 <- readRDS("improvement_df_2019.RData")
improvement_df_2020 <- readRDS("improvement_df_2020.RData")
improvement_df_2021 <- readRDS("improvement_df_2021.RData")
improvement_df_2022 <- readRDS("improvement_df_2022.RData")

### EDA ###

#  Check colinearity between variables
cont_vals <- c(2:10)
game_cor <- cor(improvement_df_2022[, cont_vals], use="complete.obs")
corrplot::corrplot(game_cor)

# Look at relationship between points and player efficiency rating
plot(improvement_df_2021[,c("PTS", "PER")])


### MODELING ###
model_df <- rbind(improvement_df_2019, improvement_df_2018, improvement_df_2017)
model_df$MIP <- as.factor(model_df$MIP)
model_df <- na.omit(model_df[, -1])

player_matrix <- model.matrix(MIP ~ ., data=model_df)[, c(-1)]
rf_mod <- randomForest(model_df$MIP ~ ., data=data.frame(player_matrix))

# See what variables are most important
varImpPlot(rf_mod)
plot(rf_mod)
plot(margin(rf_mod,model_df$MIP))

### MODEL TRAINING ###

# Take out minutes, true shooting percentage and eFG to improve model
test_22 <- subset(improvement_df_2022, select = c(1,3,4,6,7,8,9,10,12) )
test_21 <- subset(improvement_df_2021, select = c(1,3,4,6,7,8,9,10,12) )
test_20 <- subset(improvement_df_2020, select = c(1,3,4,6,7,8,9,10,12) )
test_19 <- subset(improvement_df_2019, select = c(1,3,4,6,7,8,9,10,12) )
test_18 <- subset(improvement_df_2018, select = c(1,3,4,6,7,8,9,10,12) )
test_17 <- subset(improvement_df_2017, select = c(1,3,4,6,7,8,9,10,12) )


model_df <- rbind(test_19, test_18, test_17)
model_df$MIP <- as.factor(model_df$MIP)

trainX <- model.matrix(MIP ~ . -player_name, data=model_df)[,-c(1)]
testX <- model.matrix(MIP ~ . -player_name, data=test_20)[,-c(1)]

rf_train <- randomForest::randomForest(model_df$MIP ~ ., data=data.frame(trainX),
                                      ntree=1000, na.action=na.omit, maxnodes = 25)

test_preds <- predict(rf_train, newdata = data.frame(testX), type="prob")
test_preds <- test_preds %>%
  data.frame()
test_preds <- cbind(test_preds, improvement_df_2020$MIP)
test_preds <- test_preds[order(test_preds$X1,decreasing = TRUE),]

### VALIDATION ###
validationX <- model.matrix(MIP ~ . -player_name, data=test_21)[,-c(1)]
rf_train <- randomForest::randomForest(model_df$MIP ~ ., data=data.frame(trainX),
                                      ntree=1000, na.action=na.omit, maxnodes = 25)

test_preds <- predict(rf_train, newdata = data.frame(validationX), type="prob")
test_preds <- test_preds %>%
  data.frame()
test_preds <- cbind(test_preds, improvement_df_2021$MIP)
test_preds <- test_preds[order(test_preds$X1,decreasing = TRUE),]

### PREDICTION ###
set.seed(2319)
final_df <- rbind(test_21, test_20, test_19, test_18, test_17)
final_df$MIP <- as.factor(final_df$MIP)

trainX <- model.matrix(MIP ~ . -player_name, data=final_df)[,-c(1)]
predictionX <- model.matrix(MIP ~ . -player_name, data=test_22)[,-c(1)]
rf_train <- randomForest::randomForest(final_df$MIP ~ ., data=data.frame(trainX),
                                      ntree=1000, na.action=na.omit, maxnodes = 25)

test_preds <- predict(rf_train, newdata = data.frame(predictionX), type="prob")
test_preds <- test_preds %>%
  data.frame()
test_preds <- test_preds[order(test_preds$X1,decreasing = TRUE),]
testing <- test_preds[1:10,]

# get top 5  probabilities
predicted <- improvement_df_2022[c(171,28,76,133,131),]
final_prediction <- cbind(predicted, testing$X1[1:5])
final_prediction <- cbind(final_prediction$player_name, final_prediction$`testing$X1`)
final_prediction <- final_prediction %>% 
  data.frame()
final_prediction$X2 <- as.numeric(final_prediction$X2)

# Create bar-graph to show winner probabilities
final_prediction %>%
  ggplot() +
  aes(x = reorder(X1, -X2), y = X2) + 
  labs(title="2021-2022 NBA Most Improved Player", subtitle = "Probability that player wins the award") +
  geom_bar(stat='identity', width = .8, fill = "#058ED9") +
  theme_void() +
  geom_text(aes(label=scales::percent(X2)), position=position_dodge(width=0.9), 
            vjust=1.3, size = 6, color = "white") +
  theme(axis.text.x=element_text(hjust=.5,vjust=1, size = 20), 
        axis.title.x = element_blank(), plot.title = element_text(hjust = 0.5,
                                                                  size = 25, 
                                                                  face="bold"), 
        plot.subtitle = element_text(hjust = 0.5, size = 20)) 

