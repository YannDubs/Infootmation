library(dplyr)
library(tidyr)
library(RSQLite)
library(tibble)
library(magrittr)
library(purrr)
library(pander)
library(xml2)
library(rvest)
library(stringr)

## GLOBAL OPTIONS / VAR ##
panderOptions('table.split.table', Inf)

## FUNCTIONS ##

# Loop over all matches and put the goal info into a dataframe.
value_from_xpath  <- function(element, xpath, to.int = F, index = 1) {
  xml_find_all(element, xpath) %>%
  #if empty result return NA
  {ifelse(length(.), xml_text(.[[index]]), NA)} %>%
  # put value in integer
  {ifelse(to.int, as.integer(.), .)}
}

node_to_dataframe <- function(n, key) {
  tibble_(list(
    id = ~value_from_xpath(n, './id', to.int = T),
    type = ~value_from_xpath(n, './type'),
    subtype1 = ~value_from_xpath(n, './subtype'),
    subtype2 = ~value_from_xpath(n, paste0('./', key, '_type')),
    player1 = ~value_from_xpath(n, './player1'),
    player2 = ~value_from_xpath(n, './player2'),
    team = ~value_from_xpath(n, './team'),
    lon = ~value_from_xpath(n, './coordinates/value', to.int = T, index = 1),
    lat = ~value_from_xpath(n, './coordinates/value', to.int = T, index = 2),
    elapsed = ~value_from_xpath(n, './elapsed', T),
    elapsed_plus = ~value_from_xpath(n, './elapsed_plus', T)
  ))
}

## UPLOAD DATA ##
con <- dbConnect(SQLite(), dbname="./data/database.sqlite")

# chooses season and league
df <- dbGetQuery(con,"SELECT * FROM Match WHERE country_id IN ('1729','4769','7809','10257','21518') AND season IN ('2015/2016','2014/2015')")
matches <- subset(df, select = c(id,country_id,season,stage,date,home_team_api_id,away_team_api_id,home_team_goal,away_team_goal,goal,shoton,shotoff,foulcommit,card,cross,corner,possession) )
matches$date <- as.Date(matches$date,format="%Y-%m-%d")
remove(df)

player_statsMult <- dbGetQuery(con,"SELECT * FROM Player_Attributes")

team        <- dbGetQuery(con,"SELECT * FROM Team")
team <- team[,c("team_api_id","team_long_name","team_short_name")]

country        <- dbGetQuery(con,"SELECT * FROM Country")
league        <- dbGetQuery(con,"SELECT * FROM League")

df      <- dbGetQuery(con,"SELECT * FROM Player")
# removes player fifa id because in player and player stats
player <- subset(df, select = -c(id))
remove(df)


#removes time froma dates
player$birthday <- as.Date(player$birthday,format="%Y-%m-%d")
player_statsMult$date <- as.Date(player_statsMult$date,format="%Y-%m-%d")
# keeps only values for single player (not different dates)
player_statsMult <- player_statsMult[order(player_statsMult$player_api_id,player_statsMult$date,
                                           decreasing=T),]
player_stats <- player_statsMult [ !duplicated(player_statsMult$player_api_id), ]

player_stats <-  player_stats %>%
  subset(., select = -c(player_fifa_api_id,id)) %>%
  left_join(player, by = "player_api_id")
remove(player)

country <-  country %>%
  rename(country = name) %>%
  left_join(league, by = "id")
country <- country[,c("country","name","country_id")]

#replaces id by names
matches$country_id <- country$name[match(matches$country_id,country$country_id)]
matches$home_team_api_id <- team$team_long_name[match(matches$home_team_api_id,team$team_api_id)]
matches$away_team_api_id <- team$team_long_name[match(matches$away_team_api_id,team$team_api_id)]
colnames(matches)[which(colnames(matches) == "country_id")] <- "league"
colnames(matches)[which(colnames(matches) == "home_team_api_id")] <- "home_team"
colnames(matches)[which(colnames(matches) == "away_team_api_id")] <- "away_team"

# map_df: for each element of list do function and return dataframe
# all is list('goal', 'card', 'foulcommit', 'shoton', 'shotoff', 'cross', 'corner','possession')
columnToParse <- list('goal', 'shoton', 'shotoff');
incidents <- map_df(columnToParse, function(key) {
  matches %>%
    filter_(paste0('!is.na(', key, ')')) %>% 
    select_('id', key) %>%
    collect() %>% 
    rename_('value' = key) %>%
    pmap_df(function(id, value) {
      xml <- read_xml(value)
      df  <- xml %>%
        xml_find_all(paste0('/', key, '/value')) %>%
        map_df(node_to_dataframe, key)
      
      # Add the id of the game as 'foreign key' game_id
      if (nrow(df) > 0) {
        if (length(xml_find_all(xml, paste0('/', key, '/value/', key, '_type'))) > 0) {
          df %<>%
            rename(tmp = subtype1) %>%
            rename(subtype1 = subtype2) %>%
            rename(subtype2 = tmp)
        }
        df$game_id  <- id
      }
      return(df)
    })
})

# goals type: Normal: n / Disallowed Goals: dg / Own goal : o / retaken penalties: rp
# Penalty goals : p / NA: NA / penalty saved by goal keeper : npm /missed penalties: psm 
incidents$subtype1 <- recode(incidents$subtype1,
                             "n"="goal",
                             "p"="penalty",
                             "o"="own goal",
                             "dg"="disallowed goal",
                             "npm"="saved penalty",
                             "psm"="missed penalty",
                             "rp"="retaken penalty",
                             .missing="shot")

incidents$subtype2 <- recode(incidents$subtype2,.missing="shot")

incidents <- subset(incidents, !(subtype1 %in% c("retaken penalty","disallowed goal")))
incidents$type[incidents$subtype1=="saved penalty"]<-"shoton"
incidents$type[incidents$subtype1=="missed penalty"]<-"shotoff"

incidents$subtype1[incidents$subtype1 %in% c("saved penalty","missed penalty")] <- "penalty"
incidents$type[incidents$subtype1 == "penalty"]<-"goal"

incidents$subtype1[incidents$type=="goal" & !(incidents$subtype1 %in% c("penalty","own goal"))] <- 
  incidents$subtype2[incidents$type=="goal"& !(incidents$subtype1 %in% c("penalty","own goal"))]

incidents$subtype1 <- recode(incidents$subtype1,
                             "direct_freekick"="direct freekick",
                             "tap_in"="tap in",
                             "loose_ball"="loose ball",
                             "bicycle_kick"="bicycle",
                             "blocked_shot"="shot",
                             "blocked_header"="header",
                             "miss_kick"="shot",
                             "big chance header"="header",
                             "big chance bicycle"="bicycle",
                             "big chance crossbar"="crossbar",
                             "big chance post"="post",
                             "big chance shot"="shot",
                             "big chance volley"="volley",
                             "bad shot"="shot")
# removes NA
incidents %<>% mutate(elapsed_plus = ifelse(is.na(elapsed_plus), 0, elapsed_plus))
# changes coordinates to represent changes in pitch.
# Note:  pitch.l = 105 and pitch.w = 68: I used the transformations based on penalties
# in the data penalty is always lon=23 => lon field = 46 and lat = 8 or 62 => lat field =70
incidents %<>%
  mutate(lat = ifelse(elapsed <=45, lat, 70 - lat)* (105/70),
         lon = ifelse(elapsed <=45, lon, 46 - lon)* (68/46))

#joins games and events
incidents <-  incidents %>%
  left_join(matches[,c("id","league","season","stage","date","home_team","away_team","home_team_goal","away_team_goal")]
            , by = c("game_id" = "id"))
remove(matches)

#changes the type and subtypes variables

# renames team_id in incidents
incidents$team <- team$team_long_name[match(incidents$team,team$team_api_id)]
# was the team away or home
incidents %<>% mutate(isHome = ifelse(team == home_team, 'true', 'false'))
# against whom
incidents %<>% mutate(againstTeam = ifelse(team == home_team, away_team , home_team ))
incidents %<>% subset(select = -c(home_team,away_team,subtype2))

incidents$player1 %<>% as.integer(incidents$player1)

# PLAYER_STATS add info from incidents
# comment out because computes directly in d3
if (FALSE){
groupedIncidents = incidents %>% 
  group_by(player1,type,subtype1) %>% 
  tally() 

groupedIncidentHeader = groupedIncidents[groupedIncidents$subtype1 == "header", ] %>% 
  subset(select = -c(subtype1)) %>%
  spread(type,n,fill=0) 

colnames(groupedIncidentHeader) <- c("id","goalHeader","shotoffHeader","shotonHeader")

groupedIncidentShot = groupedIncidents[groupedIncidents$subtype1 != "header", ] %>% 
  group_by(player1,type) %>% 
  summarise(count = sum(n)) %>%
  spread(type,count,fill=0) 

colnames(groupedIncidentShot) <- c("id","goalShot","shotoffShot","shotonShot")

player_stats %<>% left_join(groupedIncidentShot, by = c("player_api_id" = "id")) 
player_stats %<>% left_join(groupedIncidentHeader, by = c("player_api_id" = "id"))

player_stats[,c("goalShot","shotoffShot","shotonShot","goalHeader","shotoffHeader","shotonHeader")
             ][is.na(player_stats[,c("goalShot","shotoffShot","shotonShot","goalHeader","shotoffHeader","shotonHeader")])] <- 0 

remove(groupedIncidents)
remove(groupedIncidentHeader)
remove(groupedIncidentShot)
}
write.csv(incidents,"data/incidents.csv", row.names = F)
write.csv(player_stats,"data/player_stats.csv", row.names = F)
write.csv(player_statsMult,"data/player_statsMult.csv", row.names = F)
write.csv(country,"data/country.csv", row.names = F)
write.csv(team,"data/team.csv", row.names = F)

