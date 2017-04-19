library(tidyr)
library(rvest)
library(stringr)

test <- read.csv("data/player_stats.csv",header = TRUE)

test$jersey <- 0
test$position <- 0
test$wage <- 0
test$value <- 0
test$country <- 0
test$team <- 0
for (i in 1:nrow(test)){
  id = test[i,"player_fifa_api_id"]
  
  url <- paste(c('http://sofifa.com/player/', id), collapse = "")
  webpage <- read_html(url)
  
  jerseyRaw <- webpage  %>% html_node("td:nth-child(3) li:nth-child(4)") %>%  html_text()
  test[i,"jersey"] <- gsub(" ", "", gsub("\nJersey number\n", "", jerseyRaw))
  
  positionRaw <- webpage  %>% html_node("td:nth-child(3) li:nth-child(3)") %>%  html_text()
  test[i,"position"] <- gsub(" ", "", gsub("\nPosition\n", "", positionRaw))
  
  wageRaw <- webpage  %>% html_node(".text-center:nth-child(4)") %>%  html_text()
  test[i,"wage"] <- substr(wageRaw, 9, nchar(wageRaw)-1)
  
  valueRaw <- webpage  %>% html_node(".text-center:nth-child(3)") %>%  html_text()
  test[i,"value"] <- substr(valueRaw, 10, nchar(valueRaw)-1)
  
  countryRaw <- webpage  %>% html_node("td:nth-child(4) li:nth-child(1)") %>%  html_text() 
  test[i,"country"] <- substr(countryRaw, 2, nchar(countryRaw)-1)
  
  teamRaw <- webpage  %>% html_node("td:nth-child(3) li:nth-child(1)") %>%  html_text() 
  test[i,"team"] <- substr(teamRaw, 2, nchar(teamRaw)-1)
  
}

test$position %<>% recode("CAM"="Centre Attacking Midfielder",
                          "CB"="Centre Back",
                          "CDM"="Center Defensive Midfielder",
                          "CF"="Centre Forward",
                          "CM"="Centre Midfield",
                          "GK"="Goalkeeper",
                          "LAM"="Left Attacking Midfielder",
                          "LB"="Left Back",
                          "LCB"="Left Centre Back",
                          "LCM"="Left Centre Midfielder",
                          "LDM"="Left Defensive Midfielder",
                          "LF"="Left Forward",
                          "LM"="Left Midfield",
                          "LS"="Left Striker",
                          "LW"="Left Wing",
                          "RAM"="Right Attacking Midfielder",
                          "RB"="Right Back",
                          "RCB"="Right Centre Back",
                          "RDM"="Right Defensive Midfielder",
                          "RES"="Reserve",
                          "RF"="Right Forward",
                          "RM"="Right Midfield",
                          "RS"="Right Striker",
                          "RW"="Right Wing",
                          "RWB"="Right Wing Back",
                          "ST"="Striker",
                          "SUB"="Substitute")

#test$wage <- gsub("M","e6",test$wage)
#test$wage <- gsub("K","e3",test$wage)
#test$value <- gsub("K","e3",test$value)
#test$value <- gsub("M","e6",test$value)
#test$wage <- gsub("e6","M",test$wage)
#test$wage <- gsub("e3","K",test$wage)
#test$value <- gsub("e3","K",test$value)
#test$value <- gsub("e6","M",test$value)

write.csv(test,"data/player_stats_sofifa.csv", row.names = F)
