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

write.csv(test,"data/player_stats3.csv", row.names = F)
