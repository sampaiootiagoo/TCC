setwd('C:/Users/tiago/Desktop/TIAGO/Tiago UnB/8_semestre/tcc1')
library(pacman)
pacman::p_load(readxl,dplyr,ggplot2,stringr,xtable,rvest,data.table,curl)

#SCRAPPING DO SITE https://www.basketball-reference.com/leagues/NBA_2022_totals.html
# ESTATISTICAS DOS JOGADORES SOMENTE A PARTIR DE 1950 -> MVPS A PARTIR DE 1956

## MVPS https://www.basketball-reference.com/leagues/

webpage <- read_html("https://www.basketball-reference.com/leagues/")

mvps <- html_nodes(webpage, "table") %>% html_table() %>% .[[1]] %>% .[-1,]
names(mvps) <- c('Season','Lg','Champion','MVP','ROTY','Points','Rebounds','Assists','Win Shares')
mvps$year <- as.numeric(substr(mvps$Season, 1, 4)) + 1
head(mvps)

## JOGADORES

players <- list()

for(i in 1983:2023){
  link <- paste0('https://www.basketball-reference.com/leagues/NBA_',i,'_totals.html')
  webpage <- read_html(link)
  tbls <- html_nodes(webpage, "table") %>% html_table() %>% .[[1]]
  tbls$year <- i
  players <- data.table::rbindlist(list(players,tbls))
  Sys.sleep(2)
}

players_advanced <- list()

for(i in 1983:2023){
  link <- paste0('https://www.basketball-reference.com/leagues/NBA_',i,'_advanced.html')
  webpage <- read_html(link)
  tbls <- html_nodes(webpage, "table") %>% html_table() %>% .[[1]]
  tbls$year <- i
  players_advanced <- data.table::rbindlist(list(players_advanced,tbls))
  Sys.sleep(2)
}

## TIMES - WINS AND LOSSES https://www.basketball-reference.com/leagues/NBA_2019.html -> TAMBEM TEM STATS AVANÇADOS DOS TIMES
## 1951

#teams <- list()
#for(i in 1951:1952){
#  link <- paste0("https://www.basketball-reference.com/leagues/NBA_",i,".html")
#  webpage <- read_html(link)
#  east <- html_nodes(webpage, "table") %>% html_table() %>% .[[1]]
#  west <- html_nodes(webpage, "table") %>% html_table() %>% .[[2]]

#  colnames(east)[1] <- 'Team'
#  colnames(west)[1] <- 'Team'

#  east$year <- i
#  west$year <- i

#  all <- rbind(east,west)

#  teams <- data.table::rbindlist(list(teams,all))
#}

teams <- list()

# 1983 ate 2015 -> .[[9]]
# 2016 até 2022 -> .[[11]]

for(i in 1983:2023){
  if(i<2016){
    link <- paste0("https://www.basketball-reference.com/leagues/NBA_",i,".html")
    webpage <- read_html(link)
    winslosses <- html_nodes(webpage, "table") %>% html_table() %>% .[[9]]
    #winslosses <- winslosses[,1:5]
    winslosses$year <- i
    #winslosses <- winslosses[-1,]
    #names(winslosses) <- c('Rk','Team','Age','W','L','year')
    teams <- data.table::rbindlist(list(teams,winslosses))
    Sys.sleep(2)
  }
  else{
    link <- paste0("https://www.basketball-reference.com/leagues/NBA_",i,".html")
    webpage <- read_html(link)
    winslosses <- html_nodes(webpage, "table") %>% html_table() %>% .[[11]]
    #winslosses <- winslosses[,1:5]
    winslosses$year <- i
    #winslosses <- winslosses[-1,]
    #names(winslosses) <- c('Rk','Team','Age','W','L','year')
    teams <- data.table::rbindlist(list(teams,winslosses))
    Sys.sleep(2)
  }
  
}

names <- c('Rk','Team','Age','W','L','PW','PL','MOV','SOS','SRS','ORtg','DRtg','NRtg','Pace','FTr','3PAr','TS%','NA1','OeFG%','OTOV%','ORB%','Off FT/FGA','NA2','DeFG%',
           'DTOV%','DRB%','Def FT/FGA','NA3','Arena','Attend.','Attend./G')

colnames(teams)[-32]<-names

rm(tbls,winslosses)

players <- filter(players,Player!='Player')
players$Player <- str_replace(players$Player,'\\*','')
teams <- filter(teams,Rk!='Rk')
teams <- filter(teams,Team!='League Average')
teams$Team <- str_replace(teams$Team,'\\*','')
players_advanced <- filter(players_advanced,Player!='Player')
players_advanced$Player <- str_replace(players_advanced$Player,'\\*','')

#teste <- merge(players,players_advanced,by=c('Rk','Player','Pos','Age','Tm','G','MP'))
#df <- players_advanced %>% select(-Rk,-Player,-Pos,-Age,-Tm,-G,-V1,-V2,-year,-MP)
players <- cbind(players,select(players_advanced,-Rk,-Player,-Pos,-Age,-Tm,-G,-V1,-V2,-year,-MP))
rm(players_advanced)

write.csv(players,'Jogadores_1983-2022_2.csv',row.names = F)
write.csv(teams,'Times_1983-2022_2.csv',row.names = F)

#i <- 1950
#link <- paste0("https://www.basketball-reference.com/leagues/NBA_",i,".html")
#download.file(link, destfile = "scrapedpage.html", quiet=TRUE)
#webpage <- read_html('scrapedpage.html')
#webpage <- read_html(curl(link, handle = curl::new_handle("useragent" = "Mozilla/5.0")))
#winslosses <- html_nodes(webpage, "table") %>% html_table() %>% .[[6]]


######################################################################2023#######################################################################
#webpage <- read_html("https://www.basketball-reference.com/leagues/")
#
#players_2023 <- list()
#
#for(i in 2023){
#  link <- paste0('https://www.basketball-reference.com/leagues/NBA_',i,'_totals.html')
#  webpage <- read_html(link)
#  tbls <- html_nodes(webpage, "table") %>% html_table() %>% .[[1]]
#  tbls$year <- i
#  players_2023 <- data.table::rbindlist(list(players_2023,tbls))
#  Sys.sleep(2)
#}
#
#players_advanced_2023 <- list()
#
#for(i in 2023){
#  link <- paste0('https://www.basketball-reference.com/leagues/NBA_',i,'_advanced.html')
#  webpage <- read_html(link)
#  tbls <- html_nodes(webpage, "table") %>% html_table() %>% .[[1]]
#  tbls$year <- i
#  players_advanced_2023 <- data.table::rbindlist(list(players_advanced_2023,tbls))
#  Sys.sleep(2)
#}
#
#teams_2023 <- list()
#
## 1983 ate 2015 -> .[[9]]
## 2016 até 2022 -> .[[11]]
#
#for(i in 2023){
#  if(i<2016){
#    link <- paste0("https://www.basketball-reference.com/leagues/NBA_",i,".html")
#    webpage <- read_html(link)
#    winslosses <- html_nodes(webpage, "table") %>% html_table() %>% .[[9]]
#    #winslosses <- winslosses[,1:5]
#    winslosses$year <- i
#    #winslosses <- winslosses[-1,]
#    #names(winslosses) <- c('Rk','Team','Age','W','L','year')
#    teams_2023 <- data.table::rbindlist(list(teams_2023,winslosses))
#    Sys.sleep(2)
#  }
#  else{
#    link <- paste0("https://www.basketball-reference.com/leagues/NBA_",i,".html")
#    webpage <- read_html(link)
#    winslosses <- html_nodes(webpage, "table") %>% html_table() %>% .[[11]]
#    #winslosses <- winslosses[,1:5]
#    winslosses$year <- i
#    #winslosses <- winslosses[-1,]
#    #names(winslosses) <- c('Rk','Team','Age','W','L','year')
#    teams_2023 <- data.table::rbindlist(list(teams_2023,winslosses))
#    Sys.sleep(2)
#  }
#}
#
#names <- c('Rk','Team','Age','W','L','PW','PL','MOV','SOS','SRS','ORtg','DRtg','NRtg','Pace','FTr','3PAr','TS%','NA1','OeFG.','OTOV.','ORB.','Off FT/FGA','NA2','DeFG.',
#           'DTOV.','DRB.','Def FT/FGA','NA3','Arena','Attend.','Attend./G')
#
#colnames(teams_2023)[-32]<-names
#
#rm(tbls,winslosses)
#
#players_2023 <- filter(players_2023,Player!='Player')
#players_2023$Player <- str_replace(players_2023$Player,'\\*','')
#teams_2023 <- filter(teams_2023,Rk!='Rk')
#teams_2023 <- filter(teams_2023,Team!='League Average')
#teams_2023$Team <- str_replace(teams_2023$Team,'\\*','')
#players_advanced_2023 <- filter(players_advanced_2023,Player!='Player')
#players_advanced_2023$Player <- str_replace(players_advanced_2023$Player,'\\*','')
#
##teste <- merge(players,players_advanced,by=c('Rk','Player','Pos','Age','Tm','G','MP'))
##df <- players_advanced %>% select(-Rk,-Player,-Pos,-Age,-Tm,-G,-V1,-V2,-year,-MP)
#players_2023 <- cbind(players_2023,select(players_advanced_2023,-Rk,-Player,-Pos,-Age,-Tm,-G,-V1,-V2,-year,-MP))
#rm(players_advanced_2023)
#