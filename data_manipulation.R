setwd('C:/Users/tiago/Desktop/TIAGO/Tiago UnB/8_semestre/tcc1')
library(pacman)
pacman::p_load(readxl,dplyr,stringr)
load('.RData')

players <- read.csv('Jogadores_1983-2022_2.csv')
teams <- read.csv('Times_1983-2022_2.csv')
df <- players#[players$year=='2022'|players$year=='2021',]
data <- teams#[teams$year=='2022'|teams$year=='2021',]

rm(players,tbls,teams,webpage)

# ARRUMAR JOGADORES DUPLICADOS NA MESMA TEMPORADA

tot <- filter(df,Tm=='TOT')
duplicados <- duplicated(df[,c("Player",'year')],fromLast = T)
df <- df[!duplicados,]
df <- rbind(df,tot)
rm(tot,duplicados)

# PADRONIZAR O NOME DOS TIMES

t2 <- sort(unique(df$Tm))
t1<- sort(unique(data$Team))

data$Team <- data$Team %>% str_replace(t1[1],t2[1]) %>%
  str_replace(t1[2],t2[2]) %>% str_replace(t1[3],t2[3]) %>%
  str_replace(t1[4],t2[5]) %>% str_replace(t1[5],t2[4]) %>%
  str_replace(t1[6],t2[6]) %>% str_replace(t1[7],t2[7]) %>%
  str_replace(t1[8],t2[8]) %>% str_replace(t1[9],t2[9]) %>%
  str_replace(t1[10],t2[10]) %>% str_replace(t1[11],t2[11]) %>%
  str_replace(t1[12],t2[12]) %>% str_replace(t1[13],t2[13]) %>%
  str_replace(t1[14],t2[14]) %>% str_replace(t1[15],t2[15]) %>%
  str_replace(t1[16],t2[16]) %>% str_replace(t1[17],t2[17]) %>%
  str_replace(t1[18],t2[18]) %>% str_replace(t1[19],t2[19]) %>%
  str_replace(t1[20],t2[20]) %>% str_replace(t1[21],t2[21]) %>%
  str_replace(t1[22],t2[22]) %>% str_replace(t1[23],t2[23]) %>%
  str_replace(t1[24],t2[24]) %>% str_replace(t1[25],t2[25]) %>%
  str_replace(t1[26],t2[26]) %>% str_replace(t1[27],t2[27]) %>%
  str_replace(t1[28],t2[28]) %>% str_replace(t1[29],t2[30]) %>%
  str_replace(t1[30],t2[31])

colnames(df)[5]<-'Team'

# ADICIONAR COLUNA DE VITÓRIAS E DERROTAS NA TEMPORADA

df <- left_join(df,data[,c('Team','year','W','L')],by=c('Team','year'))

rm(data,duplicados,i,link,t1,t2)

######################################################################2023#######################################################################
#
#players_2023
#teams_2023
#df23 <- players_2023
#data23 <- teams_2023
#
#rm(players_2023,tbls,teams_2023,webpage)
#
## ARRUMAR JOGADORES DUPLICADOS NA MESMA TEMPORADA
#
#tot <- filter(df23,Tm=='TOT')
#duplicados <- duplicated(df23[,c("Player",'year')],fromLast = T)
#df23 <- df23[!duplicados,]
#df23 <- rbind(df23,tot)
#rm(tot)
#
## PADRONIZAR O NOME DOS TIMES
#
#t2 <- sort(unique(df23$Tm))
#t1<- sort(unique(data23$Team))
#
#data23$Team <- data23$Team %>% str_replace(t1[1],t2[1]) %>%
#  str_replace(t1[2],t2[2]) %>% str_replace(t1[3],t2[3]) %>%
#  str_replace(t1[4],t2[5]) %>% str_replace(t1[5],t2[4]) %>%
#  str_replace(t1[6],t2[6]) %>% str_replace(t1[7],t2[7]) %>%
#  str_replace(t1[8],t2[8]) %>% str_replace(t1[9],t2[9]) %>%
#  str_replace(t1[10],t2[10]) %>% str_replace(t1[11],t2[11]) %>%
#  str_replace(t1[12],t2[12]) %>% str_replace(t1[13],t2[13]) %>%
#  str_replace(t1[14],t2[14]) %>% str_replace(t1[15],t2[15]) %>%
#  str_replace(t1[16],t2[16]) %>% str_replace(t1[17],t2[17]) %>%
#  str_replace(t1[18],t2[18]) %>% str_replace(t1[19],t2[19]) %>%
#  str_replace(t1[20],t2[20]) %>% str_replace(t1[21],t2[21]) %>%
#  str_replace(t1[22],t2[22]) %>% str_replace(t1[23],t2[23]) %>%
#  str_replace(t1[24],t2[24]) %>% str_replace(t1[25],t2[25]) %>%
#  str_replace(t1[26],t2[26]) %>% str_replace(t1[27],t2[27]) %>%
#  str_replace(t1[28],t2[28]) %>% str_replace(t1[29],t2[30]) %>%
#  str_replace(t1[30],t2[31])
#
#colnames(df23)[5]<-'Team'
#
## ADICIONAR COLUNA DE VITÓRIAS E DERROTAS NA TEMPORADA
#
#df23 <- left_join(df23,data23[,c('Team','year','W','L')],by=c('Team','year'))
#
#rm(data23,duplicados,i,link,t1,t2)
