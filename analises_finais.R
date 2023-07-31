setwd('C:/Users/tiago/Desktop/TIAGO/Tiago UnB/8_semestre/tcc1')
library(pacman)
pacman::p_load(readxl,dplyr,tidyverse,gplots,GGally,psych,lavaan,factoextra,NbClust,gclus) #hclust
load('.RData')
'%!in%' <- function(x,y)!('%in%'(x,y))

#-----------------------------------------------------------ANÁLISES FINAIS------------------------------------------------------

## BOXPLOT DA HABILIDADE
teste$Cluster <- as.factor(teste$Cluster)
#teste$INDIVIDUAIS <- INDIVIDUAIS
#teste$APROVEITAMENTO <- APROVEITAMENTO
#teste$CHUTE <- CHUTE

ggplot(teste, aes(y=HABILIDADE, x=(Cluster),fill=(Cluster))) +
  geom_boxplot(fill=c("#003366","#A11D21"),width = 0.5) +
  scale_fill_manual(values=c("#003366","#A11D21"))+
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="Cluster", y="Habilidade") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black")) 

aggregate(HABILIDADE~Cluster,data = teste, summary)
aggregate(HABILIDADE~Cluster,data = teste, sd)

shapiro.test(teste$HABILIDADE)
var.test(HABILIDADE~Cluster,data = teste)

kruskal.test(HABILIDADE~Cluster,data = teste)

# HABILIDADE E WS

ggplot(teste, aes(x=WS, y=HABILIDADE)) + geom_point(aes(colour=Cluster)) +
  scale_colour_manual(name="Cluster", values = c("#003366", "#A11D21"), 
                      labels = c("1", "2"))+
  labs(x="Win Shares (WS)", y="Habilidade")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(legend.position="top")


## BOXPLOT DOS FATORES E WS

# boxplots dos fatores
fat1 <- ggplot(teste, aes(y=INDIVIDUAIS, x=Cluster,fill=Cluster)) +
  geom_boxplot(fill=c("#003366","#A11D21"),width = 0.5) +
  scale_fill_manual(values=c("#003366","#A11D21"))+
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="Cluster", y="Estatísticas Individuais") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))
fat2 <- ggplot(teste, aes(y=APROVEITAMENTO, x=Cluster,fill=Cluster)) +
  geom_boxplot(fill=c("#003366","#A11D21"),width = 0.5) +
  scale_fill_manual(values=c("#003366","#A11D21"))+
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="Cluster", y="Estatísticas de Aproveitamento") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))
fat3 <- ggplot(teste, aes(y=CHUTE, x=Cluster,fill=Cluster)) +
  geom_boxplot(fill=c("#003366","#A11D21"),width = 0.5) +
  scale_fill_manual(values=c("#003366","#A11D21"))+
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="Cluster", y="Estatísticas de Chute") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))
fat4 <- ggplot(teste, aes(y=WS, x=Cluster,fill=Cluster)) +
  geom_boxplot(fill=c("#003366","#A11D21"),width = 0.5) +
  scale_fill_manual(values=c("#003366","#A11D21"))+
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="Cluster", y="Win Shares (WS)") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))
gridExtra::grid.arrange(fat1,fat2,fat3,fat4)

kruskal.test(teste$INDIVIDUAIS,as.factor(teste$Cluster)) #rejeita -> tem diferença
kruskal.test(teste$APROVEITAMENTO,as.factor(teste$Cluster)) #rejeita -> tem diferença
kruskal.test(teste$CHUTE,as.factor(teste$Cluster)) #rejeita -> tem diferença
kruskal.test(teste$WS,as.factor(teste$Cluster)) #rejeita -> tem diferença
kruskal.test(teste$HABILIDADE,as.factor(teste$Cluster)) #rejeita -> tem diferença

aggregate(INDIVIDUAIS~Cluster,teste,mean)
aggregate(APROVEITAMENTO~Cluster,teste,mean)
aggregate(CHUTE~Cluster,teste,mean)
aggregate(WS~Cluster,teste,mean)

#- TROCAR O CLUSTERING E ANALISES FINAIS PARA OS DADOS DE 2023 ?

#- PEGAR OS DADOS DE MVP DAS TEMPORADAS, CALCULAR A HABILIDADE DE TODOS OS JOGADORES E VER QUANTOS MVPS "ACERTAMOS"

## MVPS
pacman::p_load(rvest,data.table,curl)
webpage <- read_html("https://www.basketball-reference.com/leagues/")
mvps <- html_nodes(webpage, "table") %>% html_table() %>% .[[1]] %>% .[-1,]
names(mvps) <- c('Season','Lg','Champion','MVP','ROTY','Points','Rebounds','Assists','Win Shares')
mvps$year <- as.numeric(substr(mvps$Season, 1, 4)) + 1
head(mvps)
rm(webpage)

## HABILIDADES
df_mvps <- df %>% select(-W,-L)
names <- df_mvps$Player
anos <- df_mvps$year
df_mvps <- as.data.frame(lapply(df_mvps[,-c(1,2,3,5,31)], as.numeric))
df_mvps <- df_mvps[,-colunas2]
df_mvps$Player <- names
df_mvps$year <- anos
df_mvps <- as.data.frame((na.exclude(df_mvps))) 
df_mvps[,-c(17,18)] <- scale(df_mvps[,-c(17,18)]) # USAR OU NAO O SCALE
rm(names,anos)

ind <- rowSums(t(apply(df_mvps[,-c(17,18)], 1, function(x)x*loadings$INDIVIDUAIS)))
apr <- rowSums(t(apply(df_mvps[,-c(17,18)], 1, function(x)x*loadings$APROVEITAMENTO)))
chu <- rowSums(t(apply(df_mvps[,-c(17,18)], 1, function(x)x*loadings$CHUTE)))
hab <- 1*ind + 1.922*apr + 1.310*chu

df_mvps$HABILIDADE <- hab 

test <- aggregate(HABILIDADE~year,data=df_mvps,FUN = max)
test2 <- df_mvps %>% select(Player,year,HABILIDADE)
test <- left_join(test,test2,by=c('year','HABILIDADE'))
df_mvps <- test
rm(test,test2)

## JUNTANDO OS DOIS

str(mvps)
mvps <- filter(mvps,year>=1983)
nrow(mvps)
nrow(df_mvps)

df_mvps <- df_mvps[order(df_mvps$year,decreasing = T),]

df_mvps$MVP <- mvps$MVP
df_mvps$MVP <- sub("^\\S+\\s+", '', df_mvps$MVP)
df_mvps$Player <- sub("^\\S+\\s+", '', df_mvps$Player)

table(df_mvps$Player==df_mvps$MVP)

df_mvps$Resultado <- df_mvps$Player==df_mvps$MVP

## HABILIDADE POR TIMES

#df_times <- df %>% select(-W,-L)
#names <- df_times$Player
#anos <- df_times$year
#Team <- df$Team
#df_times <- as.data.frame(lapply(df_times[,-c(1,2,3,5,31)], as.numeric))
#df_times <- df_times[,-colunas2]
#df_times$Player <- names
#df_times$year <- anos
#df_times$Team <- Team
#df_times <- as.data.frame((na.exclude(df_times))) 
##df_times[,-c(17,18,19)] <- scale(df_times[,-c(17,18,19)]) # USAR OU NAO O SCALE
#rm(names,anos,Team)
#
#ind <- rowSums(t(apply(df_times[,-c(17,18,19)], 1, function(x)x*loadings$INDIVIDUAIS)))
#apr <- rowSums(t(apply(df_times[,-c(17,18,19)], 1, function(x)x*loadings$APROVEITAMENTO)))
#chu <- rowSums(t(apply(df_times[,-c(17,18,19)], 1, function(x)x*loadings$CHUTE)))
#hab <- 1*ind + 1.922*apr + 1.310*chu
#
#df_times$HABILIDADE <- hab 

df_times <- aggregate(HABILIDADE ~ Time + Temporada, FUN = mean, data=tabela_serie)

table(df_times$Time)
teams <- c('LAL','BOS','GSW','CHI','SAS','CLE','MIA','DEN','PHI','MIL')
teams2 <- c('TOR','LAL','MIL','GSW','DEN')

ggplot(filter(df_times,Time %in% teams & Temporada >= 2014), aes(x=Temporada,y=HABILIDADE,group=Time,colour=Time)) +
  geom_line(size=1) + geom_point(size=2) +
  scale_colour_manual(name="Time", values = c('purple','green','gold','red','grey','darkred',
                                                 'black','darkblue','blue','darkgreen'), labels = teams)+
  labs(x="Ano", y="Habilidade média") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(legend.position="top")

ggplot(filter(df_times,Time %in% teams2 & Temporada >= 2019), aes(x=Temporada,y=HABILIDADE,group=Time,colour=Time)) +
  geom_line(size=1) + geom_point(size=2) +
  scale_colour_manual(name="Time", values = c('red','purple','green','yellow','blue'), labels = teams2)+
  labs(x="Ano", y="Habilidade média") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(legend.position="top")

tabela_serie2 <- aggregate(HABILIDADE ~ Temporada, FUN = mean, data=tabela_serie)

ggplot(filter(tabela_serie2,Temporada>1986), aes(x=Temporada,y=HABILIDADE)) +
  geom_line(size=1) + geom_point(size=2) +
  #scale_colour_manual(name="Time", values = c('red','purple','green','yellow','blue'), labels = teams2)+
  labs(x="Ano", y="Habilidade média") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(legend.position="top")


## MÉDIAS POR FATOR E POR CLUSTER

tabela_medias <- teste %>% pivot_longer(cols = c('INDIVIDUAIS','APROVEITAMENTO','CHUTE'),
                                        names_to = 'Fator',values_to = 'Valor')


aggregate(Valor ~ Cluster + Fator, data = tabela_medias, FUN = mean)
aggregate(HABILIDADE ~ Cluster, data = teste, FUN = mean)
aggregate(WS ~ Cluster, data = teste, FUN = mean)


## GRÁFICO DE HABILIDADE E CLUSTER POR TIME, POSIÇÃO, ANO


##-----------------------------------------TABELA COM PREDICOES PARA CADA ANO------------------------------------------------

tabela_top10 <- data.frame('Nome'=NA,'HABILIDADE'=NA,'Temporada'=NA)
tabela_serie <- data.frame('Nome'=NA,'HABILIDADE'=NA,'Temporada'=NA,'Time'=NA)

for(i in 1983:2023){
  df2 <- df %>% filter(year==i) %>% select(-W,-L)
  CFAquanti <- as.data.frame(lapply(df2[,-c(1,2,3,5,31)], as.numeric))
  CFAquanti <- as.data.frame(scale(CFAquanti)) # TIRANDO IDADE PQ NAO SE JUNTOU A NENHUM FATOR
  CFAquali <- df2[,c(3,5)] #ROW, YEAR E NOME FORAM RETIRADOS
  CFArem <- CFAquanti[,-colunas2] #%>% select(-Age,-W,-L)
  rm(CFAquanti,CFAquali)
  
  hip_modelo <- '
  INDIVIDUAIS =~ PTS + TOV + AST + TRB + VORP + STL + BLK + USG.
  APROVEITAMENTO =~ eFG. + FG. + TS. + X2P. + PER + BPM
  CHUTE =~ X3P. + FT.
  HABILIDADE =~ INDIVIDUAIS + APROVEITAMENTO + CHUTE'
  
  modeloexp_CFA <- lavaan::cfa(model=hip_modelo,data=CFArem)
  loadings <- inspect(modeloexp_CFA,"est")$lambda
  loadings <- as.data.frame(loadings)
  
  INDIVIDUAIS <- rowSums(t(apply(CFArem, 1, function(x)x*loadings$INDIVIDUAIS)))
  APROVEITAMENTO <- rowSums(t(apply(CFArem, 1, function(x)x*loadings$APROVEITAMENTO)))
  CHUTE <- rowSums(t(apply(CFArem, 1, function(x)x*loadings$CHUTE)))
  HABILIDADE <- 1*INDIVIDUAIS + 1.922*APROVEITAMENTO + 1.310*CHUTE
  #HABILIDADE <- summary(modeloexp_CFA)[[5]][c(17,18,19),5]*c(INDIVIDUAIS,APROVEITAMENTO,CHUTE)
  
  qnames <- as.data.frame(lapply(df2[,-c(1,2,3,5,31)], as.numeric))
  qnames$player <- df2$Player
  qnames$team <- df2$Team
  qnames$year <- df2$year
  #qnames <- as.data.frame((na.exclude(qnames))) # TINHA UM SCALE AQUI
  
  Nome <- qnames$player
  WS <- qnames$WS
  Temporada <- qnames$year
  Time <- qnames$team
  tabela <- data.frame(Nome,WS,HABILIDADE,Temporada,Time)
  tabela <- tabela %>% arrange(desc(HABILIDADE))
  tabela_serie <- rbind(select(tabela,-WS),tabela_serie)
  tabela <- tabela[1:5,c(1,3,4)]
  tabela_top10 <- rbind(tabela_top10,tabela)
  rm(tabela)
}

tabela_top10 %>% na.omit() %>% arrange(desc(Temporada)) %>% head(10)

tabela_top10 %>% na.omit() %>% arrange(desc(Temporada)) %>% filter(Temporada %in% c(2015,2018,2019,2020))


##------------------------------TABELA COM PREDICOES PARA CADA ANO COM O SEGUNDO MODELO--------------------------------------

tabela_top10_7 <- data.frame('Nome'=NA,'HABILIDADE'=NA,'Temporada'=NA)

for(i in 1983:2023){
  df2 <- df %>% filter(year==i) %>% select(-W,-L)
  CFAquanti <- as.data.frame(lapply(df2[,-c(1,2,3,5,31)], as.numeric))
  CFAquanti <- as.data.frame(scale(CFAquanti)) # TIRANDO IDADE PQ NAO SE JUNTOU A NENHUM FATOR
  CFAquali <- df2[,c(3,5)] #ROW, YEAR E NOME FORAM RETIRADOS
  CFArem <- CFAquanti[,-colunas2] #%>% select(-Age,-W,-L)
  rm(CFAquanti,CFAquali)
  
  hip_modelo2 <- '
  MR1 =~ AST + TOV + PTS + STL + VORP
  MR2 =~ FG. + eFG. + TS. + X2P. + PER
  MR3 =~ X3P.
  MR4 =~ USG.
  MR5 =~ FT.
  MR6 =~ BLK + TRB
  MR7 =~ BPM
  HABILIDADE =~ MR1 + MR2 + MR3 + MR4 + MR5 + MR6 + MR7'
  
  modeloexp_CFA2 <- lavaan::cfa(model=hip_modelo2,data=CFArem)
  loadings2 <- inspect(modeloexp_CFA2,"est")$lambda
  loadings2 <- as.data.frame(loadings2)
  
  MR1 <- rowSums(t(apply(CFArem, 1, function(x)x*loadings2$MR1)))
  MR2 <- rowSums(t(apply(CFArem, 1, function(x)x*loadings2$MR2)))
  MR3 <- rowSums(t(apply(CFArem, 1, function(x)x*loadings2$MR3)))
  MR4 <- rowSums(t(apply(CFArem, 1, function(x)x*loadings2$MR4)))
  MR5 <- rowSums(t(apply(CFArem, 1, function(x)x*loadings2$MR5)))
  MR6 <- rowSums(t(apply(CFArem, 1, function(x)x*loadings2$MR6)))
  MR7 <- rowSums(t(apply(CFArem, 1, function(x)x*loadings2$MR7)))
  HABILIDADE <- 1*MR1 + 0.795*MR2 + 0.527*MR3 + 0.533*MR4 + 0.360*MR5 + 0.777*MR6 + 1.028*MR7
  #HABILIDADE <- summary(modeloexp_CFA2)[[5]][c(17:23),5]*c(MR1,MR2,MR3,MR4,MR5,MR6,MR7)
  
  qnames <- as.data.frame(lapply(df2[,-c(1,2,3,5,31)], as.numeric))
  qnames$player <- df2$Player
  qnames$year <- df2$year
  #qnames <- as.data.frame((na.exclude(qnames))) # TINHA UM SCALE AQUI
  
  Nome <- qnames$player
  WS <- qnames$WS
  Temporada <- qnames$year
  tabela <- data.frame(Nome,WS,HABILIDADE,Temporada)
  tabela <- tabela %>% arrange(desc(HABILIDADE))
  tabela <- tabela[1,c(1,3,4)]
  tabela_top10_7 <- rbind(tabela_top10_7,tabela)
  rm(tabela)
}

tabela_top10_7 %>% na.omit() %>% arrange(desc(Temporada)) %>% head(10)

#---------------------------CLUSTER E HABILIDADE--------------------------------------------------------
# DADOS DE 2023
colnames(teste_2023)[c(1,4)] <- c('Player','year')
head(teste_2023)

teste_2023 <- left_join(x = teste_2023,y = select(df,Team,Pos,year,Player),by = c('Player','year'))
head(teste_2023)

teste_2023$Cluster2 <- str_replace(teste_2023$Cluster,'1','1a')
teste_2023$Cluster2 <- str_replace(teste_2023$Cluster2,'2','1')
teste_2023$Cluster2 <- str_replace(teste_2023$Cluster2,'1a','2')
teste_2023$Cluster2 <- factor(teste_2023$Cluster2,levels = c('1','2'))

table(teste_2023$Cluster2)
table(teste_2023$Cluster)

# TIME

#data <- aggregate(HABILIDADE~Team,teste_2023, mean)
g1 <- ggplot(filter(teste_2023,Team!='TOT'), aes(y=HABILIDADE, x=Team)) +
  geom_boxplot(width = 0.5,fill=c('lightblue')) +
  scale_fill_manual(values='lightblue')+
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="Time", y="Habilidade Média") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=5.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black")) + coord_flip()

data <- as.data.frame(table(teste_2023$Team,teste_2023$Cluster2))
g2 <- ggplot(filter(data,Var1!='TOT'), aes(x=Var1, y=Freq,fill=Var2)) + geom_bar(stat="identity",position="fill") +
  scale_fill_manual(name="Cluster", values=c("#003366","#A11D21"))+
  labs(x="Time", y="Frequência") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=5.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(legend.position="right") + coord_flip()



# POSICAO
#filter(data,Pos %in% c('C','PF','SF','SG','PG'))
#filter(data,Var1 %in% c('C','PF','SF','SG','PG'))
#data <- aggregate(HABILIDADE~Pos,teste_2023, mean)
g3 <- ggplot(teste_2023, aes(y=HABILIDADE, x=Pos)) +
  geom_boxplot(width = 0.5,fill=c('lightblue')) +
  scale_fill_manual(values='lightblue')+
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="Posição", y="Habilidade Média") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black")) + coord_flip()

data <- as.data.frame(table(teste_2023$Pos,teste_2023$Cluster2))
g4 <- ggplot(data, aes(x=Var1, y=Freq,fill=Var2)) + geom_bar(stat="identity",position="fill") +
  scale_fill_manual(name="Cluster", values=c("#003366","#A11D21"))+
  labs(x="Posição", y="Frequência") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(legend.position="right") + coord_flip()

gridExtra::grid.arrange(g1,g2,g3,g4)


#---------------------------CURIOSIDADES--------------------------------------------------------

df_mvps$MVP <- mvps$MVP
df_mvps$MVP <- sub("^\\S+\\s+", '', df_mvps$MVP)
df_mvps$Player <- sub("^\\S+\\s+", '', df_mvps$Player)

shapiro.test(as.numeric(str_extract(mvps$`Win Shares`,'[0-9][0-9]*\\.[0-9]')))

ue <- filter(df,year==1984|year==1986) %>% select(-W,-L)
ue <- filter(ue,Player=='Larry Bird')
ue <- as.data.frame(lapply(ue[,-c(1,2,3,5,31)], as.numeric))
ue <- as.data.frame((na.exclude(ue))) # TINHA UM SCALE AQUI
ue <- ue[,-colunas2]
