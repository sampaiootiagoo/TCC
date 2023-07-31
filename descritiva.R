setwd('C:/Users/tiago/Desktop/TIAGO/Tiago UnB/8_semestre/tcc1')
library(pacman)
pacman::p_load(readxl,dplyr,ggplot2,stringr,xtable,rvest,data.table,curl,ggcorrplot,gplots,RColorBrewer)
load('.RData')
'%!in%' <- function(x,y)!('%in%'(x,y))

# COLUNAS
# Rk - rank
# Pos - Position 1
# Tm - Team 1
# year 1
# Age 2
# G - Games 2
# GS - Games started 2
# MP - Minutes played 2
# FG - Field Goals 2
# FGA - Field Goals attempted 3
# FG% - Field Goals percentage 3
# 3P - 3 points 3
# 3PA - 3 points attempted 3
# 3P% - 3 points percentage 3
# 2P - 2 points 4
# 2PA - 2 points attempted 4
# 2P% - 2 points percentage 4
# eFG% - Effective field goal percentage (ajustado p 2 e 3 pontos) 4
# FT - Free throws 4
# FTA - Free throws attempted 5
# FT% - Free throws percentage 5
# ORB - offensive rebound 5
# DRB - defensive rebound 5
# TRB - total rebounds 5
# AST - assists 6
# STL - steals 6
# BLK - blocks 6
# TOV - turn overs 6
# PF - personal fouls 6
# PTS - points 7
# PER - player efficiency rating 7
# TS. - true shooting percentage 7
# OWS - offensive win share 7
# DWS - deffensive win share 7
# WS - win share 8
# OBPM - offensive box plus minus 8
# DBPM - deffensive box plus minus 8
# BPM - box plus minus 8
# VORP 8


# FILTROS
#df <- filter(df,Pos %in% c('C','PF','SF','SG','PG'))

# DESCRITIVA

## Position

Fr<-table(df$Pos)
Pr<-as.data.frame(round(prop.table(Fr), digits=4)*100)
colnames(Pr)<-c("Var1", "Pr")
comp<-merge(Fr, Pr, by="Var1")
comp$Pr<-paste(gsub("\\.",",",comp$Pr), "%", sep= '')

pos <- ggplot(comp, aes(x=reorder(Var1,Freq), y=Freq, label=Pr)) +
  geom_bar(stat="identity", fill="lightblue") +
  geom_text(vjust=0.5, size=2,hjust=-.05)+
  labs(x="Posição", y="Frequência") +
  theme_bw() + scale_y_continuous(limits = c(0,5000))+
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=4.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) + coord_flip()

## Team

Fr<-table(filter(df,Team!='TOT')$Team)
Pr<-as.data.frame(round(prop.table(Fr), digits=4)*100)
colnames(Pr)<-c("Var1", "Pr")
comp<-merge(Fr, Pr, by="Var1")
comp$Pr<-paste(gsub("\\.",",",comp$Pr), "%", sep= '')

team <- ggplot(comp, aes(x=reorder(Var1,Freq), y=Freq, label=Pr)) +
  geom_bar(stat="identity", fill="lightblue") +
  geom_text(vjust=0.5, size=2,hjust=-.05)+
  labs(x="Times", y="Frequência") +
  theme_bw() + scale_y_continuous(limits = c(0,1000))+
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=4.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) + coord_flip()

## year

Fr<-table(df$year)
Pr<-as.data.frame(round(prop.table(Fr), digits=4)*100)
colnames(Pr)<-c("Var1", "Pr")
comp<-merge(Fr, Pr, by="Var1")
comp$Pr<-paste(gsub("\\.",",",comp$Pr), "%", sep= '')

year <- ggplot(comp, aes(x=reorder(Var1,Var1), y=Freq, label=Pr)) +
  geom_bar(stat="identity", fill="lightblue") +
  geom_text(vjust=0.5, size=2,hjust=-.05)+
  labs(x="Ano", y="Frequência") +
  theme_bw() + scale_y_continuous(limits = c(0,1000))+
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=4.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) + coord_flip()

gridExtra::grid.arrange(pos,team,year)

## AGE

age <- ggplot(df, aes(x=factor(""), y=Age)) +
  geom_boxplot(fill=c("lightblue"), width = 0.5) +
  guides(fill='none') +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="Idade dos jogadores")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))

ggplot(df, aes(x=Pos, y=Age)) +
  geom_boxplot(fill=c("lightblue"), width = 0.5) +
  guides(fill='none') +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="Idade dos jogadores")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))

## Games 

g <- ggplot(df, aes(x=factor(""), y=G)) +
  geom_boxplot(fill=c("lightblue"), width = 0.5) +
  guides(fill='none') +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="Jogos")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))

gs <- ggplot(df, aes(x=factor(""), y=GS)) +
  geom_boxplot(fill=c("lightblue"), width = 0.5) +
  guides(fill='none') +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="Jogos começados")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))

# Minutos jogados

mp <- ggplot(df, aes(x=factor(""), y=MP)) +
  geom_boxplot(fill=c("lightblue"), width = 0.5) +
  guides(fill='none') +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="Minutos jogados")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))

# FG

fg <- ggplot(df, aes(x=factor(""), y=FG)) +
  geom_boxplot(fill=c("lightblue"), width = 0.5) +
  guides(fill='none') +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="Cestas convertidas")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))

# FGA

fga <- ggplot(df, aes(x=factor(""), y=FGA)) +
  geom_boxplot(fill=c("lightblue"), width = 0.5) +
  guides(fill='none') +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="Tentativas de cestas")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))

gridExtra::grid.arrange(age,g,gs,mp,fg,fga)

# FG% 

fg. <- ggplot(df, aes(x=factor(""), y=FG.)) +
  geom_boxplot(fill=c("lightblue"), width = 0.5) +
  guides(fill='none') +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="% de cestas de quadra")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))


# 3P

p3 <- ggplot(df, aes(x=factor(""), y=X3P)) +
  geom_boxplot(fill=c("lightblue"), width = 0.5) +
  guides(fill='none') +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="Pontuação em cestas de 3")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))


# 3PA

p3a <- ggplot(df, aes(x=factor(""), y=X3PA)) +
  geom_boxplot(fill=c("lightblue"), width = 0.5) +
  guides(fill='none') +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="Tentativas em cestas de 3")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))


# 3P% 

p3. <- ggplot(df, aes(x=factor(""), y=X3P.)) +
  geom_boxplot(fill=c("lightblue"), width = 0.5) +
  guides(fill='none') +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="% de cesta de 3")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))


# 2P

p2 <- ggplot(df, aes(x=factor(""), y=X2P)) +
  geom_boxplot(fill=c("lightblue"), width = 0.5) +
  guides(fill='none') +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="Pontuação em cestas de 2")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))


# 2PA

p2a <- ggplot(df, aes(x=factor(""), y=X2PA)) +
  geom_boxplot(fill=c("lightblue"), width = 0.5) +
  guides(fill='none') +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="Tentativas em cestas de 2")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))

gridExtra::grid.arrange(fg.,p3,p3a,p3.,p2,p2a)

# 2P% 

p2. <- ggplot(df, aes(x=factor(""), y=X2P.)) +
  geom_boxplot(fill=c("lightblue"), width = 0.5) +
  guides(fill='none') +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="% de cesta de 2")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))


# EFG%

efg. <- ggplot(df, aes(x=factor(""), y=eFG.)) +
  geom_boxplot(fill=c("lightblue"), width = 0.5) +
  guides(fill='none') +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="% de cestas efetivas")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))


# FT

ft <- ggplot(df, aes(x=factor(""), y=FT)) +
  geom_boxplot(fill=c("lightblue"), width = 0.5) +
  guides(fill='none') +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="Lance livres feitos")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))


# FTA

fta <- ggplot(df, aes(x=factor(""), y=FTA)) +
  geom_boxplot(fill=c("lightblue"), width = 0.5) +
  guides(fill='none') +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="Tentativas de lance livre")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))

# FT%

ft. <- ggplot(df, aes(x=factor(""), y=FT.)) +
  geom_boxplot(fill=c("lightblue"), width = 0.5) +
  guides(fill='none') +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="% de acerto de lance livre")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))


# ORB

orb <- ggplot(df, aes(x=factor(""), y=ORB)) +
  geom_boxplot(fill=c("lightblue"), width = 0.5) +
  guides(fill='none') +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="Rebotes ofensivos")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))

gridExtra::grid.arrange(p2.,efg.,ft,fta,ft.,orb)

# DRB

drb <- ggplot(df, aes(x=factor(""), y=DRB)) +
  geom_boxplot(fill=c("lightblue"), width = 0.5) +
  guides(fill='none') +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="Rebotes defensivos")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))


# TRB

trb <- ggplot(df, aes(x=factor(""), y=TRB)) +
  geom_boxplot(fill=c("lightblue"), width = 0.5) +
  guides(fill='none') +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="Rebotes totais")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))


# AST


ast <- ggplot(df, aes(x=factor(""), y=AST)) +
  geom_boxplot(fill=c("lightblue"), width = 0.5) +
  guides(fill='none') +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="Assistências")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))


# STL

stl <- ggplot(df, aes(x=factor(""), y=STL)) +
  geom_boxplot(fill=c("lightblue"), width = 0.5) +
  guides(fill='none') +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="Roubos de bola")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))



# BLK

blk <- ggplot(df, aes(x=factor(""), y=BLK)) +
  geom_boxplot(fill=c("lightblue"), width = 0.5) +
  guides(fill='none') +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="Tocos")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))



# TOV

tov <- ggplot(df, aes(x=factor(""), y=TOV)) +
  geom_boxplot(fill=c("lightblue"), width = 0.5) +
  guides(fill='none') +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="Turnovers")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))

gridExtra::grid.arrange(drb,trb,ast,stl,blk,tov)

# PF

pf <- ggplot(df, aes(x=factor(""), y=PF)) +
  geom_boxplot(fill=c("lightblue"), width = 0.5) +
  guides(fill='none') +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="Faltas pessoais")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))


# PTS

pts <- ggplot(df, aes(x=factor(""), y=PTS)) +
  geom_boxplot(fill=c("lightblue"), width = 0.5) +
  guides(fill='none') +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="Pontos totais")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))

# BPM

bpm <- ggplot(df, aes(x=factor(""), y=BPM)) +
  geom_boxplot(fill=c("lightblue"), width = 0.5) +
  guides(fill='none') +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="Saldo do jogador")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))

# VORP

vorp <- ggplot(df, aes(x=factor(""), y=VORP)) +
  geom_boxplot(fill=c("lightblue"), width = 0.5) +
  guides(fill='none') +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="VORP")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))

gridExtra::grid.arrange(pf,pts,bpm,vorp,fg,fga)

# MAPA DE CALOR P CORRELACOES

bd <- df %>% filter(year %in% c(2021,2022))
bd <- bd %>% select(FG.,X3P.,X2P.,eFG.,FT.,TRB,AST,STL,BLK,TOV,PTS,PER,TS.,USG.,BPM,VORP,year)
bd <- as.data.frame(lapply(bd, as.numeric))
#bd <- bd[,-colunas2]

k = 1
var1 <- numeric(0)
var2 <- numeric(0)
pvalor <- numeric(0)
estimativa <- numeric(0)
for(i in 1:49){
  for(j in 1:49){
    pvalor[k] <- cor.test(df[,-c(1:3,5)][[i]],df[,-c(1:3,5)][[j]],method="pearson")$p.value
    var1[k] <- names(df[,-c(1:3,5)])[i]
    var2[k] <- names(df[,-c(1:3,5)])[j]
    estimativa[k] <- cor.test(df[,-c(1:3,5)][[i]],df[,-c(1:3,5)][[j]],method="pearson")$estimate
    k <- k+1
  }
}

banco <- data.frame(var1,var2,pvalor,estimativa)
banco$estimativa <- as.numeric(as.character(banco$estimativa))
banco$pvalor <- as.numeric(as.character(banco$pvalor))

cor1 <- ggplot(banco,aes(x=var2,y=var1))+
  geom_tile(aes(fill=estimativa))+
  scale_fill_gradientn(colours = brewer.pal(n=11,name = "RdBu"),name="Coeficiente de \n  Correlação",limit = c(-1,1))+
  labs(x="",y="")+
  theme_bw()+
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        #axis.text.y = element_text(angle = -45, vjust = 0.5, hjust=1),
        axis.text = element_text(colour = "black", size=8),
        panel.border = element_blank(),
        axis.line = element_blank()) 

k = 1
var1 <- numeric(0)
var2 <- numeric(0)
pvalor <- numeric(0)
estimativa <- numeric(0)
for(i in 1:16){
  for(j in 1:16){
    pvalor[k] <- cor.test(bd[[i]],bd[[j]],method="pearson")$p.value
    var1[k] <- names(bd)[i]
    var2[k] <- names(bd)[j]
    estimativa[k] <- cor.test(bd[[i]],bd[[j]],method="pearson")$estimate
    k <- k+1
  }
}

banco <- data.frame(var1,var2,pvalor,estimativa)
banco$estimativa <- as.numeric(as.character(banco$estimativa))
banco$pvalor <- as.numeric(as.character(banco$pvalor))

cor2<-ggplot(banco,aes(x=var2,y=var1))+
  geom_tile(aes(fill=estimativa))+
  scale_fill_gradientn(colours = brewer.pal(n=11,name = "RdBu"),name="Coeficiente de \n  Correlação",limit = c(-1,1))+
  labs(x="",y="")+
  theme_bw()+
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text.y = element_text(angle = 45, vjust = 0.5, hjust=1),
        axis.text = element_text(colour = "black", size=8),
        panel.border = element_blank(),
        axis.line = element_blank()) 

gridExtra::grid.arrange(cor1,cor2)


ht1 <- gplots::heatmap.2(as.matrix(df[df$year %in% c(2021,2022),-c(1:3,5,31)]),scale="column",trace="none",density.info="none",
          col=brewer.pal(n=11,name = "RdBu"),key.xlab ="",key.title = "")

ht2 <- heatmap.2(as.matrix(bd[,-17]),scale="column",trace="none",density.info="none",
                 col=brewer.pal(n=11,name = "RdBu"),key.xlab ="",key.title = "")

gridExtra::grid.arrange(ht1,ht2)


corr <- round(cor(na.omit(df[,-c(1:3,5)]),method="pearson"),1)
ggcorrplot(corr,hc.order = T,lab = F,method = 'circle',type="lower",colors=c("#A11D21","white","#003366"),
           title = 'Correlações de Pearson')
ggsave("cor1.png", width = 158, height = 93, units = "mm")

corr <- round(cor(na.omit(df[,-c(1:3,5)]),method="spearman"),1)
ggcorrplot(corr,hc.order = T,lab = T,method = 'circle',type="lower",colors=c("#A11D21","white","#003366"),
           title = 'Correlações de Spearman')


df_cor <- as.data.frame(as.table(corr))
df_cor <- filter(df_cor,abs(Freq)>=.6)
