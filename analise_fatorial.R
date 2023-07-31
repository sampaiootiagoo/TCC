setwd('C:/Users/tiago/Desktop/TIAGO/Tiago UnB/8_semestre/tcc1')
library(pacman)
pacman::p_load(readxl,dplyr,tidyverse,gplots,GGally,psych,lavaan,lavaanPlot,mvnormalTest)
load('.RData')
'%!in%' <- function(x,y)!('%in%'(x,y))

# BANCOS
df1 <- filter(df,year=='2022'|year=='2021') %>% select(-W,-L)
quanti <- as.data.frame(lapply(df1[,-c(1,2,3,5,31)], as.numeric))
sapply(df1, function(x) sum(is.na(x)))
quanti <- as.data.frame((na.exclude(quanti))) # TINHA UM SCALE AQUI
quali <- df1[,c(3,5)] #ROW, YEAR E NOME FORAM RETIRADOS

#------------------------------------------------------------------------ANALISE FATORIAL-------------------------------------------------------------

alpha(quanti,check.keys=TRUE) #alfa de cronbach, bom ser maior que 0.8, foi 0.96
## obs: se a perda de um item aumenta o alfa, esse item n ta ajudando mt
splitHalf(quanti) # msm ideia q o alfa +-, foi igual a 0.99


quanti_cor <- cor(quanti,use="pairwise.complete.obs") 
scree(quanti_cor,factors=T,pc=F) #joelho no 4
eigenvals1 <- eigen(quanti_cor)
sum(eigenvals1$values>1) # 8 fatores
fa.parallel(quanti,fa='fa') # 8 fatores

# OLHAR:
# RMSR (PERTO DE 0)
# TUCKER LEWIS INDEX (ACIMA DE 0.9)
# RMSEA INDEX (ABAIXO DE 0.05)

model1 <- fa(quanti,nfactors=4,rotate = "varimax")
model1
model1$loadings
model1$e.values
model1$score.cor
fa.diagram(model1)

model2 <- fa(quanti,nfactors=8,rotate = "varimax") # MELHOR O DE 8
model2
model2$loadings
model2$e.values
model2$score.cor
fa.diagram(model2)

model3 <- fa(quanti,nfactors=6,rotate = "varimax")
model3
model3$loadings
model3$e.values
model3$score.cor
fa.diagram(model3)

# REMOVENDO COMBINACOES LINEARES E TENTANDO DE NOVO

colunas <- c(1,7,10,13,17,20,29,30:37,39,40,42,43,44) # AGE FG%, 3P%, 2P%, FT%, TRB, FTrate, ORB%, DRB%, TRB%, AST%, AST%, STL%, BLK%, TOV%, OWS%, DWS%, WS%48, OBPM, DBPM
rem <- na.exclude(quanti[,-colunas])

colunas2 <- c(1,2,3,4,5,6,8,9,11,12,15,16,18,19,25,29,30,31,32,33,34,35,36,37,39,40,41,42,43,44) # AGE, G, GS, MP, FG, FGA, X3P, X3PA, X2P, X2PA, FT, FTA, ORB, DRB, PF, X3PAr, FTr, ORB%, DRB%, TRB%, AST%, STL%, BLK%, TOV%, USG%, OWS, DWS, WS%48, OBPM, DBPM
rem2 <- na.exclude(quanti[,-colunas2])                                     # 41 É WS

rem_scale <- as.data.frame(scale(rem))
rem_scale2 <- as.data.frame(scale(rem2))

alpha(rem_scale,check.keys=TRUE) #alfa de cronbach, bom ser maior que 0.8, foi 0.96
alpha(rem_scale2[,-17],check.keys=TRUE) #alfa de cronbach, bom ser maior que 0.8, foi 0.96
## obs: se a perda de um item aumenta o alfa, esse item n ta ajudando mt
splitHalf(rem_scale) # msm ideia q o alfa +-, foi igual a 0.99
splitHalf(rem_scale2[,-17])

# OLHAR:
# RMSR (PERTO DE 0)
# TUCKER LEWIS INDEX (ACIMA DE 0.9)
# RMSEA INDEX (ABAIXO DE 0.05)

rem_cor <- cor(rem_scale2[,-17],use="pairwise.complete.obs") 
scree(rem_cor,factors=T,pc=F) #joelho no 3
eigenvals2 <- eigen(rem_cor)
sum(eigenvals2$values>1) # 4 fatores
fa.parallel(rem_scale2,fa='fa') # 5 ou 6

model <- fa(rem_scale2,nfactors=1,rotate = "varimax")
model
model$loadings
model$e.values
model$score.cor
fa.diagram(model) # GERAIS

model1_2 <- fa(rem_scale2,nfactors=2,rotate = "varimax")
model1_2
model1_2$loadings
model1_2$e.values
model1_2$score.cor
fa.diagram(model1_2) # GERAIS, VALIOSAS

model2_2 <- fa(rem_scale2,nfactors=3,rotate = "varimax") 
model2_2
model2_2$loadings
model2_2$e.values
model2_2$score.cor
fa.diagram(model2_2) # INDIVIDUAIS, APROVEITAMENTO, CHUTE -> FAVORITO

model3_2 <- fa(rem_scale2,nfactors=4,rotate = "varimax") # PRO REM2 AQUI JÁ FICA RUIM
model3_2
model3_2$loadings
model3_2$e.values
model3_2$score.cor
fa.diagram(model3_2) # GERAIS, AVANCADAS, PIVO, DE TIME

model4_2 <- fa(rem_scale2,nfactors=5,rotate = "varimax") 
model4_2
model4_2$loadings
model4_2$e.values
model4_2$score.cor
fa.diagram(model4_2) # ATAQUE, DEFESA, AVANCADAS, 3 PONTOS, DE TIME

model5_2 <- fa(rem_scale2,nfactors=6,rotate = "varimax") # 
model5_2
model5_2$loadings
model5_2$e.values
model5_2$score.cor
fa.diagram(model5_2) # 

model6_2 <- fa(rem_scale2,nfactors=7,rotate = "varimax") # 
model6_2
model6_2$loadings
model6_2$e.values
model6_2$score.cor
fa.diagram(model6_2) # 


m1 <- data.frame("N"='2 fatores',"BIC"=model1_2$BIC,"RMSEA"=model1_2$RMSEA[1],"TLI"=model1_2$TLI,"FIT"=model1_2$fit,"Prop Var"=0.62)
m2 <- data.frame("N"='3 fatores',"BIC"=model2_2$BIC,"RMSEA"=model2_2$RMSEA[1],"TLI"=model2_2$TLI,"FIT"=model2_2$fit,"Prop Var"=0.69)
m3 <- data.frame("N"='4 fatores',"BIC"=model3_2$BIC,"RMSEA"=model3_2$RMSEA[1],"TLI"=model3_2$TLI,"FIT"=model3_2$fit,"Prop Var"=0.75)
m4 <- data.frame("N"='5 fatores',"BIC"=model4_2$BIC,"RMSEA"=model4_2$RMSEA[1],"TLI"=model4_2$TLI,"FIT"=model4_2$fit,"Prop Var"=0.78)
m5 <- data.frame("N"='6 fatores',"BIC"=model5_2$BIC,"RMSEA"=model5_2$RMSEA[1],"TLI"=model5_2$TLI,"FIT"=model5_2$fit,"Prop Var"=0.82)
m6 <- data.frame("N"='7 fatores',"BIC"=model6_2$BIC,"RMSEA"=model6_2$RMSEA[1],"TLI"=model6_2$TLI,"FIT"=model6_2$fit,"Prop Var"=0.88)

rbind(m1,m2,m3,m4,m5,m6)

#---------------------------------------------------------------ANALISE FATORIAL CONFIRMATORIA------------------------------------------------------------
# PARA A ANALISE FATORIAL CONFIRMATORIA, VAMOS USAR OS DADOS DAS TEMPORADAS ANTERIORES: 2021

df2 <- df %>% filter(year==2020|year==2019) %>% select(-W,-L)
CFAquanti <- as.data.frame(lapply(df2[,-c(1,2,3,5,31)], as.numeric))
CFAquanti <- as.data.frame(scale(CFAquanti)) # TIRANDO IDADE PQ NAO SE JUNTOU A NENHUM FATOR
CFAquali <- df2[,c(3,5)] #ROW, YEAR E NOME FORAM RETIRADOS
CFArem <- CFAquanti[,-colunas2] #%>% select(-Age,-W,-L)
rm(CFAquanti,CFAquali)

# OLHAR:
# RMSR (PERTO DE 0)
# TUCKER LEWIS INDEX (ACIMA DE 0.9)
# RMSEA INDEX (ABAIXO DE 0.05)

# HIPOTESES

hip_modelo <- '
INDIVIDUAIS =~ PTS + TOV + AST + TRB + VORP + STL + BLK + USG.
APROVEITAMENTO =~ eFG. + FG. + TS. + X2P. + PER + BPM
CHUTE =~ X3P. + FT.
HABILIDADE =~ INDIVIDUAIS + APROVEITAMENTO + CHUTE'

modeloexp_CFA <- lavaan::cfa(model=hip_modelo,data=CFArem)

summary(modeloexp_CFA,fit.measures=T,
        standardized=T)
inspect(modeloexp_CFA,"std")$lambda
loadings <- inspect(modeloexp_CFA,"est")$lambda
library(semPlot)
semPaths(modeloexp_CFA)

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

summary(modeloexp_CFA2,fit.measures=T,
        standardized=T)
inspect(modeloexp_CFA2,"std")$lambda
loadings2 <- inspect(modeloexp_CFA2,"est")$lambda
library(semPlot)
semPaths(modeloexp_CFA2)

###############################MODELO DE EQUACOES ESTRUTURAIS##########################
## https://bookdown.org/bean_jerry/using_r_for_social_work_research/structural-equation-modeling.html
mardia(CFArem)

hip_modelo

fit.mod <- lavaan::sem(hip_modelo, data=CFArem, std.lv = TRUE, estimator = "MLM")

## MODEL FIT

fitMeasures(fit.mod, c('bic','rmsea','tli','cfi','pvalue'))

## MEASUREMENT MODEL

standardizedsolution(fit.mod, type = "std.all", se = TRUE, zstat = TRUE, pvalue = TRUE, ci = TRUE)%>% 
  filter(op == "=~") %>% 
  select(LV=lhs, Item=rhs, Coefficient=est.std, ci.lower, ci.upper, SE=se, Z=z, 'p-value'=pvalue)

parameterEstimates(fit.mod, standardized=TRUE, rsquare = TRUE) %>% 
  filter(op == "r2") %>% 
  select(Item=rhs, R2 = est) 

## STRUCTURAL MODEL

standardizedsolution(fit.mod, type = "std.all", se = TRUE, zstat = TRUE, pvalue = TRUE, ci = TRUE)%>% 
  filter(op == "=~") %>% 
  select(LV=lhs, Item=rhs, Coefficient=est.std, ci.lower, ci.upper, SE=se, Z=z, 'p-value'=pvalue,OP=op)

##########################################################################################################################################
##########################################################CONSTRUINDO A HABILIDADE##############################################################

##-------------------------------------------DATA FRAME DE 2023 PRA PREDIÇÃO------------------------------------------------
loadings <- as.data.frame(loadings)

df3 <- filter(df,year=='2023') %>% select(-W,-L)
quanti3 <- as.data.frame(lapply(df3[,-c(1,2,3,5,31)], as.numeric))
sapply(df3, function(x) sum(is.na(x)))
quanti3 <- as.data.frame(scale(na.exclude(quanti3))) # TINHA UM SCALE AQUI
rem3 <- na.exclude(quanti3[,-colunas2])

INDIVIDUAIS <- rowSums(t(apply(rem3[,-17], 1, function(x)x*loadings$INDIVIDUAIS)))
APROVEITAMENTO <- rowSums(t(apply(rem3[,-17], 1, function(x)x*loadings$APROVEITAMENTO)))
CHUTE <- rowSums(t(apply(rem3[,-17], 1, function(x)x*loadings$CHUTE)))
HABILIDADE <- 1*INDIVIDUAIS + 1.922*APROVEITAMENTO + 1.310*CHUTE

qnames <- as.data.frame(lapply(df3[,-c(1,2,3,5,31)], as.numeric))
qnames$player <- df3$Player
qnames$year <- df3$year
sapply(df3, function(x) sum(is.na(x)))
qnames <- as.data.frame((na.exclude(qnames))) # TINHA UM SCALE AQUI

Nome <- qnames$player
WS <- qnames$WS
Temporada <- qnames$year
teste_2023 <- data.frame(Nome,WS,HABILIDADE,Temporada)
cor.test(teste_2023$WS,teste_2023$HABILIDADE)

rm(qnames)

##-------------------------------------------- MONTANDO O MODELO COM OS DADOS ORIGINAIS --------------------------------------------
## TROCAR OU NAO O REM2 POR REM_SCALE2
INDIVIDUAIS <- rowSums(t(apply(rem_scale2[,-17], 1, function(x)x*loadings$INDIVIDUAIS)))
APROVEITAMENTO <- rowSums(t(apply(rem_scale2[,-17], 1, function(x)x*loadings$APROVEITAMENTO)))
CHUTE <- rowSums(t(apply(rem_scale2[,-17], 1, function(x)x*loadings$CHUTE)))
HABILIDADE <- 1*INDIVIDUAIS + 1.922*APROVEITAMENTO + 1.310*CHUTE

qnames <- as.data.frame(lapply(df1[,-c(1,2,3,5,31)], as.numeric))
qnames$player <- df1$Player
qnames$year <- df1$year
sapply(qnames, function(x) sum(is.na(x)))
qnames <- as.data.frame((na.exclude(qnames))) # TINHA UM SCALE AQUI

Nome <- qnames$player
WS <- qnames$WS
Temporada <- qnames$year
teste <- data.frame(Nome,WS,HABILIDADE,Temporada,INDIVIDUAIS,APROVEITAMENTO,CHUTE)
cor.test(teste$WS,teste$HABILIDADE,method = 'spearman')

rm(qnames,Nome,Temporada)


##########################################################################################################################################
###################################################CONSTRUINDO A SEGUNDA HABILIDADE##############################################################

##-------------------------------------------DATA FRAME DE 2023 PRA PREDIÇÃO------------------------------------------------
loadings2 <- as.data.frame(loadings2)

df3 <- filter(df,year=='2023') %>% select(-W,-L)
quanti3 <- as.data.frame(lapply(df3[,-c(1,2,3,5,31)], as.numeric))
sapply(df3, function(x) sum(is.na(x)))
quanti3 <- as.data.frame(scale(na.exclude(quanti3))) # TINHA UM SCALE AQUI
rem3 <- na.exclude(quanti3[,-colunas2])

MR1 <- rowSums(t(apply(rem3, 1, function(x)x*loadings2$MR1)))
MR2 <- rowSums(t(apply(rem3, 1, function(x)x*loadings2$MR2)))
MR3 <- rowSums(t(apply(rem3, 1, function(x)x*loadings2$MR3)))
MR4 <- rowSums(t(apply(rem3, 1, function(x)x*loadings2$MR4)))
MR5 <- rowSums(t(apply(rem3, 1, function(x)x*loadings2$MR5)))
MR6 <- rowSums(t(apply(rem3, 1, function(x)x*loadings2$MR6)))
MR7 <- rowSums(t(apply(rem3, 1, function(x)x*loadings2$MR7)))
HABILIDADE <- 1*MR1 + 0.795*MR2 + 0.527*MR3 + 0.533*MR4 + 0.360*MR5 + 0.777*MR6 + 1.028*MR7

qnames <- as.data.frame(lapply(df3[,-c(1,2,3,5,31)], as.numeric))
qnames$player <- df3$Player
qnames$year <- df3$year
sapply(df3, function(x) sum(is.na(x)))
qnames <- as.data.frame((na.exclude(qnames))) # TINHA UM SCALE AQUI

Nome <- qnames$player
WS <- qnames$WS
Temporada <- qnames$year
teste_2023_7 <- data.frame(Nome,WS,HABILIDADE,Temporada)
cor.test(teste_2023_7$WS,teste_2023_7$HABILIDADE)

rm(qnames)

##-------------------------------------------- MONTANDO O MODELO COM OS DADOS ORIGINAIS --------------------------------------------
## TROCAR OU NAO O REM2 POR REM_SCALE2
MR1 <- rowSums(t(apply(rem2, 1, function(x)x*loadings2$MR1)))
MR2 <- rowSums(t(apply(rem2, 1, function(x)x*loadings2$MR2)))
MR3 <- rowSums(t(apply(rem2, 1, function(x)x*loadings2$MR3)))
MR4 <- rowSums(t(apply(rem2, 1, function(x)x*loadings2$MR4)))
MR5 <- rowSums(t(apply(rem2, 1, function(x)x*loadings2$MR5)))
MR6 <- rowSums(t(apply(rem2, 1, function(x)x*loadings2$MR6)))
MR7 <- rowSums(t(apply(rem2, 1, function(x)x*loadings2$MR7)))
HABILIDADE <- 1*MR1 + 0.795*MR2 + 0.527*MR3 + 0.533*MR4 + 0.360*MR5 + 0.777*MR6 + 1.028*MR7

qnames <- as.data.frame(lapply(df1[,-c(1,2,3,5,31)], as.numeric))
qnames$player <- df1$Player
qnames$year <- df1$year
sapply(qnames, function(x) sum(is.na(x)))
qnames <- as.data.frame((na.exclude(qnames))) # TINHA UM SCALE AQUI

Nome <- qnames$player
WS <- qnames$WS
Temporada <- qnames$year
teste7 <- data.frame(Nome,WS,HABILIDADE,Temporada)
cor.test(teste7$WS,teste7$HABILIDADE,method = 'spearman')

rm(qnames,Nome,Temporada)
