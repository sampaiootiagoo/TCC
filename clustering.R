setwd('C:/Users/tiago/Desktop/TIAGO/Tiago UnB/8_semestre/tcc1')
library(pacman)
pacman::p_load(readxl,dplyr,tidyverse,gplots,GGally,psych,lavaan,factoextra,NbClust,gclus) #hclust
load('.RData')
'%!in%' <- function(x,y)!('%in%'(x,y))

#-----------------------------------------------------------------CLUSTERING-------------------------------------------------------------------

## MÉTODOS HIERÁRQUICOS: DENDROGRAMAS PARA VER O NUMERO IDEAL DE CLUSTERS
dist_euc <- dist(rem_scale2, method = 'euclidean')
dist_man <- dist(rem_scale2, method = 'manhattan')

par(mfrow=c(2,2))

#### AVERAGE LINKAGE - DISTANCIA EUCLIDIANA E DE MANHATTAN
hclust1 <- hclust(dist_euc, method = 'average')
plot(hclust1,main='Average Linkage - Distância Euclidiana',labels = F)

hclust2 <- hclust(dist_man, method = 'average')
plot(hclust2,main='Average Linkage - Distância de Manhattan',labels = F)

#### COMPLETE LINKAGE - DISTANCIA EUCLIDIANA E DE MANHATTAN
hclust3 <- hclust(dist_euc, method = 'complete')
plot(hclust3,main='Complete Linkage - Distância Euclidiana',labels = F)

hclust4 <- hclust(dist_man, method = 'complete')
plot(hclust4,main='Complete Linkage - Distância de Manhattan',labels = F)

#### GRÁFICO DO COTOVELO
fviz_nbclust(rem_scale2, kmeans, method = "wss") + 
  geom_vline(xintercept = 3, linetype = 2) + labs(subtitle = "Método do Cotovelo")

#### PACOTE NBCLUST
par(mfrow=c(2,2))

nb1 <- NbClust(rem_scale2, distance = "euclidean", min.nc = 2,
              max.nc = 9, method = "average") # 9 CLUSTERS -> 7, 4, 0, 0, 3, 0, 1, 8
#factoextra::fviz_nbclust(nb)

nb2 <- NbClust(rem_scale2, distance = "manhattan", min.nc = 2,
                  max.nc = 9, method = "average") # 4 CLUSTERS -> 4, 6, 8, 1, 0, 2, 0, 9
#fviz_nbclust(nb)

nb3 <- NbClust(rem_scale2, distance = "euclidean", min.nc = 2,
              max.nc = 9, method = "complete") # 2 CLUSTERS -> 7, 6, 4, 0, 1, 0, 0, 5
#fviz_nbclust(nb)

nb4 <- NbClust(rem_scale2, distance = "manhattan", min.nc = 2,
              max.nc = 9, method = "complete") # 2 CLUSTERS -> 8, 2, 2, 0, 7, 0, 0, 2
#fviz_nbclust(nb)

nb5 <- NbClust(rem_scale2, distance = "euclidean", min.nc = 2,
              max.nc = 9, method = "kmeans") # 2 CLUSTERS -> 9, 6, 1, 3, 0, 3, 0, 1
#fviz_nbclust(nb)

nb6 <- NbClust(rem_scale2, distance = "manhattan", min.nc = 2,
              max.nc = 9, method = "kmeans") # 2 CLUSTERS -> 10, 6, 1, 3, 0, 3, 0, 1
#fviz_nbclust(nb)

# codigo do grafico do numero indicado de clusters

v <- as.factor(c(2:9))
v1<- c(7, 4, 0, 0, 3, 0, 1, 8)
v2<- c(4, 6, 8, 1, 0, 2, 0, 9)
v3<- c(7, 6, 4, 0, 1, 0, 0, 5)
v4<- c(8, 2, 2, 0, 7, 0, 0, 2)
v5<- c(9, 6, 1, 3, 0, 3, 0, 1)
v6<- c(10, 6, 1, 3, 0, 3, 0, 1)

comp <- data.frame(v,v1,v2,v3,v4,v5,v6)

g1 <- ggplot(comp, aes(x=v, y=v1, label=v1)) +
  geom_bar(stat="identity", fill="lightblue") +
  geom_text(vjust=-0.5, size=4,hjust=-.05)+
  labs(x="Número de Clusters", y="Frequência") +
  theme_bw() + scale_y_continuous(limits = c(0,11))+
  ggtitle('Average Linkage Distância Euclidiana')+
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=8.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))# + coord_flip()

g2 <- ggplot(comp, aes(x=v, y=v2, label=v2)) +
  geom_bar(stat="identity", fill="lightblue") +
  geom_text(vjust=-0.5, size=4,hjust=-.05)+
  labs(x="Número de Clusters", y="Frequência") +
  theme_bw() + scale_y_continuous(limits = c(0,11))+
  ggtitle('Average Linkage Distância de Manhattan')+
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=8.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))# + coord_flip()

g3 <- ggplot(comp, aes(x=v, y=v3, label=v3)) +
  geom_bar(stat="identity", fill="lightblue") +
  geom_text(vjust=-0.5, size=4,hjust=-.05)+
  labs(x="Número de Clusters", y="Frequência") +
  theme_bw() + scale_y_continuous(limits = c(0,11))+
  ggtitle('Complete Linkage Distância Euclidiana')+
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=8.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))# + coord_flip()

g4 <- ggplot(comp, aes(x=v, y=v4, label=v4)) +
  geom_bar(stat="identity", fill="lightblue") +
  geom_text(vjust=-0.5, size=4,hjust=-.05)+
  labs(x="Número de Clusters", y="Frequência") +
  theme_bw() + scale_y_continuous(limits = c(0,11))+
  ggtitle('Complete Linkage Distância de Manhattan')+
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=8.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))# + coord_flip()

g5 <- ggplot(comp, aes(x=v, y=v5, label=v5)) +
  geom_bar(stat="identity", fill="lightblue") +
  geom_text(vjust=-0.5, size=4,hjust=-.05)+
  labs(x="Número de Clusters", y="Frequência") +
  theme_bw() + scale_y_continuous(limits = c(0,11))+
  ggtitle('K-means Distância Euclidiana')+
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=8.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))# + coord_flip()

g6 <- ggplot(comp, aes(x=v, y=v6, label=v6)) +
  geom_bar(stat="identity", fill="lightblue") +
  geom_text(vjust=-0.5, size=4,hjust=-.05)+
  labs(x="Número de Clusters", y="Frequência") +
  theme_bw() + scale_y_continuous(limits = c(0,11))+
  ggtitle('K-means Distância de Manhattan')+
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=8.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))# + coord_flip()

gridExtra::grid.arrange(g1,g2,g3,g4,g5,g6)

## MÉTODOS NÃO HIERÁRQUICOS - KMEANS
model <- kmeans(scale(rem3), centers = 2)
teste_2023$Cluster <- model$cluster
rem3$Cluster <- model$cluster
table(teste_2023$Cluster)

model2 <- kmeans(rem_scale2, centers = 2)
teste$Cluster <- model2$cluster
rem_scale2$Cluster <- model2$cluster
table(teste$Cluster)

clust1 <- ggplot(teste, aes(x=INDIVIDUAIS, y=APROVEITAMENTO)) + geom_point(aes(colour=as.factor(Cluster))) +
  scale_colour_manual(name="Cluster", values = c("#003366","#A11D21"), 
                      labels = c("1", "2"))+
  labs(x="Características Individuais", y="Características de Aproveitamento")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(legend.position="top")

clust2 <- ggplot(teste, aes(x=CHUTE, y=WS)) + geom_point(aes(colour=as.factor(Cluster))) +
  scale_colour_manual(name="Cluster", values = c("#003366","#A11D21"), 
                      labels = c("1", "2"))+
  labs(x="Características de Chute", y="Win Shares (WS)")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(legend.position="top")

gridExtra::grid.arrange(clust1,clust2)

tapply(teste$HABILIDADE, as.factor(teste$Cluster), summary)
aggregate(HABILIDADE ~ Cluster, data=teste,FUN=summary)

#### DENDROGRAMA FINAL
suppressPackageStartupMessages(library(dendextend))
dist_mat <- dist(rem_scale2[,-17], method = 'euclidean') # arg 2
hclust_avg <- hclust(dist_mat, method = 'complete') # arg 1
avg_dend_obj <- as.dendrogram(hclust_avg,label=F)
avg_col_dend <- color_branches(avg_dend_obj, k = 2)
plot(avg_col_dend,main='Dendrograma com Dados Clusterizados')

km <- hkmeans(rem_scale2[,-17],2)
cluster_assignments <- km$cluster
print(cluster_assignments)
teste <- mutate(teste,Cluster=cluster_assignments)
table(teste$Cluster)
library(factoextra)
fviz_dend(km,main="Dendrograma com Clusterização K-Means",ylab="",show_labels = F,palette = c("#8fbc8f","#018571"),rect = T,rect_border =c("#8fbc8f","#018571"),rect_fill=T)


## GRÁFICOS COM O CLUSTER

#habf <- data.frame(MR1,MR2,MR3,MR4,HAB5,cluster_assignments)
#habf$cluster_assignments <- factor(substr(habf$cluster_assignments, 1, 1))
ggplot(teste, aes(x=WS, y=HABILIDADE)) + geom_point(aes(colour=as.factor(Cluster))) +
  scale_colour_manual(name="Cluster", values = c("#003366","#A11D21"), 
                      labels = c("1", "2"))+
  labs(x="Win Shares (WS)", y="Habilidade")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(legend.position="top")
