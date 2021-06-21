setwd("C:/Users/emanu/OneDrive/Desktop/Dataset Thesis")

library(readxl)
Centrocampisti <- read_excel("C:/Users/emanu/OneDrive/Desktop/Dataset Thesis/Centrocampisti_ok.xlsx")
View(Centrocampisti)
library(tidyverse)



Centrocampisti <- Centrocampisti %>%
  filter(`Matches played`>=15) %>% 
  filter(`Minutes played`>=1000)

y <- Centrocampisti[,-c(1,2,3,4,5,6,7,8,14,15,16)]

y[,c(10,11,12)] <- (-1)*y[,c(10,11,12)]
y
View(y)

colnames(Centrocampisti)


rowclust <- hclust(dist(scale(y)),method="average")
unicor <- cor(y)
cordist <- 0.5-unicor/2
colclust <- hclust(as.dist(cordist),method="average")

library("gplots")
dev.off()
heatmap.2(unicor,Rowv=as.dendrogram(colclust),
          Colv=as.dendrogram(colclust),scale="none",trace="none",
          col=colorRampPalette(c("orange", "white","green")),
          margins =c(10,10), 
          breaks=50,cexCol=0.8,cexRow=0.8,
          notecol="black",
          notecex=1.3,main = "Correlations")

fit.1 = factanal(y, factors = 6, rotation = "varimax")
fit.1



scores_thompson = factanal(y, factors = 6, rotation = "varimax", scores = "regression")$scores


l <-fit.1$loadings

loadings<-data.frame(matrix(as.numeric(l), attributes(l)$dim, dimnames=attributes(l)$dimnames))

colnames(loadings)[1] <- "Finishing"
colnames(loadings)[2] <- "Defensive Skill"
colnames(loadings)[3] <- "Fair Play"
colnames(loadings)[4] <- "Dribbling"
colnames(loadings)[5] <- "Assist"
colnames(loadings)[6] <- "Accuracy"

colnam_fac=as.factor(colnames(y))
loadings$Test=colnam_fac
summary(loadings)



Ord <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)
Ord <- sort(Ord, decreasing = T)

loadings$Test <- reorder(loadings$Test, Ord) 
library("reshape2")
loadings.m <- melt(loadings, id="Test", 
                   measure=c("Finishing", "Defensive Skill", 
                             "Fair Play", "Dribbling",
                             "Assist","Accuracy"), 
                   variable.name="Factor", value.name="Loading")


ggplot(loadings.m, aes(Test, abs(Loading), fill=Loading)) + 
  facet_wrap(~ Factor, nrow=1) + #place the factors in separate facets
  geom_bar(stat="identity") + #make the bars
  coord_flip() + #flip the axes so the test names can be horizontal  
  #define the fill color gradient: blue=positive, red=negative
  scale_fill_gradient2(name = "Loading", 
                       high = "blue", mid = "white", low = "red", 
                       midpoint=0, guide=F) +
  ylab("Loading Strength") + #improve y-axis label
  xlab("Variables")+
  theme_bw(base_size=10)+
  ggtitle("Factors Configuration")+
  theme(axis.text.x =element_text(size=7))+
  theme(title = element_text(size=14))


#make the stacked bar graph
ggplot(loadings.m, aes(Test, abs(Loading), fill=Factor)) + 
  geom_bar(stat="identity") + coord_flip() + 
  ylab("Loading Strength") + theme_bw(base_size=10) + 
  #remove labels and tweak margins for combining with the correlation matrix plot
  theme(axis.title.y = element_blank(), 
        plot.margin = unit(c(3,1,25,0.8), "mm")) +
  ggtitle("Variables Effect")+
  theme(title = element_text(size=14))



DF <- data.frame(Centrocampisti$Player, Centrocampisti$Team, Centrocampisti$Age, scores_thompson)



SSloading <- c(2.913, 2.179, 2.026, 1.764, 1.588, 1.342)
DF$total <- (DF$Factor1*SSloading[1] + DF$Factor2*SSloading[2] + DF$Factor3*SSloading[3] + 
               DF$Factor4*SSloading[4] + DF$Factor5*SSloading[5] + DF$Factor6*SSloading[6])/sum(SSloading)

DF <- DF[order(DF$total, decreasing = T),]
DF$total <- round(DF$total, 3)

colnames(DF)[1] <- "Player"
colnames(DF)[2] <- "Team"
colnames(DF)[3] <- "Age"
colnames(DF)[10] <- "Ranking"

head(DF[,c(1,2,3,10)])

DF2 <- data.frame(Centrocampisti$Player, Centrocampisti$Goals, Centrocampisti$xG,
                  Centrocampisti$Assists, Centrocampisti$xA,
                  scores_thompson)


DF2$total <- (DF2$Factor1*SSloading[1] + DF2$Factor2*SSloading[2] + DF2$Factor3*SSloading[3] + 
                DF2$Factor4*SSloading[4] + DF2$Factor5*SSloading[5] + DF2$Factor6*SSloading[6])/sum(SSloading)

DF2 <- DF2[order(DF2$total, decreasing = T),]
DF2$total <- round(DF2$total, 3)

colnames(DF2)[1] <- "Player"
colnames(DF2)[2] <- "Goals"
colnames(DF2)[3] <- "xG"
colnames(DF2)[4] <- "Assists"
colnames(DF2)[5] <- "xA"
colnames(DF2)[12] <- "Ranking"

head(DF2[,c(1:5,12)])
  
Kurtic <- Centrocampisti[99,]
Kurtic1 <- Kurtic[,c(13,17,18,29,30,31)]

Berardi <- Centrocampisti[20,]
Berardi1 <- Berardi[,c(1,21,22,23)]


hist(DF$Ranking, col = "grey", freq = F, main = "Players' Ranking", xlab = "Ranking")
abline(v = mean(DF$Ranking), col = "red")
curve(dnorm(x, mean = mean(DF$Ranking), sd=sqrt(var(DF$Ranking))),col="blue", add=T)




#the values MV2019 and MV2020 are the market values respectively in June 6, 2019 and in August 25, 2020.
Youngs <- DF %>% filter(Age <= 24) %>% head()
Youngs$MV2019 <- c(6,15,1.5,50,4,15)
Youngs$MV2020 <- c(25,22,12,50,5,20)
Youngs[,-(4:9)]



Suso <- DF %>% filter(Player == "Suso")
Suso[,-(4:9)]

Allan <- DF %>% filter(Player == "Allan")
Allan[,-(4:9)]

#sensitivity
w <- y[,-c(10,11,12)]
fit.2 = factanal(w, factors = 5, rotation = "varimax")
fit.2

#second model factors configuration
l2 <-fit.2$loadings
l2
loadings2<-data.frame(matrix(as.numeric(l2), attributes(l2)$dim, dimnames=attributes(l2)$dimnames))

colnames(loadings2)[1] <- "Finishing"
colnames(loadings2)[2] <- "Defensive Skill"
colnames(loadings2)[3] <- "Dribbling"
colnames(loadings2)[4] <- "Assist"
colnames(loadings2)[5] <- "Accuracy"

colnam_fac2=as.factor(colnames(w))
loadings2$Test=colnam_fac2
summary(loadings2)



Ord2 <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17)
Ord2 <- sort(Ord2, decreasing = T)

loadings2$Test <- reorder(loadings2$Test, Ord2) 
library("reshape2")
loadings.m2 <- melt(loadings2, id="Test", 
                   measure=c("Finishing", "Defensive Skill", 
                              "Dribbling","Assist",
                              "Accuracy"), 
                   variable.name="Factor", value.name="Loading")


ggplot(loadings.m2, aes(Test, abs(Loading), fill=Loading)) + 
  facet_wrap(~ Factor, nrow=1) + #place the factors in separate facets
  geom_bar(stat="identity") + #make the bars
  coord_flip() + #flip the axes so the test names can be horizontal  
  #define the fill color gradient: blue=positive, red=negative
  scale_fill_gradient2(name = "Loading", 
                       high = "blue", mid = "white", low = "red", 
                       midpoint=0, guide=F) +
  ylab("Loading Strength") + #improve y-axis label
  xlab("Variables")+
  theme_bw(base_size=10)+
  ggtitle("Factors Configuration")+
  theme(axis.text.x =element_text(size=7))+
  theme(title = element_text(size=14))


#make the stacked bar graph
ggplot(loadings.m2, aes(Test, abs(Loading), fill=Factor)) + 
  geom_bar(stat="identity") + coord_flip() + 
  ylab("Loading Strength") + theme_bw(base_size=10) + 
  #remove labels and tweak margins for combining with the correlation matrix plot
  theme(axis.title.y = element_blank(), 
        plot.margin = unit(c(3,1,25,0.8), "mm")) +
  ggtitle("Variables Effect")+
  theme(title = element_text(size=14))















scores_2 <- factanal(w, factors = 5, rotation = "varimax", scores = "regression")$scores

DF3 <- data.frame(Centrocampisti$Player, Centrocampisti$Team, Centrocampisti$Age, scores_2)



SSloading2 <- c(2.855, 2.320, 1.718, 1.708, 1.349)
DF3$total <- (DF3$Factor1*SSloading2[1] + DF3$Factor2*SSloading2[2] + DF3$Factor3*SSloading2[3] + 
               DF3$Factor4*SSloading2[4] + DF3$Factor5*SSloading2[5])/sum(SSloading2)

DF3 <- DF3[order(DF3$total, decreasing = T),]
DF3$total <- round(DF3$total, 3)

colnames(DF3)[1] <- "Player"
colnames(DF3)[2] <- "Team"
colnames(DF3)[3] <- "Age"
colnames(DF3)[9] <- "Ranking"

head(DF3[,c(1,2,3,9)])


DF <- DF[order(DF$Player),]
DF3 <- DF3[order(DF3$Player),]

Ranking_diff <- (DF3$Ranking-DF$Ranking)
mean(Ranking_diff)
sqrt(mean(Ranking_diff^2))


hist(Ranking_diff, main = "Histogram of Ranking Difference", xlab = "Ranking Difference")

DF2 <- DF

DF2$Difference <- Ranking_diff

DF2 <- DF2[order(DF2$Difference),]
head(DF2)
ggplot(data=DF2, aes(x=Player, y=Difference))+
  geom_bar(stat="identity", fill="blue", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw()


DF4 <- DF2
DF5 <- DF4[1:35,]
DF6 <- DF4[36:70,]
DF7 <- DF4[71:105,]
DF8 <- DF4[106:141,]

DF5 <- DF5 %>%
  mutate(Player = fct_reorder(Player, desc(DF5$Difference)))
DF6 <- DF6 %>%
  mutate(Player = fct_reorder(Player, desc(DF6$Difference)))
DF7 <- DF7 %>%
  mutate(Player = fct_reorder(Player, DF7$Difference))
DF8 <- DF8 %>%
  mutate(Player = fct_reorder(Player, DF8$Difference))

p1 <- ggplot(data=DF5, aes(x=Difference, y=Player))+
  geom_bar(stat="identity", fill="blue", alpha=.6, width=.4)+
  ggtitle("New - Old Ranking")


p2 <- ggplot(data=DF6, aes(x=Difference, y=Player))+
  geom_bar(stat="identity", fill="blue", alpha=.6, width=.4)+
  ggtitle("New - Old Ranking")

p3 <- ggplot(data=DF7, aes(x=Difference, y=Player))+
  geom_bar(stat="identity", fill="blue", alpha=.6, width=.4)+
  ggtitle("New - Old Ranking")
p4 <- ggplot(data=DF8, aes(x=Difference, y=Player))+
  geom_bar(stat="identity", fill="blue", alpha=.6, width=.4)+
  ggtitle("New - Old Ranking")

library(gridExtra)
grid.arrange(p4, p3,ncol=2)
grid.arrange(p1, p2,ncol=2)

