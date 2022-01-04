rm(list=ls())

heatmap.own<-function(data)
{
  cormat <- round(cor(data),2)
  library(reshape2)
  melted_cormat <- melt(cormat)
  head(melted_cormat)
  library(ggplot2)
  ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile()
  # Get upper triangle of the correlation matrix
  get_upper_tri <- function(cormat){
    cormat[lower.tri(cormat)]<- NA
    return(cormat)
  }
  upper_tri <- get_upper_tri(cormat)
  upper_tri
  # Melt the correlation matrix
  library(reshape2)
  melted_cormat <- melt(upper_tri, na.rm = TRUE)
  # Heatmap
  library(ggplot2)
  ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Pearson\nCorrelation") +
    theme_minimal()+ 
    theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                     size = 10, hjust = 1))+
    coord_fixed()
}

calculate.ACC<-function(data.class,data.predicted)
{
  CM<-table(data.class,data.predicted)
  ACC<-sum(diag(CM))/sum(CM)
}

choose.cp <- function(tree) {
  
  n <- which.min(tree$cptable[, 'xerror'])
  n.min <- min(which(tree$cptable[, 'xerror'] < tree$cptable[n, 'xerror'] + tree$cptable[n, 'xstd']))
  
  return(tree$cptable[n.min, 1])
}


library(MASS)
library(rpart)
library(rpart.plot)
library(Hmisc)
games<-read.csv(file="high_diamond_ranked_10min.csv",header = T)
#convert Class to factor 
games$blueWins<-as.factor(games$blueWins)
games$gameId<-as.factor(games$gameId)
games$blueFirstBlood<-as.factor(games$blueFirstBlood)
games$redFirstBlood<-as.factor(games$redFirstBlood)

heatmap.own((games[,!names(games) %in% names(Filter(is.factor, games))]))

#get rid of all qualitative variables
games.pca<-(games[,!names(games) %in% names(Filter(is.factor, games))])

#Perform PCA analysis
#You tend to use the covariance matrix when the variable scales are similar and the correlation matrix when variables are on different scales.
Sc <- cor(games.pca)
#calculate eigen vectors and values
eSc <- eigen(Sc)
#define gamma matrix
gamma<-eSc$vectors
#calculate mean value in each column
m<-colMeans(games.pca)
#center data
games.pca.prime<-t(apply(games.pca,1,function(x) x-m))
#calculate transformed variables
games.pca.Y<-as.matrix(games.pca.prime)%*%gamma
#get rid of princiapal vectors connected to low eigen vals
games.pca.Y<-games.pca.Y[,abs(eSc$values)>10**(-8)]
# Plot cumulative variance connected to principal variables
cum.var<-cumsum(eSc$values**2)
barplot(cum.var/max(cum.var),ylim=c(0,1),names.arg=1:length(cum.var),las=2,ylab="Cumulative variance",xlab="Number of cumulated variables")
title("Cumalative variance", cex.main=1.4, font=2)
#see which original variables have biggest impact in new variables
vector_comps<-apply(abs(gamma)>10^(-1),2,function(x) names(games.pca)[x])
barplot(gamma[,1],names.arg=names(games.pca),las=2,cex.names =  1,main="Wplyw zmiennych na skladowa glowna")
#create data frame with class included
data<-data.frame(class=games$blueWins,games.pca.Y)


library(caret)
#setup data for training and testing
trainIndex <- createDataPartition(data$class, p = .75, 
                                  list = FALSE, 
                                  times = 1)
newdata<-data[-trainIndex,]
data<-data[trainIndex,]
#create full tree
# minsplit the minimum number of observations that must exist in a node in order for a split to be attempted
# minbucket the minimum number of observations in any terminal node
tree0<-rpart(class~.,data,minsplit=2,minbucket=1,cp=0)
#plot full tree
#rpart.plot(tree0,type=1,extra=1)
#predict clases
tree0.class<-predict(tree0,newdata=newdata,type="class")
#function which returns Accuracy first argument - training data, second argument predicted classes
tree <- prune(tree0, cp = choose.cp(tree0))
rpart.plot(tree,type=1,extra=1)
tree.class<-predict(tree,newdata=newdata,type="class")
library("ROCR")
pred <- prediction(predict(tree, type = "prob")[, 2], data$class)
plot(performance(pred, "tpr", "fpr"))
abline(0, 1, lty = 2)
auc_ROCR <- performance(pred, measure = "auc")@y.values[[1]]
ACC<-calculate.ACC(newdata$class,tree.class)
ACC.full<-calculate.ACC(newdata$class,tree0.class)
print(ACC.full)
print(ACC)
#calculate errors of clasfication for full tree on trainging data, and optimal tree for trainging and test data
ACC0 <- calculate.ACC(newdata$class, predict(tree0, newdata, type = "class"))
ACC1 <- calculate.ACC(data$class, predict(tree, data, type = "class"))
ACC2 <- calculate.ACC(newdata$class, predict(tree, newdata, type = "class"))






#PROJECT PART 2
data<-data.frame(
                  class=games$blueWins,
                  blueFirstBlood=games$blueFirstBlood,
                  blueKillsDiff=games$blueKills-games$redKills,
                  blueAssistsDiff=games$blueAssists-games$redAssists,
                  blueDragonsDiff=games$blueDragons-games$redDragons,
                  blueHeraldsDiff=games$blueHeralds-games$redHeralds,
                  blueTowersDestroyedDiff=games$blueTowersDestroyed-games$redTowersDestroyed,
                  blueAvgLevelDiff=games$blueAvgLevel-games$redAvgLevel,
                  blueTotalMinionsKilledDiff=games$blueTotalMinionsKilled-games$redTotalMinionsKilled,
                  blueTotalJungleMinionsKilledDiff=games$blueTotalJungleMinionsKilled-games$redTotalJungleMinionsKilled,
                  blueGoldDiff=games$blueGoldDiff,
                  blueTotalExperienceDiff=games$blueTotalExperience-games$redTotalExperience,
                  blueWardsPlacedDiff=games$blueWardsPlaced-games$redWardsPlaced,
                  blueWardsDestroyedDiff=games$blueWardsDestroyed-games$redWardsDestroyed
                  )

library(reshape2)
heatmap.own((data[,!names(data) %in% names(Filter(is.factor, data))]))

trainIndex <- createDataPartition(data$class, p = .75, 
                                  list = FALSE, 
                                  times = 1)
newdata<-data[-trainIndex,]
data<-data[trainIndex,]
#create full tree
# minsplit the minimum number of observations that must exist in a node in order for a split to be attempted
# minbucket the minimum number of observations in any terminal node
tree0<-rpart(class~.,data,minsplit=2,minbucket=1,cp=0)
#plot full tree
#rpart.plot(tree0,type=1,extra=1)
#predict clases
tree0.class<-predict(tree0,newdata=newdata,type="class")
#function which returns Accuracy first argument - training data, second argument predicted classes
tree <- prune(tree0, cp = choose.cp(tree0))
rpart.plot(tree,type=1,extra=1)
tree.class<-predict(tree,newdata=newdata,type="class")
library("ROCR")
pred <- prediction(predict(tree, type = "prob")[, 2], data$class)
plot(performance(pred, "tpr", "fpr"))
abline(0, 1, lty = 2)
auc_ROCR <- performance(pred, measure = "auc")@y.values[[1]]
ACC<-calculate.ACC(newdata$class,tree.class)
ACC.full<-calculate.ACC(newdata$class,tree0.class)
print(ACC.full)
print(ACC)
#calculate errors of clasfication for full tree on trainging data, and optimal tree for trainging and test data
ACC0 <- calculate.ACC(newdata$class, predict(tree0, newdata, type = "class"))
ACC1 <- calculate.ACC(data$class, predict(tree, data, type = "class"))
ACC2 <- calculate.ACC(newdata$class, predict(tree, newdata, type = "class"))
