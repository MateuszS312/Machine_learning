rm(list=ls())
library(MASS)
library(rpart)
library(rpart.plot)
library(Hmisc)


# Load dataset
data('iris')
data <- iris
names(data)[names(data)=="Species"]<-"class"
data$class<-as.factor(as.numeric(data$class))
trainIndex <- createDataPartition(data$class, p = .8, 
                                  list = FALSE, 
                                  times = 1)
newdata<-data[-trainIndex,]
data<-data[trainIndex,]


#Plot data
par(mfrow = c(2,2))
cols <- c("blue", "orange","green")

plot(data[,1:2], col = cols[data$class], cex = 2)
text(data[,1:2], labels = 1:nrow(data), cex = 0.6)
plot(data[,3:4], col = cols[data$class], cex = 2)
text(data[,3:4], labels = 1:nrow(data), cex = 0.6)
plot(data[,c(2,4)], col = cols[data$class], cex = 2)
text(data[,c(2,4)], labels = 1:nrow(data), cex = 0.6)
plot(data[,c(1,3)], col = cols[data$class], cex = 2)
text(data[,c(1,3)], labels = 1:nrow(data), cex = 0.6)
#Conclsion from data: LDA should give nice results because data is nicely separated
par(mfrow = c(1,2))
# Choose optimal CP value for tree according to the 1-SE rule
 choose.cp <- function(tree) {
  
  n <- which.min(tree$cptable[, 'xerror'])
  n.min <- min(which(tree$cptable[, 'xerror'] < tree$cptable[n, 'xerror'] + tree$cptable[n, 'xstd']))
  
  return(tree$cptable[n.min, 1])
}
# choose.cp<-function(tree)
# {
#   min.xerror<-min(tree$cptable[,'xerror'])[1]
#   xerr.sd<-tree$cptable[tree$cptable[,'xerror']==min.xerror,'xstd'][1]
#   cp<-tree$cptable[tree$cptable[,'xerror']<min.xerror+xerr.sd,'CP'][1]
#   return(cp)
# }
# Calculate error rate of the clasifier
err.rate <- function(org.class, pred.class) {
  
  CM <- table(org.class, pred.class)
  
  return(1 - sum(diag(CM)) / sum(CM))
}

# Create full tree
tree0 <- rpart(class ~ ., data, minsplit = 0, minbucket = 0, cp = 0)
# Create optimal tree
tree <- prune(tree0, cp = choose.cp(tree0))

#calculate errors of clasfication for full tree on trainging data, and optimal tree for trainging and test data
err0 <- err.rate(newdata$class, predict(tree0, newdata, type = "class"))
err1 <- err.rate(data$class, predict(tree, data, type = "class"))
err2 <- err.rate(newdata$class, predict(tree, newdata, type = "class"))


# function for performing bagging operation with lda clasifier
bagging.own.lda <- function(data, N) {
  
  # randomized data sampled from original data
  dane <- replicate(N, sample(1:nrow(data), rep = T))
  
  # create lda classifiers
  ldas <- lapply(1:N, function(i) lda(class ~ ., data = data[dane[,i],], maxdepth = 1))
  
  tmp <- list(dane = dane)
  
  tmp$N <- N
  tmp$data <- data
  tmp$ldas <- ldas
  # Perform clasification based on bagging
  tmp1 <- bagging.own.pred.lda(tmp, data)
  
  tmp$lda.class <- tmp1$lda.class
  tmp$votes <- tmp1$votes
  tmp$class <- tmp1$class
  
  tmp$err <- tmp1$err
  
  return(tmp)
}

# Perform clasification based on bagging with LDA classifiers
bagging.own.pred.lda <- function(bag, data) 
{
  tmp <- list()
  lda.class <- do.call(cbind,sapply(1:bag$N, function(i) (predict(bag$ldas[[i]], newdata = data, type = "class")['class'])))
  votes <- t(sapply(1:nrow(lda.class), function(i) table(factor(lda.class[i,], levels = levels(data$class)))))
  
  class <- factor(levels(data$class)[apply(votes, 1, which.max)], levels = levels(data$class))
  
  tmp$lda.class <- lda.class
  tmp$votes <- votes
  tmp$class <- class
  
  tmp$err <- err.rate(data$class, tmp$class)
  
  return(tmp)
}
# Number of used lda classifiers
vals <- c(1, 5, 10, 20, 50)

# Run bagging algorithm for different number of lda classifiers for training sample (10 times) and get error rate
tab <- sapply(vals, function(v) replicate(10, bagging.own.lda(data, v)$err))

# Run bagging algorithm for different number of lda classifiers for testsample (10 times) and get error rate
tab.new <- sapply(vals, function(v) replicate(10, bagging.own.pred.lda(bagging.own.lda(data, v), newdata)$err))

# Calculate mean error rate and sd for different number of used classifiers for training sample
tab.m <- apply(tab, 2, mean)
tab.s <- apply(tab, 2, sd)

# Calculate mean error rate and sd for different number of used classifiers for test sample
tab.new.m <- apply(tab.new, 2, mean)
tab.new.s <- apply(tab.new, 2, sd)

#Create error plots for bagging with LDA
errbar(vals, tab.m, tab.m + tab.s, tab.m - tab.s, ylim = c(0, 0.55),xlab="Liczba klasyfikatorow",ylab="Blad klasyfiakcji")
title("LDA")
errbar(vals, tab.new.m, tab.new.m + tab.new.s, tab.new.m - tab.new.s,pch=17, add = T, col = "red", errbar.col = "red")
legend("topleft",legend=c("PU","PT"),col=c("black","red"),merge=F,pch=c(15,17))
abline(h = err0, lty = 2)
abline(h = err1)
abline(h = err2, col = "red")

# function for performing bagging operation with trees
bagging.own <- function(data, N) {
  
  # randomized data sampled from original data
  dane <- replicate(N, sample(1:nrow(data), rep = T))
  
  # create trees
  #maxdepth sets the maximum deth of any node of the final tree
  trees <- lapply(1:N, function(i) rpart(class ~ ., data = data[dane[,i],], maxdepth = 1))
  # trees <- lapply(1:N, function(i) rpart(class ~ ., data = data[dane[,i],]))
  # #create optimal trees
  # trees<-lapply(trees, function(t)  prune(t, cp = choose.cp(t)))
  tmp <- list(dane = dane)
  
  tmp$N <- N
  tmp$data <- data
  tmp$trees <- trees
  
  tmp1 <- bagging.own.pred(tmp, data)
  
  tmp$trees.class <- tmp1$trees.class
  tmp$votes <- tmp1$votes
  tmp$class <- tmp1$class
  
  tmp$err <- tmp1$err
  
  return(tmp)
}

# Perform clasification based on bagging with trees
bagging.own.pred <- function(bag, data) {
  
  tmp <- list()
  
  trees.class <- sapply(1:bag$N, function(i) predict(bag$trees[[i]], newdata = data, type = "class"))
  votes <- t(sapply(1:nrow(trees.class), function(i) table(factor(trees.class[i,], levels = levels(data$class)))))
  
  class <- factor(levels(data$class)[apply(votes, 1, which.max)], levels = levels(data$class))
  
  tmp$trees.class <- trees.class
  tmp$votes <- votes
  tmp$class <- class
  
  tmp$err <- err.rate(data$class, tmp$class)
  
  return(tmp)
}


# Number of used trees
vals <- c(1, 5, 10, 20, 50)

# Run bagging algorithm for different number of lda classifiers for training sample (10 times) and get error rate
tab <- sapply(vals, function(v) replicate(10, bagging.own(data, v)$err))
# Run bagging algorithm for different number of lda classifiers for testsample (10 times) and get error rate
tab.new <- sapply(vals, function(v) replicate(10, bagging.own.pred(bagging.own(data, v), newdata)$err))

# Calculate mean error rate and sd for different number of used classifiers for training sample
tab.m <- apply(tab, 2, mean)
tab.s <- apply(tab, 2, sd)

# Calculate mean error rate and sd for different number of used classifiers for test sample
tab.new.m <- apply(tab.new, 2, mean)
tab.new.s <- apply(tab.new, 2, sd)

#Create error plots for bagging with trees
errbar(vals, tab.m, tab.m + tab.s, tab.m - tab.s,pch=15, ylim = c(0, 0.5),xlab="Liczba klasyfikatorow",ylab="Blad klasyfiakcji")
title("Drzewa")

errbar(vals, tab.new.m, tab.new.m + tab.new.s, tab.new.m - tab.new.s,pch=17, add = T, col = "red", errbar.col = "red")
legend("topleft",legend=c("PU","PT"),col=c("black","red"),merge=F,pch=c(15,17))
abline(h = err0, lty = 2)
abline(h = err1)
abline(h = err2, col = "red")
