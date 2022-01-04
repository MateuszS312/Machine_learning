rm(list=ls())
library(rpart)
library(rpart.plot)
# a) loading data
data<-read.table("wine.data",sep=",")
# b) add names to columns
colnames(data)<-make.names(c("Class","Alcohol",
                  "Malic acid",
                  "Ash",
                  "Alcalinity of ash",
                  "Magnesium",
                  "Total phenols",
                  "Flavanoids",
                  "Nonflavanoid phenols",
                  "Proanthocyanins",
                  "Color intensity",
                  "Hue",
                  "OD280/OD315 of diluted wines",
                  "Proline"))
#convert Class to factor 
data$Class<-factor(data$Class)

#create full tree
# minsplit the minimum number of observations that must exist in a node in order for a split to be attempted
# minbucket the minimum number of observations in any terminal node
tree<-rpart(Class~.,data,minsplit=2,minbucket=1,cp=0)
#plot full tree
rpart.plot(tree,type=1,extra=1)
#predict clases
tree.class<-predict(tree,newdata=data,type="class")
#function which returns Accuracy first argument - training data, second argument predicted classes
calculate.ACC<-function(data,data.predicted)
{
  CM<-table(data$Class,data.predicted)
  ACC<-sum(diag(CM))/sum(CM)
}
ACC.full<-calculate.ACC(data,tree.class)
print(ACC)
library(caret)
#useing caret to perform crosvalidation
fit<-trainControl("CV",number=5)
tree.cv<-train(Class~.,data=data,method='rpart',trControl=fit)
tree.class.cv<-predict(tree.cv,newdata=data)
ACC<-calculate.ACC(data,tree.class.cv)
#an algorithm to compute the cp parameter for the optimal tree
min.xerror<-min(tree$cptable[,'xerror'])[1]
xerr.sd<-tree$cptable[tree$cptable[,'xerror']==min.xerror,'xstd'][1]
cp<-tree$cptable[tree$cptable[,'xerror']<min.xerror+xerr.sd,'CP'][1]

#cut the tree
tree.cut<-prune(tree,cp=cp)
#plot optimal tree
rpart.plot(tree.cut,type=1,extra=1)

tree.class.cut<-predict(tree.cut,newdata=data,type='class')
ACC.cut<-calculate.ACC(data,tree.class.cut)
cat("ACC for full tree:",ACC.full," |ACC for optimal treee:",ACC.cut)

#create optimal tree for different data and return Acc and difference between number of nodes in full tree and optimal tree
create.different.trees<-function(data)
{
  tree<-rpart(Class~.,data,minsplit=2,minbucket=1,cp=0)
  min.xerror<-min(tree$cptable[,'xerror'])[1]
  xerr.sd<-tree$cptable[tree$cptable[,'xerror']==min.xerror,'xstd'][1]
  cp<-tree$cptable[tree$cptable[,'xerror']<min.xerror+xerr.sd,'CP'][1]
  #cut the tree
  tree.cut<-prune(tree,cp=cp)
  rpart.plot(tree.cut,type=1,extra=1)
  tree.class.cut<-predict(tree.cut,newdata=data,type='class')
  ACC<-calculate.ACC(data,tree.class.cut)

  r<-c(ACC,nrow(tree$frame[tree$frame['var']=='<leaf>',])-nrow(tree.cut$frame[tree.cut$frame['var']=='<leaf>',]))
  return(r)
}
#calculate ACC and difference in size for data depending on a different number of variables
ACC.vec<-sapply(2:length(data),function(x) create.different.trees(data[,1:x]))
#make plots to show the difference
plot(2:length(data), ACC.vec[1,], ylab="Accuracy", xlab="Number of used variables")
plot(2:length(data), ACC.vec[2,], ylab="Difference in size", xlab="Number of used variables")

