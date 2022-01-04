rm(list=ls())
set.seed(10)
library(MASS)

draw.data.gauss <- function(S1, S2, m1, m2, n1, n2) {
  
  X1 <- mvrnorm(n1, m1, S1)
  X2 <- mvrnorm(n2, m2, S2)
  
  X1 <- data.frame(X1); colnames(X1) <- c("x", "y")
  X2 <- data.frame(X2); colnames(X2) <- c("x", "y")
  
  X1$class <- 1; X2$class <- 2
  
  data <- rbind(X1, X2); data$class <- factor(data$class)
  
  return(data)
}

# Parametry danych z rozkladu Gaussa
S1 <- matrix(c(4, 2, 2, 4), 2, 2)
S2 <- matrix(c(4, 2, 2, 2), 2, 2)

m1 <- c(-1, -1)
m2 <- c(2, 2)

n1 <- 30
n2 <- 20

# Generowanie obserwacji
data <- draw.data.gauss(S1, S2, m1, m2, n1, n2)
data

library(class)

# Funkcja knn
CM.large<-function(original.class,predicted.class)
{
  
  CM<-table(original.class,predicted.class)
  FP<-rowSums(CM)-diag(CM)
  FN<-colSums(CM)-diag(CM)
  TP<-diag(CM)
  TN<-sum(CM)-(FP+FN+TP)
  
  #skutecznosc klasyfikatora
  ACC<-sum(diag(CM))/sum(CM)
  # Sensitivity, hit rate, recall, or true positive rate
  TPR <- TP/(TP+FN)
  # Specificity or true negative rate
  TNR <- TN/(TN+FP) 
  # Precision or positive predictive value
  PPV <- TP/(TP+FP)
  # Negative predictive value
  NPV <- TN/(TN+FN)
  # Fall out or false positive rate
  FPR <- FP/(FP+TN)
  # False negative rate
  FNR <- FN/(TP+FN)
  # False discovery rate
  FDR <- FP/(TP+FP)
  # Overall accuracy
  ACC <- (TP+TN)/(TP+FP+FN+TN)
  #Wartosci true positive i true negarive
  #zakladamy ze klasa "2 jest pozytywna
  return(c(ACC = round(ACC[2],4), TP = TP[2], TN = TN[2], TPR = round(TPR[2], 4), FPR = round(FPR[2], 4), row.names = NULL))
  
}
vector.CM.Large<-function(teach.data,test.data)
{
  data_list<-lapply(1:21,function(x) knn(teach.data[,1:2], test.data[,1:2], teach.data$class, x))
  # Porównanie
  CM_vector<-t(sapply(data_list, function(x) CM.large(test.data$class,x)))
  return(CM_vector)
}

CM_vector<-vector.CM.Large(data,data)
plot(1:21,CM_vector[,"ACC.2"],ylab="Accuracy",xlab="Number of neighbors k")
plot(1:21,CM_vector[,"TP.2"],ylab="True positive",xlab="Number of neighbors k")
plot(1:21,CM_vector[,"TN.2"],ylab="True negative",xlab="Number of neighbors k")


# test_data<-draw.data.gauss(S1,S2,m1,m2,10,5)
# CM_vector_test<-vector.CM.Large(data,test_data)
# 
# plot(1:21,CM_vector_test[,"ACC.2"],ylab="Accuracy",xlab="Number of neighbors k")
# plot(1:21,CM_vector_test[,"TP.2"],ylab="True positive",xlab="Number of neighbors k")
# plot(1:21,CM_vector_test[,"TN.2"],ylab="True negative",xlab="Number of neighbors k")

mean_CM.large<-function(teach.data,test.data)
{
  test_data<-lapply(1:10,function(x) draw.data.gauss(S1,S2,m1,m2,10,5))
  w<-lapply(test_data,function(x) (vector.CM.Large(teach.data,x)))
  w<-Reduce('+',w)
  return(w/10)
}
CM_mean_vector<-mean_CM.large(data,test_data)
plot(1:21,CM_mean_vector[,"ACC.2"],ylab="Mean Accuracy",xlab="Number of neighbors k")
plot(1:21,CM_mean_vector[,"TP.2"],ylab="Mean True positive",xlab="Number of neighbors k")
plot(1:21,CM_mean_vector[,"TN.2"],ylab="Mean True negative",xlab="Number of neighbors k")

