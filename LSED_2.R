rm(list=ls())
library(MASS)
library(mvtnorm)
library(e1071)
# a) loading data
data<-read.table("wine.data",sep=",")
# b) add names to columns
colnames(data)<-c("Class","Alcohol",
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
 	"Proline")
#convert Alcohol to factor (from now on we interpret Alcohol as class )
data$Class<-factor(data$Class)


#function which return Confiusion Matrix for classifier
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
  return(c(ACC = round(ACC,4), TP = TP, TN = TN, TPR = round(TPR, 4), FPR = round(FPR, 4), row.names = NULL))
  
}

info<-function(data,pv=data)
{
  class.lda <- lda(Class ~., data)
  class.qda <- qda(Class ~., data)
  class.nb <- naiveBayes(Class ~., data)
  # Powtórne podstawienie
  data.lda <- predict(class.lda, data)
  data.qda <- predict(class.qda, data)
  data.nb <- predict(class.nb, data)
  # G³ówne wartoœci z macierzy pomy³ek dla powtórnego podstawienia
  res <- CM.large(data$Class, data.lda$class)
  res <- rbind(res, CM.large(data$Class, data.qda$class))
  res <- rbind(res, CM.large(data$Class, data.nb))
  rownames(res) <- c("LDA", "QDA", "NB")
  return(res)
}
#Powtorne podstawienie
info(data)
#Sprawdzanie skutecznosci klasyfikatorow dla kilku pierwszych skladowych
l<-c(2,5,10)
lapply(l,function(l) info(data[,1:l]))

#podpunkt 4 
# Trenowanie klasyfikatorów na PU
N <- nrow(data)
data.rnd <- data[sample(1:N),1:3]
PU <- data.rnd[1:(N/2),]
PV <- data.rnd[(N/2+1):round(3*N/4),]
PT <- data.rnd[round(3*N/4+1):N,]
#testowanie na probie walidacyjnej
info(PU,PV)
#najlepsze rezultaty ma lda
#proba testowa
info(PU,PT)

#kroswalidacja
CV <- function(data, K) {
  data<-data.cv  
  N <- nrow(data)
  # Dane przetasowane
  data.rnd <- data[sample(1:N),]
  
  # Tworzenie K pseudoprób
  sets <- sapply(1:K, function(i) (((i-1) * (N/K) + 1):(i * (N/K))))
  
  # Przypadek K = 1
  if(is.vector(sets)) sets <- t(as.matrix(sets))
  
  # Dla ka¿dej pseudopróby wyznaczamy liczbê pomy³ek
  res <- t(sapply(1:K, function(k) info(data.rnd[-c(sets[,k]),], data.rnd[c(sets[,k]),])["LDA",]))
  return(res)
}

# G³ówna funkcja odpowiedzialna za CV
# przyjmuje PU (jedna z pseudoprób) oraz PT
CV.main <- function(learn, test) {
  
  learn.classifier <- lda(Class ~ ., data=learn)
  test.pred <- predict(learn.classifier, newdata = test)
  # Macierz pomy³ek
  CM <- table(test$Class, test.pred$class)
  #print(CM)
  # Liczba b³êdów
  return(sum(CM) - sum(diag(CM)))
}
data.cv<-data[,1:3]
k<-CV(data.cv,5)
print(k)
# Powtórne podstawienie
info(data[,1:3],data[,1:3])["LDA",]
#testowanie na probie walidacyjnej
info(PU,PV)["LDA",]

