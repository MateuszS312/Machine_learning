rm(list=ls())
library(MASS)
library(mvtnorm)
# Macierze kowariancji dla klas
S1 <- matrix(c(4,0,0,4),2,2)
S2 <- matrix(c(4,0,0,4),2,2)
# Wartoœci oczekiwane
mt1 <- c(-3, -1)
mt2 <- c(2, 2)
# Liczba punktów w klasach
n1 <- 40
n2 <- 30
n <- n1 + n2
# Generowanie rozk³adów
X1 <- mvrnorm(n1, mt1, S1)
X2 <- mvrnorm(n2, mt2, S2)
# Zamiana na ramki danych
X1 <- data.frame(X1); colnames(X1) <- c("x", "y")
X2 <- data.frame(X2); colnames(X2) <- c("x", "y")
# Nanoszenie punktów na wykres
plot(X1$x, X1$y, ylim = c(-8,8), xlim = c(-8,8), xlab = "X", ylab = "Y", pch = 19, col = "blue", font = 2)
abline(v = 0, h = 0, col = "gray")
points(X2$x, X2$y, pch = 19, col = "orange")
# Przypisanie klas
X1$class <- 1; X2$class <- 2

# "Sklejenie" danych do jednej ramki
# oraz przekszta³cenie typu zmiennej przechowuj¹cej klasy
data <- rbind(X1, X2); data$class <- factor(data$class)
dat<-data
naive.bayess<-function(learn_data,predict_data)
{
  n2<-sum(learn_data$class==2)
  n1<-sum(learn_data$class==1)
  n<-n1+n2
  mu_2x<-mean(learn_data$x[learn_data$class==2])
  mu_2y<-mean(learn_data$y[learn_data$class==2])
  mu_1x<-mean(learn_data$x[learn_data$class==1])
  mu_1y<-mean(learn_data$y[learn_data$class==1])
  sd_2x<-sd(learn_data$x[learn_data$class==2])
  sd_2y<-sd(learn_data$y[learn_data$class==2])
  sd_1x<-sd(learn_data$x[learn_data$class==1])
  sd_1y<-sd(learn_data$y[learn_data$class==1])
  tmp<-log(n2/n1)+
    +log(dnorm(predict_data$x,mean=mu_2x,sd=sd_2x)/dnorm(predict_data$x,mean=mu_1x,sd=sd_1x))+
    +log(dnorm(predict_data$y,mean=mu_2y,sd=sd_2y)/dnorm(predict_data$y,mean=mu_1y,sd=sd_1y))
  predict_data$class<-ifelse(tmp>0,2,1)
  return(predict_data)
}
data_2<-naive.bayess(data,data)
plot(data_2$x[data_2$class=="1"], data_2$y[data_2$class=="1"], ylim = c(-8,8), xlim = c(-8,8), xlab = "X", ylab = "Y", pch = 19, col = "blue", font = 2)
abline(v = 0, h = 0, col = "gray")
points(data_2$x[data_2$class=="2"], data_2$y[data_2$class=="2"], pch = 19, col = "orange")

# Definiowanie zakresów wspó³przêdnych
xp <- seq(-10, 10, 0.1)
yp <- seq(-10, 10, 0.1)

# Rozpiêcie siatki na wspó³przêdnych
gr <- expand.grid(x = xp, y = yp)

library(e1071)

# Klasyfikator naiwnego Bayesa
data.nb <- naiveBayes(class ~ x + y, data)

# Przewidywanie klas za pomoc¹ klasyfikatora naiwnego Bayesa
# domyœlnie zwraca klasy
data.nb.pred <- predict(data.nb, data)

# opcja method = "raw" daje prawdopodobieñstwa a posteriori
data.nb.pred <- predict(data.nb, data, type = "raw")
library(klaR)
png("LSED_zad_1.png")
# Rysowanie prostej rozdzielaj¹cej, punktów etc
with(data, drawparti(class, x, y, method = "naiveBayes", xlab = "X", ylab = "Y", font = 2))

#na data sie uczy i sprawdzam klasyfikacje punktow siatki
#contour plotuje linie "ekwipotencjalna" 
contour(xp, yp, matrix(naive.bayess(data,gr)$class, length(xp)), add = T, lwd = 2, lty = 2, col = "blue")
dev.off()
