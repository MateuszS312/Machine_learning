rm(list=ls())
library(MASS)
library(e1071)
# Generate points
draw.data.gauss <- function(S1, S2, m1, m2, n1, n2) {
  
  X1 <- mvrnorm(n1, m1, S1)
  X2 <- mvrnorm(n2, m2, S2)
  
  X1 <- data.frame(X1); colnames(X1) <- c("x", "y")
  X2 <- data.frame(X2); colnames(X2) <- c("x", "y")
  
  X1$class <- 1; X2$class <- 2
  
  data <- rbind(X1, X2); data$class <- factor(data$class)
  
  return(data)
}
calculate.ACC<-function(data.class,data.predicted)
{
  CM<-table(data.class,data.predicted)
  ACC<-sum(diag(CM))/sum(CM)
}
# Plot points
plot.data <- function(data) {
  
  cols <- c("blue", "red")
  
  plot(data[,1:2], col = cols[data$class], cex = 2.5, pch = 19)
  text(data[,1:2], labels = 1:nrow(data), cex = 0.8, col = "white", font = 2)
  
}


library(caret)
par(mfrow = c(1,1))
# Gauss distribution parameters for data
S1 <- matrix(c(4, 2, 2, 4), 2, 2)
S2 <- matrix(c(4, 2, 2, 2), 2, 2)

m1 <- c(-1, -1)
m2 <- c(2, 2)

n1 <- 30
n2 <- 20

# Setting the seed for data sampling
set.seed(128)

# Generate data
data <- draw.data.gauss(S1, S2, m1, m2, n1, n2)
#plot data
plot.data(data)
#use caret to train Linear svm on data with crosvalidation 
fit<-trainControl("CV",number=5)
svm.cv<-train(class~.,data=data,method='svmLinear',tuneGrid=expand.grid(C=10**(seq(0.1,1.5,0.1))),trControl=fit)
#predict clases of test sample
class.svm<-predict(svm.cv,data)
data.svm <- svm(class ~ x + y, type = "C-classification", data = data, cost = svm.cv$bestTune, scale = F, kernel = "linear")
# Print support vectors and alpha vals
print(with(data.svm, cbind(SV, coefs)))
library(klaR)
points(data.svm$SV, col = 'Green', cex = 4, pch = 1)
#data$class<-class.svm

# Calcultate w vector
w <- t(data.svm$SV) %*% data.svm$coefs
b <- data.svm$rho

xx <- -5:5
# plot separating hyperplane
lines(xx, -w[1] / w[2] * xx + b / w[2], lty = 5, lwd = 2)

# plot margin hyperplane
lines(xx, -w[1] / w[2] * xx + (b + 1) / w[2], lty = 2, lwd = 2, col = "blue")
lines(xx, -w[1] / w[2] * xx + (b - 1) / w[2], lty = 2, lwd = 2, col = "red")
mtext(paste('ACC SVM=',toString(calculate.ACC(data$class,class.svm))), side=1, line=3, at=3,font=15)



# LDA METHOD
data.lda <- lda(class ~ x + y, data)

# Predicting classes with the LDA method
# we use the same data as for teaching
# that is, resubstituion
data.lda.pred <- predict(data.lda, data)

# Function for determining posterior probabilities in the LDA method
f.lda <- function(X, m1, m2, S, pi1, pi2) 
  {
  return(pi1 * dmvnorm(X, m1, S) / (pi1 * dmvnorm(X, m1, S) + pi2 * dmvnorm(X, m2, S)))
}



# Defining coordinate ranges
xp <- seq(-10, 10, 0.1)
yp <- seq(-10, 10, 0.1)

# Create grid on coordinates
gr <- expand.grid(x = xp, y = yp)
library(mvtnorm)
# Determine the probability density for each point on the grid
# and save the effects to the matrix
X1.pdf <- matrix(dmvnorm(gr, m1, S1), length(xp))
X2.pdf <- matrix(dmvnorm(gr, m2, S2), length(xp))


# Estimating expected values
X1<-data[data$class==1,]
X2<-data[data$class==2,]
me1 <- apply(X1[,1:2], 2, mean)
me2 <- apply(X2[,1:2], 2, mean)
# Estimateing covariance matrices
Se1 <- cov(X1[,1:2])
Se2 <- cov(X2[,1:2])

# Matrix of covariance to the LDA method
# identical to the W matrix
Se <- ((n1 - 1) * Se1 + (n2 - 1) * Se2) / (n1 + n2 - 2)


# A priori probabilities
pi1 <- n1 / (n1 + n2)
pi2 <- n2 / (n1 + n2)



# Determining the vector a and  vector b
a <- t(me1 - me2) %*% ginv(Se)
b <- log(pi1 / pi2) - 0.5 * a %*% (me1 + me2)


# Determination of the intercept and the slope
B <- -b / a[2]
A <- -a[1] / a[2]
# Plotting a line
abline(B, A, col = "deepskyblue4", lwd = 2,lty=5)
legend("topleft",legend=c("1","2","SVM","LDA"),col=c("blue","red","black","deepskyblue4"),merge=F,pch=c(19,19,NA,NA),lty=c(NA,NA,5,5))
mtext(paste('ACC LDA=',toString(calculate.ACC(data$class,data.lda.pred$class))), side=1, line=3, at=-3,font=15)

      