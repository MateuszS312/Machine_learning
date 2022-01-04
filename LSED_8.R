rm(list=ls())
par(mfrow = c(1,1))

plot.data <- function(data) 
{
  
  cols <- c("blue", "red","green")
  
  plot(data[,1:2], col = cols[data$Class], cex = 2.5, pch = 19)
  text(data[,1:2], labels = 1:nrow(data), cex = 0.8, col = "white", font = 2)
  
}

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
data.w.class<-data[,2:ncol(data)]
# Perform PCA analysis
data.pc <- princomp(~., cor=T, data=data.w.class)

# Plot cumulative variance connected to variables
cum.var<-cumsum(data.pc$sdev**2)
barplot(cum.var/max(cum.var),ylim=c(0,1),names.arg=1:length(cum.var),las=2,ylab="Cumulative variance",xlab="Number of cumulated variables")
title("Cumalative variance", cex.main=1.4, font=2)
#create comperative plots of data in old and new coordinates
par(mfrow = c(1,2))
plot.data(data.frame(data.w.class[1:2],Class=data$Class))
title("Plot of first two variables \nin old coordinates", cex.main=1.4, font=2)
new.data1<-data.frame(data.pc$scores[,1:2],Class=data$Class)
plot.data(new.data1)
title("Plot of first two variables \nin new coordinates", cex.main=1.4, font=2)
par(mfrow = c(1,2))
plot.data(data.frame(data.w.class[2:3],Class=data$Class))
title("Plot of second and third variable \nin old coordinates", cex.main=1.4, font=2)
new.data1<-data.frame(data.pc$scores[,2:3],Class=data$Class)
plot.data(new.data1)
title("Plot of second and third variable \nin new coordinates", cex.main=1.4, font=2)
