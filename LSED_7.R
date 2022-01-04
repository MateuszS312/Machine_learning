# Pobranie danych
animals <- cluster::animals

#adding colnames
colnames(animals) <- c("warm-blooded", 
                       "can fly",
                       "vertebrate",
                       "endangered",
                       "live in groups",
                       "have hair")
#All variables are encoded as 1 = 'no', 2 = 'yes'.

# Tworzenie wykresu
hmap<-heatmap(as.matrix(animals), col = c("red","green"), scale="column", keep.dendro=T)
dendr_row<-hmap$Rowv
plot(dendr_row)
# ROW ANALYSIS
# in first step:
#   man, lion and chi (?) to form one group are combined because they cant fly
#   elephant and whale are also combined because they dont have hair and cant fly
#   cow and rabit are combined because they are not endangered and cant fly
#   sal and liz are combined because they cant fly ther are not endangered they dont have hair and they are not warm bloded
#   ant and lob are combined because they cant fly they are not endangered they dont have hair they are not warm bloded they are not vertabrates
#   spi and cpl are combined because they cannot fly they are not endangered they do not live in groups they are not warm bloded they are not vertabretes and they do have HairEyeColor
# In second step:
#   liz sal and her are combined but lizards dont live in groups and they are lizard like animals
#   cat is combined with cow and rabit but cats dont live in groups
# In third step:
#   frog is combined with liz sal and Hershey
#   elephant whale man lion and chi are combined
# In fourth step:
#   fly is combined with lob and ansari.test
#     duc is combined with eagle because they are birds
# In fifth step
#   cpl spi bee fly lob and ant are combined as they are Insect and they are not vertabretes 
# In sixth step:
#   cat is combined with cow rabit elephant whale man lion chi because they are mamals
# In seventh step:
#   birds lizard like animals and mamalas are combined to form vertabretes
# In last step:
#   all animals are combined to form animals group
dendr_col<-hmap$Colv
plot(dendr_col)
#COL ANALYSIS
#in first step:
# vertabretes and warm bloded are combined because vertabretes are usually warm bloded
#in second step
# we can see that warm bloded vertabreetes usually live in group
#in third step:
# we can see that flying animals are usually endangered
#in fourth step:
# we can see that warm bloded vertabreeetes have hair
#in last step are possible animal features are connected to one group






rm(list=ls())

data("iris")
iris$Species<-as.factor(iris$Species) 
names(iris)[names(iris)=="Species"]<-"Class"

library(Hmisc)
iris.new<-iris[,1:4]
iris.class<-iris$Class
#create every possible combination of variables
indx_list<-c(combn(names(iris.new),m=2,simplify = F),combn(names(iris.new),m=3,simplify = F),combn(names(iris.new),m=4,simplify = F))
m<-ncol(iris.new)
indx_list<-unlist((lapply(2:m, function (kk) combn(names(iris.new),m=kk,simplify=F))),recursive = F)
#clusters assigned by km
km_output<-lapply(indx_list,function(indx) kmeans(iris.new[,indx],3)$cluster)


#color definition for clusters
blueop <- "#0000ff55"; orangeop <- "#ffa50055"; redop <- "#00ff0055"
#function for plotting the data and comparing with km output
plot_data<-function(indxs,class_km)
{
  cols <- c("blue", "orange","green")
  plot(iris[,indxs], col = cols[iris$Class], cex = 2,pch=19)
  text(iris[,indxs], labels = 1:nrow(iris), cex = 0.6,col='white')
  title("Oryginalne dane + efekty k-œrednich ")
  points(iris[,indxs], pch = 19, cex = 3, col = c(blueop, orangeop,redop)[class_km])
}
#create plots for all possible combinations
par(mfrow = c(2,3))
plot_comb<-combn(names(iris.new),m=2,simplify=F)
sapply(km_output, function(x) sapply(plot_comb, function(y) plot_data(y,x)))
