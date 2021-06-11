#installing packages

install.packages("ggplot2")
install.packages("stats")
install.packages("ggfortify")
install.packages("dplyr")

#loading 

library(stats)
library(dplyr)
library(ggplot2)
library(ggfortify)


#importing data

data <- read.csv("Iris.csv")
View(data)

#unlabeled data

mydata <-select(data,c(1,2,3,4,5))

#wss plot function

wssplot <- function(Data, nc=15,seed=12345)
{
  wss <- (nrow(Data)-1)*sum(apply(Data,2,var))
          for (i in 2:nc){
            set.seed(seed)
            wss[i] <- sum (kmeans(Data,centers = i)$withinss)}
  plot(1:nc,wss,type="b",xlab="Number of Clusters",
       ylab="within grous sum of squares")
}

wssplot(mydata)
#The optimum number of cluster is equal to two

#k-means
km <- kmeans(mydata,2)

#cluster plot

autoplot(km,mydata,frame=TRUE)

#Clusters centres
km$centers
