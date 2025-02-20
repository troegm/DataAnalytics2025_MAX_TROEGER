# Lab 3
# Max Troeger (troegm@rpi.edu)
# February 21, 2025
library(sandwich)
library(lmtest)
library(readr)
library(ggplot2)
library(caret)
library(class)

# read dataset
abalone <- read.csv("Documents/School/data_analytics/repo/Lab3/abalone_dataset.csv")

## add new column age.group with 3 values based on the number of rings 
abalone$age.group <- cut(abalone$rings, br=c(0,8,11,35), labels = c("young", 'adult', 'old'))


## alternative way of setting age.group
abalone$age.group[abalone$rings<=8] <- "young"
abalone$age.group[abalone$rings>8 & abalone$rings<=11] <- "adult"
abalone$age.group[abalone$rings>11 & abalone$rings<=35] <- "old"

##############
# Exercise 1 #
##############

## First specification (optimal k=11 from trial-by-error)
n <- nrow(abalone)
s_abalone <- sample(n,n*.8)
abalone.train <- abalone[s_abalone,]
abalone.test <- abalone[-s_abalone,]
knn1 <- knn(train=abalone.train[,2:4],test=abalone.test[,2:4],cl=abalone.train$age.group,k=11)
### Confusion Table
confuse1 <- table(knn1, abalone.test$age.group, dnn=list('predicted','actual'))
confuse1
accuracy1 <- sum(diag(confuse1))/length(abalone.test$age.group)
accuracy1*100

## Second specification (optimal k=15 from trial-by-error)
knn2 <- knn(train=abalone.train[,5:7],test=abalone.test[,5:7],cl=abalone.train$age.group,k=15)
### Confusion Table
confuse2 <- table(knn2, abalone.test$age.group, dnn=list('predicted','actual'))
confuse2
accuracy2 <- sum(diag(confuse2))/length(abalone.test$age.group)
accuracy2*100

##############
# Exercise 2 #
##############
k.list <- c(1,3,5,7,9,11,13,15,17,19,21)
wcss.list <- c()

for (k in k.list) {
  km <- kmeans(abalone[,5:7], centers = k)
  wcss <- km$tot.withinss
  wcss.list <- c(wcss.list,wcss)
}

### Optimal k is approximately 9
plot(k.list,wcss.list,type = "b")

km <- kmeans(abalone[,5:7], centers = 9)
clusters <- as.factor(km$cluster)

### For Viscera Weight
ggplot(abalone, aes(x = viscera_wieght, y = age.group, colour = as.factor(clusters))) +
  geom_point()
### For Shucked Weight
ggplot(abalone, aes(x = shucked_wieght, y = age.group, colour = as.factor(clusters))) +
  geom_point()
