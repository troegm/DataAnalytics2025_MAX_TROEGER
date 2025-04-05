################################
# Lab 6 - Max Troeger          #
# Friday April 4               #
################################

library(readr)
library(ggplot2)
library(e1071)
library(caret)
library(cv)

dataset <- read_csv("/Users/maxtroeger/Downloads/NY-House-Dataset.csv")
dataset <- dataset[-which(dataset$PROPERTYSQFT==2184.207862),]
dataset <- subset(dataset,PRICE!=0)
dataset <- subset(dataset,PROPERTYSQFT!=0)

ggplot(dataset, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm",color="red")

# get_error function
get_error <- function(mod) {
  k = 10000 ; mae <- c(); mse <- c(); rmse <- c()
  for (i in 1:k) {
    train.indexes <- sample(nrow(dataset),0.75*nrow(dataset))
    train <- dataset[train.indexes,]
    test <- dataset[-train.indexes,]
    
    pred <- predict(mod, test)  
    err <- pred-log10(test$PRICE)
    
    abs.err <- abs(err)
    mean.abs.err <- mean(abs.err)
    
    sq.err <- err^2
    mean.sq.err <- mean(sq.err)
    root.mean.sq.err <- sqrt(mean.sq.err)  
    
    mae <- c(mae,mean.abs.err)
    mse <- c(mse,mean.sq.err)
    rmse <- c(rmse,root.mean.sq.err)
  }
  error_list <- list("Mean Average Error" = mean(mae), "Mean Square Error" = mean(mse), "Root Mean Square Error" = mean(rmse))
  return(error_list)
}

# Linear Fit
fit.lm <- lm(log10(PRICE)~log10(PROPERTYSQFT),dataset)
get_error(fit.lm)

# Support Vector Machine (Linear)
fit.svm <- svm(log10(PRICE) ~ log10(PROPERTYSQFT), dataset, kernel="linear")
get_error(fit.svm)

# Tuned Support Vector Machine (Radial)
#tune.svm(log10(PRICE) ~ log10(PROPERTYSQFT), data=dataset, kernel="radial", gamma = seq(0.1,1,0.1), cost = 10^seq(-3,2,1), tune.control=tune.control(cross = 5))
fit.better.svm <- svm(log10(PRICE) ~ log10(PROPERTYSQFT), dataset, kernel="radial", gamma=0.4, cost=1)
get_error(fit.better.svm)
