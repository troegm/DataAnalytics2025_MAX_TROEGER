# Lab 5
# Max Troeger - troegm@rpi.edu
# Friday March 28

### Init
library(readr)
library(ellipse)
library(caret)
library(ggfortify)
library(dplyr)
library(class)
library(caret)
library(e1071)
library(MLmetrics)
#library(usethis) 
#usethis::edit_r_environ()
# needed to set vsize to 100GB
set.seed(123)

# Part 1 - Wine classification

## Load the dataframe and assign column names
wine <- read_csv("~/Documents/School/data_analytics/repo/Lab5/wine.data",  col_names = FALSE)
colnames(wine) <- c("class","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid phenols","Proanthocyanins","Color intensity","Hue","OD280/OD315 of diluted wines","Proline")
wine$class <- as.factor(wine$class)

dataset <- wine

## split train/test
train.indexes <- sample(nrow(dataset),0.75*nrow(dataset))

train <- dataset[train.indexes,]
test <- dataset[-train.indexes,]

## Do SVM and get accuracy measure (linear)
tune.svm <- tune.svm(class ~ `Flavanoids`+`Total phenols`+`OD280/OD315 of diluted wines`, data=train, kernel = 'linear', gamma=seq(0.001,1,.01), cost=2^seq(-6,4,2))
tune.svm
fit.svm <- svm(class ~ `Flavanoids`+`Total phenols`+`OD280/OD315 of diluted wines`, data=train, kernel = 'linear', gamma=0.001, cost=0.25)
pred.svm  <-  predict(fit.svm,train)

cm = as.matrix(table(Actual = train$class, Predicted = pred.svm))
cm
#        Predicted
#Actual  1  2  3
#      1 37  5  0
#      2 11 41  5
#      3  0  1 33
n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted
recall = diag / rowsums
precision = diag / colsums
f1 = 2 * precision * recall / (precision + recall)

data.frame(precision, recall, f1)

## Do SVM and get accuracy measure (radial)
tune.svm <- tune.svm(class ~ `Flavanoids`+`Total phenols`+`OD280/OD315 of diluted wines`, data=train, kernel = 'radial', gamma=seq(0.001,1,.01), cost=2^seq(-6,4,2))
tune.svm
fit.svm <- svm(class ~ `Flavanoids`+`Total phenols`+`OD280/OD315 of diluted wines`, data=train, kernel = 'linear', gamma=0.311, cost=16)
pred.svm  <-  predict(fit.svm,train)

cm = as.matrix(table(Actual = train$class, Predicted = pred.svm))
cm
#      Predicted
#Actual  1  2  3
#     1 39  3  0
#     2 11 42  4
#     3  0  1 33
n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted
recall = diag / rowsums
precision = diag / colsums
f1 = 2 * precision * recall / (precision + recall)

data.frame(precision, recall, f1)
# Evidently this model performs better than the previous one

# Using kNN
knn <- knn(train = train, test = train, cl = train$class, k = 5)

cm = as.matrix(table(Actual = train$class, Predicted = knn))
cm
#      Predicted
#Actual  1  2  3
#     1 37  2  3
#     2  5 44  8
#     3  1 10 23
n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted
recall = diag / rowsums
precision = diag / colsums
f1 = 2 * precision * recall / (precision + recall)

data.frame(precision, recall, f1)
# This model performs worse overall than the latter 2

# Part 2
nycdata <- read_csv("/Users/maxtroeger/Documents/School/data_analytics/repo/Lab5/NY-House-Dataset.csv")
# Turns out I was using the wrong NY set, this one is much smaller!
#### Some of these prices are so outrageously large I can't tell if this is just new york being new york
#### or if I should remove these. I supply the graphs as requested
dataset <- nycdata
dataset <- dataset[-which(dataset$PROPERTYSQFT==2184.207862),]
dataset <- dataset[-which(dataset$PRICE>=195000000),]

## split train/test
train.indexes <- sample(nrow(dataset),0.75*nrow(dataset))

train <- dataset[train.indexes,]
test <- dataset[-train.indexes,]

## Do SVM and linear
tune.svm <- tune.svm(log10(PRICE)~log10(PROPERTYSQFT), data=train, kernel = 'radial', gamma=seq(.1,1,.1), cost=2^seq(-6,4,2))
tune.svm
fit.svm <- svm(log10(PRICE)~log10(PROPERTYSQFT), data=train, kernel = 'radial', gamma=0.7, cost=4)
pred.svm  <-  predict(fit.svm,train)
svr.outs <- data.frame(real=log10(train$PRICE), pred=pred.svm)

ggplot(svr.outs, aes(x = real, y = pred)) +
  geom_point() +
  stat_smooth(method = "lm",color="red")

lnfit <- lm(log10(PRICE)~log10(PROPERTYSQFT),data=train)
pred.ln  <-  predict(lnfit,train)
ln.outs <- data.frame(real=log10(train$PRICE), pred=pred.ln)

ggplot(ln.outs, aes(x = real, y = pred)) +
  geom_point() +
  stat_smooth(method = "lm")

## Residuals
svm.fitted <- predict(fit.svm,test)
svm.resid <-svm.fitted-test$PRICE
svm.resid.out <- data.frame(.fitted=svm.fitted, .residual=svm.resid)

ggplot(svm.resid.out, aes(x = .fitted, y = .residual)) +
  geom_point() +
  stat_smooth(method = "lm",color="red")

ln.fitted <- predict(lnfit,test)
ln.resid <-ln.fitted-test$PRICE
ln.resid.out <- data.frame(.fitted=ln.fitted, .residual=ln.resid)

ggplot(ln.resid.out, aes(x = .fitted, y = .residual)) +
  geom_point() +
  stat_smooth(method = "lm")