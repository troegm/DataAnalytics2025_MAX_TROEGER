# Lab 4
# Max Troeger - troegm@rpi.edu
# Friday March 21

### Init
library(readr)
library(ellipse)
library(caret)
library(ggfortify)
library(dplyr)
library(class)
library(caret)
library(MLmetrics)
set.seed(123)

### Load the dataframe and assign column names
wine <- read_csv("~/Documents/School/data_analytics/repo/Lab4/wine.data",  col_names = FALSE)
colnames(wine) <- c("class","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid phenols","Proanthocyanins","Color intensity","Hue","OD280/OD315 of diluted wines","Proline")

### assign training and testing data
n = 178
s_wine <- sample(n,n*.8)

wine.train <- wine[s_wine,]
wine.test <- wine[-s_wine,]

### Split the data into `class` and relevant classification variables
X.train <- wine.train %>% select(-`class`)
X.test <- wine.test %>% select(-`class`)

## Part 1: Compute the PCs and plot the dataset using the 1st and 2nd PC
### Do PCA
principal_components.train <- prcomp(X.train,center=TRUE,scale=TRUE)
principal_components.test <- prcomp(X.test,center=TRUE,scale=TRUE)

### Make the plot
autoplot(principal_components.train, data = wine.train, colour = 'class',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)

## Part 2: Identify the variables that contribute the most to the 1st PCA
print(principal_components.train)

## Part 3: Drop the variables least contributing to the 1st PCA and rerun PCA
### Flavanoids, Total phenols, OD280 contribute the most to PC1
X_2.train <- X.train %>% select(`Flavanoids`,`Total phenols`, `OD280/OD315 of diluted wines`)
X_2.test <- X.test %>% select(`Flavanoids`,`Total phenols`, `OD280/OD315 of diluted wines`)
principal_components_2.train <- prcomp(X_2.train,center=TRUE,scale=TRUE)
principal_components_2.test <- prcomp(X_2.test,center=TRUE,scale=TRUE)

### Make the plot
autoplot(principal_components_2.train, data = wine.train, colour = 'class',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)

## Part 4: Train a classifier model (e.g. kNN) to predict wine type using the original dataset.
###
Y.train <- wine.train$class %>% as.factor
Y.test <- wine.test$class %>% as.factor
k = round(sqrt(n)) - 1

### train model & predict in one step ('knn' function from 'class' library)
knn_orig.predicted <- knn(train = X.train, test = X.test, cl = Y.train, k = k)

### create contingency table/ confusion matrix & calculate classification accuracy
contingency_orig.table <- table(knn_orig.predicted, Y.test, dnn=list('predicted','actual'))
orig_accuracy <- sum(diag(contingency_orig.table))/length(Y.test)

## Part 5: Train a classifier model to predict wine type using the data projected into the first 3 PCs (scores)
### Get scores for first 3 PCs
scores.train <- principal_components.train$x[,1:3]
scores.test <- principal_components.test$x[,1:3]

### Get an idea of classification
featurePlot(x=scores.train, y=Y.train, plot="ellipse")

### Do the classification
knn_scores.predicted <- knn(train = scores.train, test = scores.test, cl = Y.train, k = k)

### create contingency table/ confusion matrix & calculate classification accuracy
contingency_scores.table <- table(knn_scores.predicted, Y.test, dnn=list('predicted','actual'))
score_accuracy <- sum(diag(contingency_scores.table))/length(Y.test)

## Part 6: Compare the 2 classification models using contingency tables and precision/recall/f1 metrics
### Precision/recall/f1 are defined *by class*, so for 3 classes we need to calculate it several times.
### I choose f1 because it is the harmonic mean of precision and recall

### First by using accuracy:
if (score_accuracy > orig_accuracy) {
  cat("Using kNN on the scores for the first 3 PCs produces more a more accurate classification than running kNN on the original dataset:\n",format(score_accuracy*100,digits=4),"% >",format(orig_accuracy*100,digits=4),"%\n")
} else {
  cat("Using kNN on the original dataset produces a more accurate classification than running kNN on the scores for the first 3 PCs:\n",format(score_accuracy*100,digits=4),"% <",format(orig_accuracy*100,digits=4),"%\n")
}

### Now calculate F1
f1_scores <- F1_Score(knn_scores.predicted,Y.test)
f1_orig <- F1_Score(knn_orig.predicted,Y.test)

### Now by using F1:
if (f1_scores > f1_orig) {
  cat("Using kNN on the scores for the first 3 PCs has better predictive performance (F1) than running kNN on the original dataset:\n",format(f1_scores,digits=4),">",format(f1_orig,digits=4),'\n')
} else {
  cat("Using kNN on the original dataset has better predictive performance (F1) than running kNN on the scores for the first 3 PCs:\n",format(f1_scores,digits=4),"<",format(f1_orig,digits=4),'\n')
}
