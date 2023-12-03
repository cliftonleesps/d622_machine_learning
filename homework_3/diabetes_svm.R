library(tidyverse)
library(rpart)
library(rpart.plot)
library(randomForest)
library(datasets)
library(caret)
library(e1071)
library(kernlab)
library(pROC)
library(performanceEstimation)
library(ggplot2)
library(GGally)
library(kernlab)
library(tidymodels)



data <- read_csv('diabetes.csv', col_types="nnnnnnnnf")
data <- smote(Outcome ~ ., data, perc.over = 1.5, perc.under = 2.0)

svmfit <- svm(Outcome ~., data=data, kernel="linear", cost=10)

set.seed(1234)

sample_set <- sample(nrow(data), round(nrow(data)*0.80), replace = FALSE)

data_train <- data[sample_set,]
data_test <- data[-sample_set,]


svm_linear <- svm(Outcome ~., data=data_train, kernel="linear", cost=10)
svm_poly <- svm(Outcome ~., data=data_train, kernel="polynomial", coef=1, gama=1, cost=10)
svm_radial <- svm(Outcome ~., data=data_train, kernel="radial", gama=1, cost=10)
svm_sigmoid <- svm(Outcome ~., data=data_train, kernel="sigmoid", gama=1, cost=10)



r <- predict(svm_linear, data_test)
c <- confusionMatrix(r, data_test$Outcome)
c$overall[[1]]


r <- predict(svm_poly, data_test)
c <- confusionMatrix(r, data_test$Outcome)
c$overall[[1]]


r <- predict(svm_radial, data_test)
c <- confusionMatrix(r, data_test$Outcome)
c$overall[[1]]

r <- predict(svm_sigmoid, data_test)
c <- confusionMatrix(r, data_test$Outcome)
c$overall[[1]]

