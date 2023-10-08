library(class)
library(e1071)
library(tidyverse)
library(lubridate)
library(corrplot)
library(reshape2)
library(caret)
library(olsrr)
library(fastDummies)
library(MASS)

small <- read_csv("1000 Sales Records.csv", col_types="fffffcncnnnnn")
##large <- read_csv("50000 Sales Records.csv", col_types="fffffcncnnnnn")


small$`Order Date` <- mdy(small$`Order Date`)
small$`Ship Date` <- mdy(small$`Ship Date`)

#large$`Order Date` <- mdy(large$`Order Date`)
#large$`Ship Date` <- mdy(large$`Ship Date`)


summary(small)
## initial observations
## No NA's, Order ID is probably irrelevant
##


## find plot many hist at once
## data analysis
## box plots of profit vs:  item type; region; country; order priority
## scatter of units sold vs unit price
## Look for correlation (total cost and total revenue?)
## box plot of ship & order dates

## cross validate/train model


small %>% ggplot() + geom_boxplot(mapping = aes(x=`Item Type`, y=`Total Profit`), fill='red')
small %>% ggplot() + geom_boxplot(mapping = aes(x=`Country`, y=`Total Profit`), fill='red')
small %>% ggplot() + geom_boxplot(mapping = aes(x=`Region`, y=`Total Profit`), fill='red')
small %>% ggplot() + geom_boxplot(mapping = aes(x=`Region`, y=`Units Sold`), fill='red')
small %>% ggplot() + geom_boxplot(mapping = aes(x=`Country`, y=`Units Sold`)) + facet_wrap(~Region)
small %>% ggplot() + geom_boxplot(mapping = aes(x=`Sales Channel`, y=`Total Profit`), fill='red')
small %>% ggplot() + geom_boxplot(mapping = aes(x=`Sales Channel`, y=`Unit Cost`), fill='red')
small %>% ggplot() + geom_boxplot(mapping = aes(x=`Sales Channel`, y=`Unit Price`), fill='red')
small %>% ggplot() + geom_boxplot(mapping = aes(x=`Sales Channel`, y=`Total Revenue`), fill='red')


## bar chart for factors
small %>% ggplot() + geom_bar(mapping=aes(y=Region))
small %>% ggplot() + geom_bar(mapping=aes(y=Country))
small %>% ggplot() + geom_bar(mapping=aes(y=`Sales Channel`))
small %>% ggplot() + geom_bar(mapping=aes(y=`Item Type`))
small %>% ggplot() + geom_bar(mapping=aes(y=`Order Priority`))

## between factors
ggplot(data = small) +  geom_count(mapping = aes(x = `Sales Channel`, y = `Item Type`))
ggplot(data = small) +  geom_count(mapping = aes(x = `Sales Channel`, y = `Country`))

## tiles
small %>% count(`Item Type`, `Sales Channel`) %>% ggplot(mapping = aes(x=`Sales Channel`, y=`Item Type`)) + geom_tile(mapping=aes(fill=n))
small %>% count(`Country`, `Sales Channel`) %>% ggplot(mapping = aes(x=`Sales Channel`, y=`Country`)) + geom_tile(mapping=aes(fill=n))
small %>% count(`Region`, `Sales Channel`) %>% ggplot(mapping = aes(x=`Sales Channel`, y=`Region`)) + geom_tile(mapping=aes(fill=n))
small %>% count(`Order Priority`, `Sales Channel`) %>% ggplot(mapping = aes(x=`Sales Channel`, y=`Order Priority`)) + geom_tile(mapping=aes(fill=n))


## correlations
small_numeric <- small %>% select(where(is.numeric)) %>% select(-`Order ID`)
small_correlations <- cor(small_numeric)
corrplot(small_correlations)

## chisq.test for categorical checks
chisq.test(table(small$`Sales Channel`, small$`Item Type`))
chisq.test(table(small$`Sales Channel`, small$`Country`))
chisq.test(table(small$`Sales Channel`, small$`Order Priority`))
chisq.test(table(small$`Sales Channel`, small$`Region`))


## histograms
s_df <- small_numeric %>% melt()
s_df %>% ggplot(aes(x= value)) + geom_histogram(color='#023020', fill='gray',bins=50) + facet_wrap(~variable, scales = 'free',  ncol = 4) + theme_bw()


##featurePlot(small_numeric[,1:ncol(small_numeric)-1], small_numeric[,6], plot = "scatter", type = c("p", "smooth"), span = 1)
featurePlot(small_numeric[,1:ncol(small_numeric)-1], small_numeric[,6], plot = "pairs", type = c("p", "smooth"), span = 1)


linear_model <- lm(`Total Profit` ~., data=small)


## predicting Total profit has an adjusted R^2 of 1! Perfect prediction which is normal considering the scatterplots and the definitions of total profit from the costs, units sold, etc. It follows the definition and no uncertainty exists; unless there were misreported numbers.

## not very interesting. Should try predicting which sales are off vs online from the dataset.



## confirm the linear regression model can predict total profits 100%.
set.seed(12341)
sample_set <- sample(nrow(small), round(nrow(small)*0.75), replace=FALSE)
#small_train <- small[sample_set,] %>% dplyr::select(-c(Country,`Order ID`, `Ship Date`, `Total Profit`, `Ship Date`, `Total Revenue`, `Total Cost`, ))
#small_test <- small[-sample_set,] %>% dplyr::select(-c(Country,`Order ID`, `Ship Date`, `Total Profit`, `Ship Date`, `Total Revenue`, `Total Cost`, ))
small_train <- small[sample_set,] %>% dplyr::select(-c(Country))
small_test <- small[-sample_set,] %>% dplyr::select(-c(Country))
#small_train <- small[sample_set,]
#small_test <- small[-sample_set,]

#sample_set <- sample(nrow(large), round(nrow(large)*0.75), replace=FALSE)
#large_train <- large[sample_set,] %>% dplyr::select(-Country)
#large_test <- large[-sample_set,] %>% dplyr::select(-Country)


#small_linear <- lm(`Total Profit` ~., data=small_train)
small_linear <- lm(`Total Profit` ~., data=small_train)
p <- predict(small_linear, small_test)
RMSE(p, small_test$`Total Profit`)  # 3.082376e-09
R2(p, small_test$`Total Profit`)    # 1 - predicted 100%, as expected from the model.


## logistic regression
#small_logistic <- glm(small_train, family=binomial, formula=`Sales Channel` ~ .)
small_logistic <- glm(small_train, family=binomial, formula=`Sales Channel` ~ .)
small_predict <- predict(small_logistic, small_test, type='response')
small_predict <- ifelse(small_predict >= 0.5, 1, 0)
small_predict_glm <-table(small_test$`Sales Channel`, small_predict)
sum(diag(small_predict_glm))/sum(small_predict_glm)

## LDA
small_lda <- lda(`Sales Channel` ~ Region + `Item Type` , small_train)
p1 <- predict(small_lda, small_test)$class
tab <- table(Predicted = p1, Actual = small_test$`Sales Channel`)
cat("Accuracy is:", sum(diag(tab))/sum(tab))


## naive bayes model
small_nb <- naiveBayes(`Sales Channel` ~., data=small_train, laplace=1)
small_predict_nb <- predict(small_nb, small_test, type="class")
small_table_nb <- table(small_test$`Sales Channel`, small_predict_nb)
sum(diag(small_table_nb))/sum(small_table_nb)


# KNN
sales_labels <- small %>% dplyr::select(`Sales Channel`)
small_dummies <- dummy_cols(small)
sample_dummies_index <- sample(nrow(small_dummies), round(nrow(small_dummies)*0.75), replace=FALSE)
small_train <- small_dummies[sample_dummies_index,]  %>% dplyr::select(-c(Country, Region, `Item Type`,`Sales Channel`, `Order Priority`, `Order Date`, `Order ID`, `Ship Date` ))
small_test <- small_dummies[-sample_dummies_index,]  %>% dplyr::select(-c(Country, Region, `Item Type`,`Sales Channel`, `Order Priority`, `Order Date`, `Order ID`, `Ship Date` ))
small_train_labels <- sales_labels[sample_dummies_index,]
small_test_labels <- sales_labels[-sample_dummies_index,]

#small_knn_pred <- knn(train = small_train, test = small_test, cl = small_train_labels$`Sales Channel`, k=15)
small_knn_pred <- knn(train = small_train, test = small_test, cl = small_train_labels$`Sales Channel`, k=1)
small_knn_pred_table <- table(small_test_labels$`Sales Channel`, small_knn_pred)
sum(diag(small_knn_pred_table))/sum(small_knn_pred_table)
