
#DECISION TREES - ENSAMBLE METHODS
# - BAGGING - Bootstrap Sampling - sub-samples with replacemnet - 
# - Random Forest - random split decision,godd for regression better for classification, no pruning,
#                   good for large sets, shows importance ov variables
# - Boosting - every split improves the chances using base Learning ML algorithm, implementations:
#              - XGBoost- parallel processing, fast, handles missing data, build in cross validation
#              - GBM - slower, easier, build in cross validation
#
https://www.analyticsvidhya.com/blog/2016/04/complete-tutorial-tree-based-modeling-scratch-in-python/

#----------------------------------------------------------------------------------
# RANDOM FOREST 
library(randomForest)
x <- cbind(x_train,y_train)
# Fitting model
fit <- randomForest(Species ~ ., x,ntree=500)
summary(fit)
#Predict Output 
predicted= predict(fit,x_test)

#--------------------------------------------------------------------------------------------------------
#BOOSTING 
# GBM in CARET with Cross Validation

	library(caret)
	fitControl <- trainControl(method = "cv",
      	                     number = 10, #5folds)
	tune_Grid <-  expand.grid(interaction.depth = 2,
                            n.trees = 500,
                            shrinkage = 0.1,
                            n.minobsinnode = 10)
	set.seed(825)
	fit <- train(y_train ~ ., data = train,
                 method = "gbm",
                 trControl = fitControl,
                 verbose = FALSE,
                 tuneGrid = gbmGrid)
	predicted= predict(fit,test,type= "prob")[,2] 

# XGBoost (eXtreme Gradient Boosting)
#https://www.analyticsvidhya.com/blog/2016/01/xgboost-algorithm-easy-steps/
# 1. Takes anly numeric input - convert categorical to numeric, in one step:
      sparse_matrix <- sparse.model.matrix(response ~ .-1, data = campaign)

	library(xgboost)
	library(readr)
	library(stringr)
	library(caret)
	library(car)
2. LOAD DATA
	set.seed(100)
	setwd("C:\\Users\\ts93856\\Desktop\\datasource")
	# load data
	df_train = read_csv("train_users_2.csv")
	df_test = read_csv("test_users.csv")
#3. Data cleanup
#4.Run
	xgb <- xgboost(data = data.matrix(X[,-1]), 
 	 label = y, 
 	 eta = 0.1,
	 max_depth = 15, 
	 nround=25, 
	 subsample = 0.5,
	 colsample_bytree = 0.5,
	 seed = 1,
	 eval_metric = "merror",
	 objective = "multi:softprob",
	 num_class = 12,
	 nthread = 3
	)

#5. Score the test population
# predict values in test set
	y_pred <- predict(xgb, data.matrix(X_test[,-1]))

another example of XGBoost at:
https://github.com/rachar1/DataAnalysis/blob/master/xgboost_Classification.R

#--------------------------------------------------------------------------------------------------------

http://trevorstephens.com/kaggle-titanic-tutorial/r-part-5-random-forests/

Random Forests process has the two sources of randomness:
1.Bagging takes a randomized sample of the rows in your training set, with replacement
2.Random Forests take only a subset of variables, typically the square root of the number available.


install.packages('randomForest')
library(randomForest)

library(titanic)
