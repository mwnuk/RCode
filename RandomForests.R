
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
######################################################################
# BAGGING from islr
######################################################################
	library(MASS)
	library(randomForest)
	set.seed(1)
      dim(Boston)
# build train set of 300 out of total of 506 records
	train = sample(1:nrow(Boston), 300)
#response is medv - medium value of the house
	bag.boston=randomForest(medv~.,data=Boston,subset=train,mtry=13,importance=TRUE)
	bag.boston
#Bagging
	yhat.bag = predict(bag.boston,newdata=Boston[-train,])
	plot(yhat.bag, boston.test)
	abline(0,1)
	mean((yhat.bag-boston.test)^2)

#limit number of trees to 25
	bag.boston=randomForest(medv~.,data=Boston,subset=train,mtry=13,ntree=25)
	yhat.bag = predict(bag.boston,newdata=Boston[-train,])
	mean((yhat.bag-boston.test)^2)


######################################################################
# RANDOM FOREST from islr
######################################################################

#RandomForest grows 500 trees - 
# mtry is only tuning parameter ( number of variables selected at each split, 
#                                 example: from 13 parameters in Housing data we pick 6 random to consider at evert split )  
	set.seed(1)
	rf.boston=randomForest(medv~.,data=Boston,subset=train,mtry=6,importance=TRUE)
	yhat.rf = predict(rf.boston,newdata=Boston[-train,])
	mean((yhat.rf-boston.test)^2)
	importance(rf.boston)

# find a good mtry
	oob.err = double(13)
      test.err= double(13) 
	for( mtry in (1:13) ){
		fit=randomForest(medv~.,data=Boston,subset=train,mtry=mtry,mtree=400)
	    #exract Mean Square Error (mse) 
		oob.err[mtry]=fit$mse[400]
	    #predict on test data ( minus train) 
            pred=predict(fit,Boston[-train,])
          #compute mean square erorr with construction
		test.err[mtry]=with(Boston[-train,],mean((medv-pred)^2))
	}
#plot error
	matplot(1:mtry,cbind(test.err,oob.err),pch=19,col=c("red","blue"),type="b",ylab="Mean Square Error")
	legend("topright",legend=c("OOB","Test"),pch=19,col=c("red","blue"))
#conclusion: 1. best value is around 4
             2. error came down from 26 for 1 variable to eleven - powerful technique
             4  at 13 variables it it bagging, so there is a slight improvement from bagging
   

######################################################################
# BOOSTING from islr
######################################################################
# boosting grows shorter trees, 
	library(MASS)
	library(gbm)
	set.seed(1)
#Ask boosting to 10000 trees, depth means amount of splits at each of the trees
	boost.boston=gbm(medv~.,data=Boston[train,],distribution="gaussian",n.trees=10000,interaction.depth=4)
	summary(boost.boston)
#summary shows var importance: rm and lstat(% of low status peaple) are most important variables
	par(mfrow=c(1,2))
	plot(boost.boston,i="rm")
	plot(boost.boston,i="lstat")
#predict
	yhat.boost=predict(boost.boston,newdata=Boston[-train,],n.trees=5000)
	mean((yhat.boost-boston.test)^2)

# best way is to use Cross validation to choose number of trees
#ture parameters and compare Mean Square Error
	boost.boston=gbm(medv~.,data=Boston[train,],distribution="gaussian",n.trees=5000,interaction.depth=4,shrinkage=0.2,verbose=F)
	yhat.boost=predict(boost.boston,newdata=Boston[-train,],n.trees=5000)
	mean((yhat.boost-boston.test)^2)

#small test to show how error changes with amount of trees
         

#--------------------------------------------------------------------------------------------------------
#BOOSTING again
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
