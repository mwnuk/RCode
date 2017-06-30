
#DECISION TREES - ENSAMBLE METHODS
# - BAGGING - Bootstrap Sampling - sub-samples with replacemnet - 
# - Random Forest - random split decision,godd for regression better for classification, no pruning,
#                   good for large sets, shows importance ov variables
# - Boosting - every split improves the chances using base Learning ML algorithm, implementations:
#              - XGBoost- parallel processing, fast, handles missing data, build in cross validation
#              - GBM - slower, easier, build in cross validation
#              - LightGBM - 7 times faster then XGBoost
#
#https://www.analyticsvidhya.com/blog/2016/04/complete-tutorial-tree-based-modeling-scratch-in-python/
#
# Ubalanced datasets, like rare froud detection 
#  - underSampling majority class
#  - overSampling minority class, library(ROSE), RandomOverSampling
#  - synthetic Minority overSamplig SMOT
#  - modified SMOT as MSMOTE
# Another approach Ensamble techniques 
#  - Adaptiva bagging - combination of regression, NNetworks and Decision Trees   
#  - Adaptive Boosting - converts week learner/classifier to a strong one ( week has low prediction rate)
#                        adjust weights in steps depending on a correctness of prediction
#  - GradientTree boosting - improves prediction in every step
#
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




############################################################################################################
#Unbalanced Binary Classification
############################################################################################################


#https://www.analyticsvidhya.com/blog/2016/03/practical-guide-deal-imbalanced-classification-problems/
#ROSE  package ( alternatively 	library(unbalanced))   
	install.packages("ROSE")
	library(ROSE)
# simulated dataset - Half circle filled data
	data(hacide)

#data set has 3 variables, cls,x1,x2 , already split into two sets: hacide.train, hacide.train
	str(hacide.train)
#check classes distribution-only 2% of cls are 1 - severly imbalanced set
	prop.table(table(hacide.train$cls))

# check prediction accuracy on imbalanced data set
	library(rpart)
	treeimb <- rpart(cls ~ ., data = hacide.train)
	pred.treeimb <- predict(treeimb, newdata = hacide.test)
# check accuracy of prediction using accuracy.meas from ROSE package
# measures: precision=1- no false positives, recall=0.02 - a lot of false negatives, F=0,167 - week accuracy of a model
	accuracy.meas(hacide.test$cls, pred.treeimb[,2])
#better way to estimate accuracy - 0.6 is a very low score
	roc.curve(hacide.test$cls, pred.treeimb[,2], plotit = F)

#over sampling until total amount ofsamles is 1960, so there are 980 1s and 980 0s
	data_balanced_over <- ovun.sample(cls ~ ., data = hacide.train, method = "over",N = 1960)$data
	table(data_balanced_over$cls)
#undersampling
	data_balanced_under <- ovun.sample(cls ~ ., data = hacide.train, method = "under", N = 40, seed = 1)$data
	table(data_balanced_under$cls)
#both
	data_balanced_both <- ovun.sample(cls ~ ., data = hacide.train, method = "both", p=0.5,                             N=1000, seed = 1)$data
	table(data_balanced_both$cls)
#package ofers synthetic sampling
	data.rose <- ROSE(cls ~ ., data = hacide.train, seed = 1)$data
	table(data.rose$cls)

#build decision tree models
	tree.rose <- rpart(cls ~ ., data = data.rose)
	tree.over <- rpart(cls ~ ., data = data_balanced_over)
	tree.under <- rpart(cls ~ ., data = data_balanced_under)
	tree.both <- rpart(cls ~ ., data = data_balanced_both)

#make predictions on unseen data
	pred.tree.rose <- predict(tree.rose, newdata = hacide.test)
	pred.tree.over <- predict(tree.over, newdata = hacide.test)
	pred.tree.under <- predict(tree.under, newdata = hacide.test)
	pred.tree.both <- predict(tree.both, newdata = hacide.test)

#model validation
#AUC ROSE	
	roc.curve(hacide.test$cls, pred.tree.rose[,2])
#AUC Oversampling
	roc.curve(hacide.test$cls, pred.tree.over[,2])
#AUC Undersampling
	roc.curve(hacide.test$cls, pred.tree.under[,2])
#AUC Both
	roc.curve(hacide.test$cls, pred.tree.both[,2])

# the winner is Synthetic sampling



#######################################################################################
# XBOOST again
#
# http://xgboost.readthedocs.io/en/latest/R-package/xgboostPresentation.html
#
#  objective functions: regression, classification and ranking
#
# Input Type: it takes several types of input data:
# 	-Dense Matrix: R‘s dense matrix, i.e. matrix ;
# 	-Sparse Matrix: R‘s sparse matrix, i.e. Matrix::dgCMatrix ;
# 	-Data File: local data files ;
# 	-xgb.DMatrix: its own class (recommended).
#  Note: sparse matrix zeros are not stored in memory
#######################################################################################

	library( xgboost) 
# Example dataset - can mushroom be eaten - from the same pacakge
	data(agaricus.train, package='xgboost')
	data(agaricus.test, package='xgboost')
	train <- agaricus.train
	test <- agaricus.test
# data in a form of dgCMatrix(sparse) and we are predicting numeric value Label
	str( train)
	dim(test$data)
	dim(test$label)

# objective = "binary:logistic": we will train a binary classification model ;
# max.deph = 2: the trees won’t be deep, because our case is very simple ;
# nthread = 2: the number of cpu threads we are going to use;
# nround = 2: there will be two passes on the data, the second one will enhance the model by further reducing the difference between ground truth and prediction.

	bstSparse <- xgboost(data = train$data, label = train$label, max.depth = 2, eta = 1, nthread = 2, nround = 2, objective = "binary:logistic")

#again - use Verbose 0,1,2 to see the progress
	bstSparse <- xgboost(data = train$data, label = train$label, max.depth = 2, eta = 1, nthread = 2, nround = 2, objective = "binary:logistic",verbose=2)

# again - Alternatively, you can put your dataset in a dense matrix, i.e. a basic R matrix.
	bstDense <- xgboost(data = as.matrix(train$data), label = train$label, max.depth = 2, eta = 1, nthread = 2, nround = 2, objective = "binary:logistic")

#prediction
	pred <- predict(bstSparse, test$data)

# size of the prediction vector
	print(length(pred))

#model performance - error 0.02 is very resonable
	err <- mean(as.numeric(pred > 0.5) != test$label)
	print(paste("test-error=", err))

# FOR ADVANCED FEATURES data needs to be in xgb.DMatrix
	dtrain <- xgb.DMatrix(data = train$data, label=train$label)
	dtest  <- xgb.DMatrix(data = test$data, label=test$label)

#Advanced feature 1 - watchlist shows probress in passes
	watchlist <- list(train=dtrain, test=dtest)
	bst <- xgb.train(data=dtrain, max.depth=2, eta=1, nthread = 2, nround=2, watchlist=watchlist, objective = "binary:logistic")

#Advanced feature 2 - Linear boosting, instead of default Tree boosting
	bst <- xgb.train(data=dtrain, booster = "gblinear", max.depth=2, nthread = 2, nround=2, watchlist=watchlist, eval.metric = "error", eval.metric = "logloss", objective = "binary:logistic")
#feature 3 - save/load model
	xgb.save(bst, "xgboost.model")
	bst2 <- xgb.load("xgboost.model")
	pred2 <- predict(bst2, test$data)
#feature 4 - compare models
	print(paste("sum(abs(pred2-pred))=", sum(abs(pred2-pred))))


#######################################################################################
# LightGBM
#
# https://www.analyticsvidhya.com/blog/2017/06/which-algorithm-takes-the-crown-light-gbm-vs-xgboost/
#
#
# Tuning Parameters of Light GBM
#
# FOR BEST FIR
# •num_leaves : This parameter is used to set the number of leaves to be formed in a tree. Theoretically relation between num_leaves and max_depth is num_leaves= 2^(max_depth). However, this is not a good estimate in case of Light GBM since splitting takes place leaf wise rather than depth wise. Hence num_leaves set must be smaller than 2^(max_depth) otherwise it may lead to overfitting. Light GBM does not have a direct relation between num_leaves and max_depth and hence the two must not be linked with each other.
# •min_data_in_leaf : It is also one of the important parameters in dealing with overfitting. Setting its value smaller may cause overfitting and hence must be set accordingly. Its value should be hundreds to thousands of large datasets.
# •max_depth: It specifies the maximum depth or level up to which tree can grow.
#
# 
# FOR SPEED
# •bagging_fraction : Is used to perform bagging for faster results
# •feature_fraction : Set fraction of the features to be used at each iteration
# •max_bin : Smaller value of max_bin can save much time as it buckets the feature values in discrete bins which is computationally inexpensive.
#
# FOR ACCURACY
# •Use bigger training data
# •num_leaves : Setting it to high value produces deeper trees with increased accuracy but lead to overfitting. Hence its higher value is not preferred.
# •max_bin : Setting it to high values has similar effect as caused by increasing value of num
#
#######################################################################################







