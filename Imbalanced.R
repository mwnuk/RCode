#
# Ubalanced datasets, like rare froud detection 
#
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
############################################################################################################



############################################################################################################
#  Unbalanced Binary Classification - Sampling approach
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
# Unbalanced Binary Classification - Ensamble approach
#
# XBOOST 
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
# Example dataset - is mushroom eatable - from the same pacakge
	data(agaricus.train, package='xgboost')
	data(agaricus.test, package='xgboost')
	train <- agaricus.train
	test <- agaricus.test
# data in a form of dgCMatrix(sparse) and we are predicting numeric value Label
	str( train)
	dim(test$data)
	dim(test$label)

# objective = "binary:logistic": for binary classification, "reg:liniar" for regression
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
#cross validation
      cv.res=xgb.cv(data = train$data, nfold=5, label=train$label,nround=2,objective = "binary:logistic",eval_metric="auc" )  

# FOR ADVANCED FEATURES data needs to be in xgb.DMatrix
	dtrain <- xgb.DMatrix(data = train$data, label=train$label)
	dtest  <- xgb.DMatrix(data = test$data, label=test$label)

#Advanced feature 1 - watchlist shows progress in passes
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
############################################################################



