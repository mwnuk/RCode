# VALIDATION        
# 1. Using training set and approximate test validation
#       - Cp, AIC,BIC,
# 2. Test set against traning set using Cross Validation
#       - by hand
#       - using CARET or DAAG package
###################################################################################

###################################################################################
#
#   BY SPLIT (using CARET) 
#
#  The example below splits the iris dataset so that 80% is used for training a 
#  Naive Bayes model and 20% is used to evaluate the models performance.
#
###################################################################################
http://machinelearningmastery.com/how-to-estimate-model-accuracy-in-r-using-the-caret-package/

# load the libraries
	library(caret)
	#library(klaR)
# load the iris dataset
	data(iris)
# define an 80%/20% train/test random split of the dataset using CARET
	split=0.80
	trainIndex <- createDataPartition(iris$Species, p=split, list=FALSE)
	data_train <- iris[ trainIndex,]
	data_test <- iris[-trainIndex,]
# train a naive bayes model
	model <- NaiveBayes(Species~., data=data_train, laplace=1)
# make predictions
	x_test <- data_test[,1:4]  # remove 5th column
	y_test <- data_test[,5]    # only last column
	predictions <- predict(model, x_test)
# summarize results
	confusionMatrix(predictions$class, y_test)


###################################################################################
#
#   BOOTSTRAP Validation (using CARET) 
#
#   Bootstrap resampling involves taking random samples from the dataset (with re-selection) 
#   against which to evaluate the model. In aggregate, the results provide an indication of 
#   the variance of the models performance. Typically, large number of resampling iterations 
#   are performed (thousands or tends of thousands).
#
###################################################################################
# load the library
	library(caret)
# load the iris dataset
	data(iris)
# define training control
	train_control <- trainControl(method="boot", number=100)
# train the model
	model <- train(Species~., data=iris, trControl=train_control, method="nb")
# summarize results
	print(model)

###################################################################################
#
#   CROSS VALIDATION (using CARET) 
#
#   The k-fold cross validation method involves splitting the dataset into k-subsets. 
#   For each subset is held out while the model is trained on all other subsets. This 
#   process is completed until accuracy is determine for each instance in the dataset, 
#   and an overall accuracy estimate is provided.
#   Can be repeated few times with averaged results:
#        train_control <- trainControl(method="repeatedcv", number=10, repeats=3)
#
###################################################################################

# load the library
	library(caret)
# load the iris dataset
	data(iris)
# define training control
	train_control <- trainControl(method="cv", number=10)
# fix the parameters of the algorithm
	grid <- expand.grid(.fL=c(0), .usekernel=c(FALSE))
# train the model
	model <- train(Species~., data=iris, trControl=train_control, method="nb", tuneGrid=grid)
# summarize results
	print(model)


###################################################################################
#
#   CROSS VALIDATION (using DAAG) 
# from book "Data Analysis and Graphics Using R", Maindonald, J.H.
#
#
###################################################################################

mydata <- data.frame(ymat, xmat)
fit    <- lm(Species ~ ., data=iris)
library(DAAG)
cv.lm(df=iris, fit, m=10) # ten-fold cross validation 
??DAAG


###################################################################################
# 
#  LEAVE ONE OUT  Cross Validation (LOOCV), (using CARET) 
# 
#  a data instance is left out and a model constructed on all other data instances in 
#  the training set. This is repeated for all data instances.
###################################################################################

# load the library
	library(caret)
# load the iris dataset
	data(iris)
# define training control
	train_control <- trainControl(method="LOOCV")
# train the model
	model <- train(Species~., data=iris, trControl=train_control, method="nb")
# summarize results
	print(model)





