# VALIDATION        
# 1. Indirect: Using training set and approximate test validation
#       - Cp, AIC,BIC,
# 2. Direct: Test set against traning set
#       - split for training/test data set
#       - Cross Validation CARET or DAAG package or by hand
#       - LOOCV - LEaveOne out Cross Validation
#       - Bootstrap Validation Caret or by hand
# Validating Classification model 
#     - Confusion Matrix either by hand or from Caret package, Accuracy, Kappa
# Validating Regression Models 
#     - MSE - Mean Square Error 
# 

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
	library(klaR)
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

############################################################
# SIMPLE SPLIT 
############################################################
	set.seed(502)
	ind=sample(2,nrow(iris),replace=T,prob=c(.7,.3))
	train<-iris[ind==1,]
	test<-iris[ind==2,]
length( iris$Species) 
length( train$Species)
length( test$Species)


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
	grid <- expand.grid(.fL=c(0), .usekernel=c(FALSE), .adjust=c(FALSE))
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


###################################################################################
# 
#  FIVE FOLD CROSS VALIDATION By Hand
# 
#  a data instance is left out and a model constructed on all other data instances in 
#  the training set. This is repeated for all data instances.
###################################################################################
library(plyr)
library(randomForest)

	data <- iris

# in this cross validation example, we use the iris data set to 
# predict the Sepal Length from the other variables in the dataset 
# with the random forest model 



	k = 5 #Folds

# sample from 1 to k, nrow times (the number of observations in the data)
	data$id <- sample(1:k, nrow(data), replace = TRUE)
	list <- 1:k

# prediction and testset data frames that we add to with each iteration over
# the folds

	prediction <- data.frame()	
	testsetCopy <- data.frame()

#Creating a progress bar to know the status of CV

	progress.bar <- create_progress_bar("text")
	progress.bar$init(k)



for (i in 1:k){

  # remove rows with id i from dataframe to create training set
  # select rows with id i to create test set

  	trainingset <- subset(data, id %in% list[-i])

  	testset <- subset(data, id %in% c(i))
  
  # delay to see progress bar
	Sys.sleep(2) 

  # run a random forest model

  	mymodel <- randomForest(trainingset$Sepal.Length ~ ., data = trainingset, ntree = 100)

                                                     

  # remove response column 1, Sepal.Length

  	temp <- as.data.frame(predict(mymodel, testset[,-1]))

  # append this iteration's predictions to the end of the prediction data frame

  	prediction <- rbind(prediction, temp)

  # append this iteration's test set to the test set copy data frame
  # keep only the Sepal Length Column

	testsetCopy <- rbind(testsetCopy, as.data.frame(testset[,1]))
  	progress.bar$step()
}



# add predictions and actual Sepal Length values

	result <- cbind(prediction, testsetCopy[, 1])

	names(result) <- c("Predicted", "Actual")

	result$Difference <- abs(result$Actual - result$Predicted)


# As an example use Mean Absolute Error as Evalution 

	summary(result$Difference)


