# SUPPORT VECTOR MACHINES ( CLASSIFICATION AND REGRESSION)
# SVM slow for large dataseta.
# Two parameters: 
#  - cost is a general penalizing parameter for C-classi?cation and 1 to 1000
#  - gamma is the radial basis function-specific kernel parameter.
# e1071 has function  tune.svm()
#######################################################################################
#https://cran.r-project.org/web/packages/e1071/vignettes/svmdoc.pdf

# CLASSIFICATION 
# Compare SVM vs rpart ( Recursive Partitioning and Regression Trees)
#-----------------------------------------
	library(e1071) 
	library(rpart) 
	data(Glass, package="mlbench") 
# split data into a train and test set 
	index <- 1:nrow(Glass) 
	testindex <- sample(index, trunc(length(index)/3)) 
	testset <- Glass[testindex,] 
	trainset <- Glass[-testindex,]

#Both for the SVM and the partitioning tree (via rpart()), 
#we ?t the model and try to predict the test set values:
	svm.model <- svm(Type ~ ., data = trainset, cost = 100, gamma = 1) 
	svm.pred <- predict(svm.model, testset[,-10])
#(The dependent variable, Type, has column number 10. cost is a general 
#penalizing parameter for C-classi?cation and gamma is the radial basis 
#function-speci?c kernel parameter.)
	rpart.model <- rpart(Type ~ ., data = trainset) 
      rpart.pred <- predict(rpart.model, testset[,-10], type = "class")


# compute svm confusion matrix > 
	table(pred = svm.pred, true = testset[,10])
	table(pred = rpart.pred, true = testset[,10])


#validation, what is a percentage of accuratly predicted values
      round(sum(svm.pred==testset$Type,na.rm=TRUE)/
         length(testset$Type), digits=2 )
#same validation using Caret package( notice kappa measure) 
	library("caret") #confusion matrix	
	confusionMatrix( table(svm.pred,testset$Type))
	confusionMatrix( table(rpart.pred,testset$Type))
SMV gives better results then Decision Tree (0.63 vs 0.61)

##############################################################################
#  REGRESSION
# The regression capabilities of SVMs are demonstrated on the ozone data. 
##############################################################################
	library(e1071) 
	library(rpart) 
      data(Ozone, package="mlbench") 

## split data into a train and test set 
	index <- 1:nrow(Ozone) 
	testindex <- sample(index, trunc(length(index)/3)) 
	testset <- na.omit(Ozone[testindex,-3]) 
	trainset <- na.omit(Ozone[-testindex,-3])

	svm.model <- svm(V4 ~ ., data = trainset, cost = 1000, gamma = 0.0001) 
	svm.pred <- predict(svm.model, testset[,-3]) 

#tricky way of calculating MSE (Mean Square Error) 
	crossprod(svm.pred - testset[,3]) / length(testindex)

	rpart.model <- rpart(V4 ~ ., data = trainset) 
	rpart.pred <- predict(rpart.model, testset[,-3]) 
	crossprod(rpart.pred - testset[,3]) / length(testindex)


SMV gives better results then Decision Tree

###########################################################################
# MATRIX OPERATIONS
#crossproduct is equivalent to x %*% y matrix multiplication
#help (crossprod)

x <- 1:4
(z <- x %*% x)    # scalar ("inner") product (1 x 1 matrix)
crossprod(x)
drop(z)             # as scalar

y <- diag(x)
z <- matrix(1:12, ncol = 3, nrow = 4)
y %*% z
crossprod(y,z)
y %*% x
crossprod(y,x)
x %*% z

length(testset[,3])
length(svm.pred)
svm.pred[0:5]
testset[0:5,3]
#Mean Square Error
crossprod(svm.pred[0:5] - testset[0:5,3]) / length(testindex)

###########################################################################
#
# SVM TUNING AND PLOTTING
#
###########################################################################
# from ISLR
# SVM CLASSIFIER
# Prepare and plot the data
	set.seed(1)
	x=matrix(rnorm(20*2), ncol=2)
	y=c(rep(-1,10), rep(1,10))
	x[y==1,]=x[y==1,] + 1
	plot(x, col=(3-y))
# Plot SVM
	dat=data.frame(x=x, y=as.factor(y))
	library(e1071)
	svmfit=svm(y~., data=dat, kernel="linear", cost=10,scale=FALSE)
	plot(svmfit, dat)
# change cost
	svmfit$index
	summary(svmfit)
	svmfit=svm(y~., data=dat, kernel="linear", cost=0.1,scale=FALSE)
	plot(svmfit, dat)

#Tune SVM - cost and gamma
	svmfit$index
	set.seed(1)
	tune.out=tune(svm,y~.,data=dat,kernel="linear",ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))
	summary(tune.out)
	bestmod=tune.out$best.model
	summary(bestmod)

# Validate best model
	xtest=matrix(rnorm(20*2), ncol=2)
	ytest=sample(c(-1,1), 20, rep=TRUE)
	xtest[ytest==1,]=xtest[ytest==1,] + 1
	testdat=data.frame(x=xtest, y=as.factor(ytest))
	ypred=predict(bestmod,testdat)
	table(predict=ypred, truth=testdat$y)

#compare model with cost=0.01
	svmfit=svm(y~., data=dat, kernel="linear", cost=.01,scale=FALSE)
	ypred=predict(svmfit,testdat)
	table(predict=ypred, truth=testdat$y)
	x[y==1,]=x[y==1,]+0.5
	plot(x, col=(y+5)/2, pch=19)

# compare model with cost=1e5
	dat=data.frame(x=x,y=as.factor(y))
	svmfit=svm(y~., data=dat, kernel="linear", cost=1e5)
	summary(svmfit)
	plot(svmfit, dat)
# compare model with cost=1
	svmfit=svm(y~., data=dat, kernel="linear", cost=1)
	summary(svmfit)
	plot(svmfit,dat)



# Support Vector Machine - Radial kernel

	set.seed(1)
	x=matrix(rnorm(200*2), ncol=2)
	x[1:100,]=x[1:100,]+2
	x[101:150,]=x[101:150,]-2
	y=c(rep(1,150),rep(2,50))
	dat=data.frame(x=x,y=as.factor(y))
	plot(x, col=y)

	train=sample(200,100)
	svmfit=svm(y~., data=dat[train,], kernel="radial",  gamma=1, cost=1)
	plot(svmfit, dat[train,])
	summary(svmfit)

	svmfit=svm(y~., data=dat[train,], kernel="radial",gamma=1,cost=1e5)
	plot(svmfit,dat[train,])

	set.seed(1)
	tune.out=tune(svm, y~., data=dat[train,], kernel="radial", ranges=list(cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4)))
	summary(tune.out)
	table(true=dat[-train,"y"], pred=predict(tune.out$best.model,newx=dat[-train,]))










