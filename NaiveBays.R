# DISCRIMINANT MODELS
# 1. NaiveBayes   - probability based
# 2. KNN          - k-nearest neighbors 
# Naive Bayes requires Categorical Variable as a target, 
# target can be multilevel.
# it takes both: categorical and numerical variables
# Iris dataset, 150 observations of 5 variables
###################################################################################
library("psych") # pairs panels
library("e1071") #naive bays
library("caret") #confusion matrix
library( data.table)
attach(iris)

#iris Fishers dataset
	head(iris)
	pairs.panels(iris)
#Variable iris$Species is categorical by its nature ( 3 categoriees)
# in other cases convert continuous var to categorical:
#   outvar<-ifelse( inputvar1<0.3, "low", 
#                    ifelse(input1<0.6, "mid","hi" )

#Create data frame with all important inputs and the output variable
	wb<-data.frame(Sepal.Length ,Sepal.Width,Petal.Length, Petal.Width, Species )
 

	set.seed(2017)
      wb.size<-length(Species)
      wb.train.size=round(wb.size*0.7)   #70% for training
      wb.validation.size<-wb.size-wb.train.size   # the rest for testing
      wb.train.idx<-sample(seq(1:wb.size),wb.train.size) # random indices used for training
      wb.train.sample<-wb[wb.train.idx,]
      wb.validation.sample<-wb[-wb.train.idx,]
#build a classifier
      classf<-naivebayes(  
              subset(wb.train.sample,select = -Species),   #remove output( do we really need it?) 
              wb.train.sample$Species, laplace=1)      #specify output
      classf
#predictor
      pred<-predict(classf,
		subset(wb.validation.sample,select = -Species))   #remove output

#confusion matrix- columns have predictions, rows test data
      table(pred,wb.validation.sample$Species)

#validation, what is a percentage of accuratly predicted values
      round(sum(pred==wb.validation.sample$Species,na.rm=TRUE)/
         length(wb.validation.sample$Species), digits=2 )

#validation and performance report using Caret package - notice kappa statistics
	confusionMatrix( table(pred,wb.validation.sample$Species))

#result is pretty good 96% and its hard to improve it
# steps to improve:
# - change variables, 
# - remove skewed ones, 
# - diffrent random seed to have different split between train and validation data
# 



################################################################
#K-NN CLASSIFIER
################################################################

library(class)

	pred<-knn( subset(wb.train.sample,select = -Species), 
           subset(wb.validation.sample,select = -Species), 
           factor(wb.train.sample$Species),
		k=3,prob=TRUE,use.all=TRUE)
#Validation using Caret package
confusionMatrix( table(pred,wb.validation.sample$Species))
# results are 96% again, really good



################################################################
# CARET DATA SPLITTING
# http://machinelearningmastery.com/how-to-estimate-model-accuracy-in-r-using-the-caret-package/
################################################################

# load the library
library(caret)

################################################################
# CROSS VALIDATION
################################################################


# define training control - The final model accuracy is taken as the mean from the number of repeats.
	train_control <- trainControl(method="repeatedcv", number=10, repeats=3)

# train the model
	model <- train(Species~., data=wb, trControl=train_control, method="nb")
# summarize results
	print(model)
traceback()
sessionInfo()    

################################################################
# LEAVE ONE OUT CROSS VALIDATION _ LOOCV
################################################################
a data instance is left out and a model constructed on all other data instances in the training set. 
This is repeated for all data instances.

# define training control
	train_control <- trainControl(method="LOOCV")
# train the model
	model <- train(Species~., data=iris, trControl=train_control, method="nb")
# summarize results
	print(model)



       
