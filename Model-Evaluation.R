- R2 is the ratio between of the amount of variance explained by a regression model to the total variation of the data.
  It measures goodness of model.
  In Finance expect R2 in range 0.5-0.8, in Biology much lower in a range 0.005 -0.007
  R2=0.6 means that we reduced outcome variance around it means by 60%
- p value - probability that event happens negating  null Hypothesis - less then 0.05 - high probability
    values greater then 0.05 are not evidence against null hypothesis.
- t statistic merely measure how strong is the evidence that there is a nonzero association. 
   Even a weak effect can be extremely significant given enough data. Range 2-40, bigger better.
   By hand can be done using t-tests( 1-sample t-test, paired t-test,2-sample t-test) 
-F statistics - The F statistic tests the null hypothesis that none of the predictors has any effect. 
  Rejecting that null means concluding that *some* predictor has an effect, not that *all* of them do. Range 100-600. bigger better. 
-ANOVA( Analysis of Variance) - measures F-ratio, it is defined as the ratio of Mean Square(between groups) 
  to Mean Square(within groups)
- Chi-square Goodness of fit when outcome is categorical. It is used top calculated p-value and then judge the null hypothesis.

Is at least one predictor useful?
 F statistics is large


Which predictors are important?
 p value 

How good is the model?
R2 is large

---------------------------------------------------

LINIAR MODEL SELECTION 

In essence
if you have milion observations on 4 varaibles - do the least square
if you have 400 variables and 300 observations (p>n) need to reduce amount of features in 3 ways:
1. SUBSET SELECTION
     - best subset selection, consider every possible subset of p, and pick the best (CrossValidation, Cp,AIC,BIC,AdjustedR2 etc), good method for p<40
     - Forward StepWise methods - start with no predictors, add one p at a time, picking p wich will give the best model, total=p^2 models
     - Backward StepWise method - select all predictors, remove on p at a time, picking p wich will give the best model
             Estimate Test Error
               1. Indirectly compute training error - Cp,AIC,BIC,adjusterR2 - want them all small, but adjustedR2 as large as possible
               2. Directly estimate test error - validation and cross validation, 
          
2. SHRIKAGE parameters put penalty on the size of predictors
     - Ridge - lambda - shrinkage parameter, when lambda=0 same as least square method
     - Lasso
           lambda selection: split model in to 10 parts, 
                             for 9 parts apply Ridge,Lasso with whole range of lambda
                             record error on 10th part
                             do it 10 times, add up all records, get CrossValidation curve as a function of lambda 

    
3. DIMENSION REDUCTION - extracting important combination of variables 
     -Principal Component Regression - liniar combination of original predictor ( part of Unsupervised Learning)  - pick the direction( as a component) where data varies the most 
     -Partial Least Squares - select components in a suprvised way, chose components by looking at the response
-------------------------------------------------------------------
VALIDATION
	-TEST ERROR is the error we incur on a new data, that the model hasn't seen.
	-TRAINING ERROR is the error we we get applying the model to the same data it was trained from.
      - Ingredients of e prediction error are
               - bias - how far off on average the model is from the truth
               - variance - how much that estimate varies around its average
	-CROSS_VALIDATION gives good idea about test error of the model
      -LOOCV- Leave One Out Cross Validation - specail case when we don't have to refit the model at all. Training sets differ by 
              one observation, so they are very similar. 
	-BOOTSTRAP VALIDATION is useful to get idea of the variability of sd of en estimate and its bias,
              it samples from a dataset with replacement, it treats dataset like it would be entire population
Error is measured by
 - Mean Square Error  for Quantitative response
 - MissClassification Error Rate - for Discret Response Classification 
      
---------------------------------------------------

#VALIDATION - split data in to 50% train and 50% test part
	library(ISLR)
	set.seed(1)
	dim(Auto)
	train=sample(392,196)# take a 196 random elements from 392, because Auto has 392 records
      length(train)
	plot(sample)

#Mean Square Error of a liniar fit on Test set 
	lm.fit=lm(mpg~horsepower,data=Auto,subset=train)
	attach(Auto)
	mean((mpg-predict(lm.fit,Auto))[-train]^2)

#Mean Square Error of a polynomial fit degree2 on Test set 
	lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
	mean((mpg-predict(lm.fit2,Auto))[-train]^2)

#Mean Square Error of a polynomial fit degree2 on Test set 
	lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
	mean((mpg-predict(lm.fit3,Auto))[-train]^2)

----------------------------------------------------------------
# VALIDATION - Leave-One-Out Cross-Validation LOOCV

	glm.fit=glm(mpg~horsepower,data=Auto)
	coef(glm.fit)
	lm.fit=lm(mpg~horsepower,data=Auto)
	coef(lm.fit)

	library(boot)
	glm.fit=glm(mpg~horsepower,data=Auto)
	cv.err=cv.glm(Auto,glm.fit)
	cv.err$delta
	cv.error=rep(0,5)
	for (i in 1:5){
 		glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
		cv.error[i]=cv.glm(Auto,glm.fit)$delta[1]
	}
	cv.error
      mean(cv.error)
----------------------------------------------------------------
# VALIDATION k-Fold Cross-Validation
---	
#create folds using CARET
	require(caret)
	flds <- createFolds(Auto, k = 10, list = TRUE, returnTrain = FALSE)
	names(flds)[1] <- "train"
	Auto[ flds[[1]], ]  #fold1
	Auto[ flds[[2]], ]  #fold2

---
#10 fold using no packages
#Randomly shuffle the data
	Auto<-Auto[sample(nrow(Auto)),]

#Create 10 equally size folds
	folds <- cut(seq(1,nrow(Auto)),breaks=10,labels=FALSE)

#Perform 10 fold cross validation
for(i in 1:10){
    #Segement your data by fold using the which() function 
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- Auto[testIndexes, ]
    trainData <- Auto[-testIndexes, ]
    #Use the test and train data partitions however you desire...

    #lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=trainData)
    #mean((mpg-predict(lm.fit2,Auto))[-trainData]^2)

}


	set.seed(17)
	cv.error.10=rep(0,10) #replicates 0 ten times
	for (i in 1:10){
		glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
 		cv.error.10[i]=cv.glm(Auto,glm.fit,K=10)$delta[1]
 	}
	cv.error.10
      mean(cv.error.10)

----------------------------------------------------------------
# VALIDATION - The Bootstrap

	alpha.fn=function(data,index){
 		X=data$X[index]
 		Y=data$Y[index]
 		return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
 	}
	alpha.fn(Portfolio,1:100)
	set.seed(1)
#take 100 samples with replacement	
	alpha.fn(Portfolio,sample(100,100,replace=T))
#take 1000 bootstraps
	boot.out = boot(Portfolio,alpha.fn,R=1000)
#looking for standard error
	boot.out 
	plot (boot.out)


