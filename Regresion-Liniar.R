http://www.r-tutor.com/elementary-statistics/simple-linear-regression/prediction-interval-linear-regression

# Liniar regression is sensitive to outliers 
# Model is no good if there is relationship between residuals and predicted Values.
# Liniar regression suffers from:
#  -Multi-collinearity - when vars a re corelated, 
#  -autocorrelation- error terms are time correlated, response from a previous time period becomes a predictor
#  -heteroskedastisity - when vertical spread of the data is increasing when X changes
# Liniar regression have small error from Bias and large from a Variance
# but Polynomial regression has high bias and low variance
# Performance measure:
# - Sum Of Sqaure Errors - SSE - sensitive to number of data points
# - Rsquare - (act-predicted)/(act-mean), range 0-1, bigger better, 
# - adjusted R2 - does not depend on amount of predictors, use always for models with more then 1 predictor
# Example: Single Var model with R2=.27 means that 27% of variance can be expalined by that var 
#
### HANDLING NA #####
options("na.action")  # global setting
na.omit or na.exclude  - skip
na.pass - keep all data including NAs
na.fail - return object only if contains no missing values


##################################################
# SIMPLE LINIAR MODEL
##################################################

	?faithful
	summary(faithful)
	faithful
	plot(faithful[, -3], main = f.tit,
     		xlab = "Eruption time (min)",
     	ylab = "Waiting time to next eruption (min)")
	lines(lowess(faithful$eruptions, faithful$waiting, f = 2/3, iter = 3),
      	col = "red")
	)
#  help (lowess)

	attach(faithful)     # attach the data frame 
	eruption.lm = lm(eruptions ~ waiting) 

	newdata = data.frame(waiting=80) 

	predict(eruption.lm, newdata, interval="predict") 

#The 95% prediction interval of the eruption duration for the waiting time of 
#80 minutes is between 3.1961 and 5.1564 minutes. 

#help(predict.lm) 

	predict(eruption.lm, newdata, interval="predict",level=0.6) 

	predict(eruption.lm,  interval="predict",level=0.6) 

###########################################################################
x <- rnorm(15)
y <- x + rnorm(15)
predict(lm(y ~ x))
new <- data.frame(x = seq(-3, 3, 0.5))
predict(lm(y ~ x), new, se.fit = TRUE)
pred.w.plim <- predict(lm(y ~ x), new, interval = "prediction")
pred.w.clim <- predict(lm(y ~ x), new, interval = "confidence")
matplot(new$x, cbind(pred.w.clim, pred.w.plim[,-1]),
        lty = c(1,2,2,3,3), type = "l", ylab = "predicted y")


###########################################################################
cbind - adding columns
help (cbind)
m<-cbind(1, 1:7)
cbind(m, 8:14)

rbind - adding rows 
 x = rbind(c(10, "[]", "[[1,2]]"), c(20, "[]", "[[1,3]]"))
 x
     [,1] [,2] [,3]     
[1,] "10" "[]" "[[1,2]]"
[2,] "20" "[]" "[[1,3]]"


###########################################################################
## MULTIPLE LINIAR REGRESSION
###########################################################################
library(MASS)
library(ISLR)

BOSTON 


	lm.fit=lm(medv~lstat+age,data=Boston)
	summary(lm.fit)
	lm.fit=lm(medv~.,data=Boston)
	summary(lm.fit)
	library(car)
	vif(lm.fit)
#when all is considered, age is not signifficant any more, remove it
	lm.fit1=lm(medv~.-age,data=Boston)
	summary(lm.fit1)
	lm.fit1=update(lm.fit, ~.-age)



