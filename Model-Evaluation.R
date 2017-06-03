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
---------------------------------------------------

   



library(glmnet)
grid=10^seq(10,-2,length=100)
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)
dim(coef(ridge.mod))
ridge.mod$lambda[50]
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1,50]^2))
ridge.mod$lambda[60]
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[-1,60]^2))
predict(ridge.mod,s=50,type="coefficients")[1:20,]
set.seed(1)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]
ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid, thresh=1e-12)
ridge.pred=predict(ridge.mod,s=4,newx=x[test,])
mean((ridge.pred-y.test)^2)
mean((mean(y[train])-y.test)^2)
ridge.pred=predict(ridge.mod,s=1e10,newx=x[test,])
mean((ridge.pred-y.test)^2)
ridge.pred=predict(ridge.mod,s=0,newx=x[test,],exact=T)
mean((ridge.pred-y.test)^2)
lm(y~x, subset=train)
predict(ridge.mod,s=0,exact=T,type="coefficients")[1:20,]
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
ridge.pred=predict(ridge.mod,s=bestlam,newx=x[test,])
mean((ridge.pred-y.test)^2)
out=glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=bestlam)[1:20,]


