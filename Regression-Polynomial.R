# NONLINIAR MODELS
# 1.Polynomials
# 2.Step function
# 3.Splines
# 4.Local Regression
# 5.General Additive Models GAM


# 1. POLYNOMIALS
# https://www.r-bloggers.com/fitting-polynomial-regression-in-r/
# Danger:high order polynomials (n > 4) may lead to over-fitting
# Liniar regression have small error from Bias and large from a Variance
# but Polynomial regression has high bias and low variance

	set.seed(20)
#Predictor (q). Use seq for generating equally spaced sequences fast
	q <- seq(from=0, to=20, by=0.1)
#Value to predict (y):
	y <- 500 + 0.4 * (q-10)^3
#Some noise is generated and added to the real signal (y):
	noise <- rnorm(length(q), mean=10, sd=80)
	noisy.y <- y + noise
#Plot of the noisy signal:
	plot(q,noisy.y,col='deepskyblue4',xlab='q',main='Observed data')
	lines(q,y,col='firebrick1',lwd=3)set.seed(20)
#Predictor (q). Use seq for generating equally spaced sequences fast
	q <- seq(from=0, to=20, by=0.1)
#Value to predict (y):
	y <- 500 + 0.4 * (q-10)^3
#Some noise is generated and added to the real signal (y):
	noise <- rnorm(length(q), mean=10, sd=80)
	noisy.y <- y + noise


#Plot of the noisy signal:
	plot(q,noisy.y,col='deepskyblue4',xlab='q',main='Observed data')
	lines(q,y,col='firebrick1',lwd=3)
# MODEL
	model <- lm(noisy.y ~ poly(q,3))
#Or (  it cause problems with correlated variables ) 
	model3 <- lm(noisy.y ~ q + I(q^2) + I(q^3))
	model2 <- lm(noisy.y ~ q + I(q^2))
	model1 <- lm(noisy.y ~q  )
#Confidence intervals for model parameters:
	confint(model, level=0.95)

#Predicted values and confidence intervals:
	predicted.intervals <- predict(model,data.frame(x=q),interval='confidence',level=0.99)
	#predicted.intervals <- predict(model1,data.frame(x=q),interval='confidence',level=0.99)
	#predicted.intervals <- predict(model2,data.frame(x=q),interval='confidence',level=0.99)
	#predicted.intervals <- predict(model3,data.frame(x=q),interval='confidence',level=0.99)
#Add lines to the existing plot:
	lines(q,predicted.intervals[,1],col='green',lwd=3) #fit
	lines(q,predicted.intervals[,2],col='black',lwd=1) #lower
	lines(q,predicted.intervals[,3],col='black',lwd=1) #upper
#Add a legend:
	legend("bottomright",c("Observ.","Signal","Predicted"), 
       col=c("deepskyblue4","red","green"), lwd=3)


#################################################################################################

# Another example of Polynomial fit from ISLR
	library(ISLR)
	attach(Wage)

	fit=lm(wage~poly(age,4),data=Wage)
	coef(summary(fit))
#notice that first two coefficients are signifficant, 3rd one litte less,
# so looks like cubic polynomial would be sufficient.
	fit2=lm(wage~poly(age,4,raw=T),data=Wage)
	coef(summary(fit2))
	fit2a=lm(wage~age+I(age^2)+I(age^3)+I(age^4),data=Wage)
	coef(fit2a)
	fit2b=lm(wage~cbind(age,age^2,age^3,age^4),data=Wage)

#prepare the plot:

	agelims=range(age)
	age.grid=seq(from=agelims[1],to=agelims[2])  # default by 1 
      age.grid   # sequence of integers

	preds=predict(fit,newdata=list(age=age.grid),se=TRUE) #with se standard errors

	se.bands=cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se.fit)  

	par(mfrow=c(1,2),mar=c(4.5,4.5,1,1),oma=c(0,0,4,0))
	plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
	title("Degree-4 Polynomial",outer=T)
	lines(age.grid,preds$fit,lwd=2,col="blue")
	matlines(age.grid,se.bands,lwd=1,col="blue",lty=3)


# Splines
	library(ISLR)
	attach(Wage)
	library(splines)
	agelims=range(age)
	age.grid=seq(from=agelims[1],to=agelims[2])
	
	fit=lm(wage~bs(age,knots=c(25,40,60)),data=Wage)
	pred=predict(fit,newdata=list(age=age.grid),se=T)
	plot(age,wage,col="gray")
	lines(age.grid,pred$fit,lwd=2)
	lines(age.grid,pred$fit+2*pred$se,lty="dashed")
	lines(age.grid,pred$fit-2*pred$se,lty="dashed")

dim(bs(age,knots=c(25,40,60)))
dim(bs(age,df=6))
attr(bs(age,df=6),"knots")
fit2=lm(wage~ns(age,df=4),data=Wage)
pred2=predict(fit2,newdata=list(age=age.grid),se=T)
lines(age.grid, pred2$fit,col="red",lwd=2)
plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
title("Smoothing Spline")
fit=smooth.spline(age,wage,df=16)
fit2=smooth.spline(age,wage,cv=TRUE)
fit2$df
lines(fit,col="red",lwd=2)
lines(fit2,col="blue",lwd=2)
legend("topright",legend=c("16 DF","6.8 DF"),col=c("red","blue"),lty=1,lwd=2,cex=.8)
plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
title("Local Regression")
fit=loess(wage~age,span=.2,data=Wage)
fit2=loess(wage~age,span=.5,data=Wage)
lines(age.grid,predict(fit,data.frame(age=age.grid)),col="red",lwd=2)
lines(age.grid,predict(fit2,data.frame(age=age.grid)),col="blue",lwd=2)
legend("topright",legend=c("Span=0.2","Span=0.5"),col=c("red","blue"),lty=1,lwd=2,cex=.8)

# GAMs

gam1=lm(wage~ns(year,4)+ns(age,5)+education,data=Wage)
library(gam)
gam.m3=gam(wage~s(year,4)+s(age,5)+education,data=Wage)
par(mfrow=c(1,3))
plot(gam.m3, se=TRUE,col="blue")
plot.gam(gam1, se=TRUE, col="red")
gam.m1=gam(wage~s(age,5)+education,data=Wage)
gam.m2=gam(wage~year+s(age,5)+education,data=Wage)
anova(gam.m1,gam.m2,gam.m3,test="F")
summary(gam.m3)
preds=predict(gam.m2,newdata=Wage)
gam.lo=gam(wage~s(year,df=4)+lo(age,span=0.7)+education,data=Wage)
plot.gam(gam.lo, se=TRUE, col="green")
gam.lo.i=gam(wage~lo(year,age,span=0.5)+education,data=Wage)
library(akima)
plot(gam.lo.i)
gam.lr=gam(I(wage>250)~year+s(age,df=5)+education,family=binomial,data=Wage)
par(mfrow=c(1,3))
plot(gam.lr,se=T,col="green")
table(education,I(wage>250))
gam.lr.s=gam(I(wage>250)~year+s(age,df=5)+education,family=binomial,data=Wage,subset=(education!="1. < HS Grad"))
plot(gam.lr.s,se=T,col="green")








