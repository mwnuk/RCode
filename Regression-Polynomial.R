# NONLINIAR MODELS
# 1.Polynomials
# 2.Step function
# 1.Splines
# 2.Local Regression
# 1.General Additive Models GAM


# 1. POLYNOMIALS
#https://www.r-bloggers.com/fitting-polynomial-regression-in-r/
#Danger:high order polynomials (n > 4) may lead to over-fitting


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
#notice that first two coefficients are signifficant, 3 one litte less,
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










