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


