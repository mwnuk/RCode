
# TIME SERIES MODELING
# Stationary series - no dependence on time
# Step1: Visualize the series - trend, sesonality
# Step2: Stationarize the series - Detrending, Differencing, Seasonality 
# Step3: Find optimal parameters- ACF and PACF plots
# Step4: Build ARIMA model
# Step5: Make predictions
# ARMA model - AutoRegression Moving Average
#
#
#https://www.analyticsvidhya.com/blog/2015/12/complete-tutorial-time-series-modeling/

#The AirPassengers dataset consists of monthly totals of international airline passengers, 1949 to 1960.
	data(AirPassengers)
	class(AirPassengers)

	start(AirPassengers)
	end(AirPassengers)
	
	plot(AirPassengers)
	abline(reg=lm(AirPassengers~time(AirPassengers)))
	cycle(AirPassengers)
#This will aggregate the cycles and display a year on year trend
	plot(aggregate(AirPassengers,FUN=mean))
#Box plot across months will give us a sense on seasonal effect 
	boxplot(AirPassengers~cycle(AirPassengers))

#Test if set is stationary
	library("tseries")
	adf.test(diff(log(AirPassengers)), alternative="stationary", k=0)
#FIND PARAMETERS
	acf(log(AirPassengers))
	acf(diff(log(AirPassengers)))
	pacf(diff(log(AirPassengers)))

# BUILD MODEL
	(fit <- arima(log(AirPassengers), c(0, 1, 1),seasonal = list(order = c(0, 1, 1), period = 12)))


#MAKE PREDICTION for next 10 years
	pred <- predict(fit, n.ahead = 10*12)

	ts.plot(AirPassengers,2.718^pred$pred, log = "y", lty = c(1,3))
