
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

#(p,d,q)=(0,1,1) after few tries to get smallest AIC/BIC
# BUILD MODEL
	(fit <- arima(log(AirPassengers), c(0, 1, 1),seasonal = list(order = c(0, 1, 1), period = 12)))


#MAKE PREDICTION for next 10 years
	pred <- predict(fit, n.ahead = 10*12)

	ts.plot(AirPassengers,2.718^pred$pred, log = "y", lty = c(1,3))


# Apply fitted model to later data
	air.model2 <- Arima(window(AirPassengers,start=1957),model=air.model)

# Forecast accuracy measures on the log scale.
# in-sample one-step forecasts.
	accuracy(air.model)
# out-of-sample one-step forecasts.
	accuracy(air.model2)
# out-of-sample multi-step forecasts
	accuracy(forecast(air.model,h=48,lambda=NULL),
      	   log(window(AirPassengers,start=1957)))



############################################################################
#
#  AutoArima 
#
#
#
#   http://rpubs.com/ajaydecis/ts
###########################################################################
	library(Rcmdr)
	library(RcmdrPlugin.epack)
	
	library(quadprog)
	install.packages(forecast)
	library(forecast)
      require(forecast)

	data("AirPassengers")
	a=ets(AirPassengers)
	a

	predict(a,10)

	str(predict(a,10))

	b=auto.arima(AirPassengers)
	b

	predict(b,10)
      ts.plot(AirPassengers)
	newdataset=forecast::predar3(b,fore1=48)


??arima

############################################################################
#
#  Few simple model
#
#
###########################################################################
# Model 1
	WWWusage %>% forecast %>% plot
	fit <- ets(window(WWWusage, end=60))
	fc <- forecast(WWWusage, model=fit)
# Model 2
	fit <- ets(USAccDeaths)
	plot(forecast(fit,h=48))
# Model 3
fit <- Arima(usconsumption[,1], order=c(0,0,3))

http://www.sciencedirect.com/science/article/pii/S0378375802001866


