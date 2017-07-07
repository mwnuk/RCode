
# TIME SERIES MODELING
# ARIMA and Dynlm ( Dynamic Linear Model) 


##############################################################################3
#    ARIMA 
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
#   http://rpubs.com/ajaydecis/ts
###########################################################################
	library(Rcmdr)
	library(RcmdrPlugin.epack)
	
	library(quadprog)
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

#######
# xreg defines which data object contains the observations of 
# the predictors. E.g., if xreg is a matrix of predictors:
#
#  model = Arima(series, order = c(1,1,0), xreg = covariates)
#######
#
#  To find the order of the ARIMA process, you can simply use 
#  the auto.arima function also found in the forecast package. 
#  It automatically locates the best-fitting ARIMA model to the data, 
#  “fit” defined by one of three possible information criteria in 
# the ic argument: the AIC (given by aic), the AICc (aicc), or the BIC (bic).
# E.g.:  model = auto.arima(series, ic = “aic”)
#######



#################################
##
## Dynamic Linear Models ##
##
##  https://cran.r-project.org/web/views/Econometrics.html
## 
#################################
## 
## multiplicative SARIMA(1,0,0)(1,0,0)_12 model fitted
## to UK seatbelt data
      library("dynlm")
	data("UKDriverDeaths", package = "datasets")
	uk <- log10(UKDriverDeaths)
	dfm <- dynlm(uk ~ L(uk, 1) + L(uk, 12))
	dfm
4 dynlm
## explicitly set start and end
	dfm <- dynlm(uk ~ L(uk, 1) + L(uk, 12), start = c(1975, 1), end = c(1982, 12))
	dfm
## remove lag 12
	dfm0 <- update(dfm, . ~ . - L(uk, 12))
	anova(dfm0, dfm)
## add season term
	dfm1 <- dynlm(uk ~ 1, start = c(1975, 1), end = c(1982, 12))
	dfm2 <- dynlm(uk ~ season(uk), start = c(1975, 1), end = c(1982, 12))
	anova(dfm1, dfm2)
	plot(uk)
	lines(fitted(dfm0), col = 2)
	lines(fitted(dfm2), col = 4)
## regression on multiple lags in a single L() call
	dfm3 <- dynlm(uk ~ L(uk, c(1, 11, 12)), start = c(1975, 1), end = c(1982, 12))
	anova(dfm, dfm3)
## Examples 7.11/7.12 from Greene (1993)
	data("USDistLag", package = "lmtest")
	dfm1 <- dynlm(consumption ~ gnp + L(consumption), data = USDistLag)
	dfm2 <- dynlm(consumption ~ gnp + L(gnp), data = USDistLag)
	plot(USDistLag[, "consumption"])
	lines(fitted(dfm1), col = 2)
	lines(fitted(dfm2), col = 4)
	if(require("lmtest")) encomptest(dfm1, dfm2)
###############################
## Time Series Decomposition ##
###############################
## airline data
	data("AirPassengers", package = "datasets")
	ap <- log(AirPassengers)
	ap_fm <- dynlm(ap ~ trend(ap) + season(ap))
	summary(ap_fm)
## Alternative time trend specifications:
## time(ap) 1949 + (0, 1, ..., 143)/12
## trend(ap) (1, 2, ..., 144)/12
## trend(ap, scale = FALSE) (1, 2, ..., 144)
## Exhibit 3.5/3.6 from Cryer & Chan (2008)
	if(require("TSA")) {
	data("tempdub", package = "TSA")
	td_lm <- dynlm(tempdub ~ harmon(tempdub))
	summary(td_lm)
	plot(tempdub, type = "p")
	lines(fitted(td_lm), col = 2)
M1Germany


