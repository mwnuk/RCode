#https://www.quantinsti.com/blog/forecasting-stock-returns-using-arima-model/


library(quantmod); #getSymbols
library(tseries);
library(timeSeries);
library(forecast);
library(xts);      #time series object


# Pull data from Yahoo finance 
#getSymbols('MSFT', from='2017-01-01', to='2017-07-01')
getSymbols('MSFT', from='2017-01-01')

# Select the relevant close price series
stock_prices = MSFT[,4]

# Compute the log returns for the stock
stock = diff(log(stock_prices),lag=1)
stock = stock[!is.na(stock)]

# Plot log returns 
plot(stock,type='l', main='log returns plot')

# Conduct ADF test on log returns series
print(adf.test(stock))


# Split the dataset in two parts - training and testing
breakpoint = floor(nrow(stock)*(2.9/3))


# Apply the ACF and PACF functions
par(mfrow = c(1,1))
acf.stock = acf(stock[c(1:breakpoint),], main='ACF Plot', lag.max=100)
pacf.stock = pacf(stock[c(1:breakpoint),], main='PACF Plot', lag.max=100)

(p,d,q) 
#c(2, 0, 2)


# Initialzing an xts object for Actual log returns
Actual_series = xts(0,as.Date("2015-01-01","%Y-%m-%d"))
 
# Initialzing a dataframe for the forecasted return series
forecasted_series = data.frame(Forecasted = numeric()

#### test #######################

	fit=arima(stock, order = c(, 0, 2),include.mean=FALSE)
	plot(forecast(fit,h=5))      
	accuracy(fit)

      #ver  1
      fit2=auto.arima(stock)
	plot(forecast(fit2,h=3))
      #ver2
	fit3=auto.arima(stock,stepwise=FALSE,approx=FALSE)
	plot(forecast(fit3,h=3))

	arima.forecast = forecast(fit, h = 3,level=99)
      
	# forecast and predict are the same 
	pred <- predict(fit, n.ahead = 7)

	ts.plot(stock,2.718^pred$pred, log = "y", lty = c(1,3))

#### end test ###################

help(forecast.Arima)



for (b in breakpoint:(nrow(stock)-1)) {

	stock_train = stock[1:b, ]
	stock_test = stock[(b+1):nrow(stock), ]
      print(1)
	# Summary of the ARIMA model using the determined (p,d,q) parameters
	fit = arima(stock_train, order = c(0, 0, 1),include.mean=FALSE)
	summary(fit)
      print(2)
	# plotting a acf plot of the residuals
	acf(fit$residuals,main="Residuals plot")

	# Forecasting the log returns
	#####arima.forecast = forecast.Arima(fit, h = 1,level=99)
	arima.forecast = forecast(fit, h = 1,level=99)
	summary(arima.forecast)

	# plotting the forecast
	par(mfrow=c(1,1))
	plot(arima.forecast, main = "ARIMA Forecast")

	# Creating a series of forecasted returns for the forecasted period
	forecasted_series = rbind(forecasted_series,arima.forecast$mean[1])
	colnames(forecasted_series) = c("Forecasted")

	# Creating a series of actual returns for the forecasted period
	Actual_return = stock[(b+1),]
	Actual_series = c(Actual_series,xts(Actual_return))
	rm(Actual_return)

	print(stock_prices[(b+1),])
	print(stock_prices[(b+2),])

}



# VALIDATION 
# Adjust the length of the Actual return series
	Actual_series = Actual_series[-1]

# Create a time series object of the forecasted series
	forecasted_series = xts(forecasted_series,index(Actual_series))

# Create a plot of the two return series - Actual versus Forecasted
	plot(Actual_series,type='l',main='Actual Returns Vs Forecasted Returns')
	lines(forecasted_series,lwd=1.5,col='red')
	legend('bottomright',c("Actual","Forecasted"),lty=c(1,1),lwd=c(1.5,1.5),col=c('black','red'))

# Create a table for the accuracy of the forecast
	comparsion = merge(Actual_series,forecasted_series)
	comparsion$Accuracy = sign(comparsion$Actual_series)==sign(comparsion$Forecasted)
	print(comparsion)

# Compute the accuracy percentage metric
	Accuracy_percentage = sum(comparsion$Accuracy == 1)*100/length(comparsion$Accuracy)
	print(Accuracy_percentage)



###############3
fit <- Arima(WWWusage,c(3,1,0))
plot(forecast(fit))

library(fracdiff)
x <- fracdiff.sim( 100, ma=-.4, d=.3)$series
fit <- arfima(x)
plot(forecast(fit,h=30))
#######################################################################################
#  ARIMA WITH MULTIPLE PARAMETERS xreg
#
# https://stats.stackexchange.com/questions/41070/how-to-setup-xreg-argument-in-auto-arima-in-r
# 
#
#  Customers in a shopping mall 
#######################################################################################

library(forecast)
# create some artifical data
modelfitsample <- data.frame(Customer_Visit=rpois(49,3000),Weekday=rep(1:7,7),
                             Christmas=c(rep(0,40),1,rep(0,8)),Day=1:49)

# Create matrix of numeric predictors - convert Weekday to 7 factors
xreg <- cbind(Weekday=model.matrix(~as.factor(modelfitsample$Weekday)), 
                  Day=modelfitsample$Day,
              Christmas=modelfitsample$Christmas)

# Remove intercept
xreg <- xreg[,-1]

# Rename columns
colnames(xreg) <- c("Mon","Tue","Wed","Thu","Fri","Sat","Day","Christmas")

head(xreg)

# Variable to be modelled - without frequency it will model all 50 records
visits <- ts(modelfitsample$Customer_Visit), frequency=7))

# Find ARIMAX model
modArima <- auto.arima(visits, xreg=xreg)
plot(forecast(modArima,xreg=xreg))

# without other predictors visits=3000 flat
modArima2 <- auto.arima(visits)
plot(forecast(modArima2))



#######################################################################################
#
#
#
#
#
#######################################################################################

getSymbols('MSFT', from='2017-01-01')

# Select the relevant close price series
stock_prices_open  = MSFT[,1]
stock_prices_hi    = MSFT[,2]
stock_prices_lo    = MSFT[,3]
stock_prices_close = MSFT[,4]
stock_prices_vol   = MSFT[,5]


# Compute the log returns for the stock
stock_open = diff(log(stock_prices_open),lag=1)
stock_hi = diff(log(stock_prices_hi),lag=1)
stock_lo = diff(log(stock_prices_lo),lag=1)
stock_close = diff(log(stock_prices_close),lag=1)
stock_vol = diff(log(stock_prices_vol),lag=1)

stock_open  = stock_open[!is.na(stock)]
stock_hi    = stock_hi[!is.na(stock)]
stock_lo    = stock_lo[!is.na(stock)]
stock_close = stock_close[!is.na(stock)]
stock_vol   = stock_vol[!is.na(stock)]

# Plot log returns 
plot(stock,type='l', main='log returns plot')


# Create matrix of numeric predictors
xreg <- cbind(stock_open,stock_hi,stock_lo,stock_vol )


# Variable can be modeled without frequency
stock_close <- ts(stock_close, frequency=7)

fit3=auto.arima(stock_close,xreg=xreg)
plot(forecast(fit3,xreg=xreg))
points(1:length(stock_close),fitted(fit3),type="l",col="green")


fit3=auto.arima(stock_close,xreg=xreg,D=7,max.P = 5, max.Q = 5)
plot(forecast(fit3,xreg=xreg))
points(1:length(stock_close),fitted(fit3),type="l",col="green")
















