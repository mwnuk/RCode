http://stockcharts.com/school/doku.php?id=chart_school:technical_indicators:relative_strength_index_rsi

library( data.table)
train<-fread("C:\\try\\R\\Quotes\\ba1.csv")

#train<-train[0:722,]
x<-seq(1, to=length(train$close))

which( is.na(train$volume))

train<-transform( train,close=as.numeric(close)
                       ,open=as.numeric(open)
                       ,high=as.numeric(high)
                       ,low=as.numeric(low)
                       ,gain= ifelse((as.numeric(close)-as.numeric(open))>0,(as.numeric(close)-as.numeric(open)),0)
                       ,loss= ifelse((as.numeric(close)-as.numeric(open))<0,(as.numeric(open)-as.numeric(close)),0)
                       ,na.rm = FALSE )

summary(train) 
#help(transform)

####################################################
## Relative Strength Index
####################################################
RSI <- function ( data,window ){
	window<- window -1 
	total<-length(data$close) 
	result <-vector(length=length(data$close))
	for( i in window:total )
	{
		gn  <- sum( data$gain[ (i-window) : i ] )
      	ls  <- sum( data$loss[ (i-window) : i ] )
            RS= gn/ls
		result[i]=as.integer(100 - (100/(1+RS)) )
      }
	return (result)
}



####################################################
## All all values to train data frame as extra columns
####################################################

train <- cbind( train,RSI=RSI( train, 14 ))
train[743:757, c("close","open","gain","loss","RSI")]

