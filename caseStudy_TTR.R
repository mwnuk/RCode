#  PREPARE DATA

#################################################################
## Read from Drop Box
#################################################################
library( data.table)

#ttrc<-fread("https://www.dropbox.com/s/3bk6sjw0xkfl5xr/AAPL.csv?raw=1") #AAPL
#ttrc<-fread("https://www.dropbox.com/s/1pw6w0r8k4mth04/BA1.csv?raw=1")  #BA
#ttrc<-fread("https://www.dropbox.com/s/oskhoty4jie6dft/C1.csv?raw=1")   #C
#ttrc<-fread("https://www.dropbox.com/s/amnhzzm4k66f766/MSFT.csv?raw=1") #MSFT


#library( data.table)
ttrc<-fread("C:\\try\\R\\Quotes\\ba.csv",header=TRUE)
ttrc<-ttrc[order(date)] 


ttrc<-transform( ttrc,close=as.numeric(close)
                       ,open=as.numeric(open)
                       ,high=as.numeric(high)
                       ,low=as.numeric(low)
                       #,gain= ifelse((as.numeric(close)-as.numeric(open))>0,(as.numeric(close)-as.numeric(open)),0)
                       #,loss= ifelse((as.numeric(close)-as.numeric(open))<0,(as.numeric(open)-as.numeric(close)),0)
                       ,volume=as.numeric(volume)/1000
				)  #,na.rm = FALSE )



library(TTR)
#??T?TR
#data(ttrc) 

# Bollinger Bands
	bbands <- BBands( ttrc[,c("high","low","close")] ) #,select =pctB)
      
# Directional Movement Inde
	adx <- ADX(ttrc[,c("high","low","close")])
# Moving Averages
	ema <- EMA(ttrc[,"close"], n=20)
	sma <- SMA(ttrc[,"close"], n=50)
# MACD
	macd <- MACD( ttrc[,"close"] )

# Stochastics
	stochOsc <- stoch(ttrc[,c("high","low","close")])
# RSI
 	rsi <- RSI(ttrc[,"close"],10)

############################
# data(ttrc) 
# price<-ttrc[,"Close"]
# RSI(price,14)
############################
 

#REMOVE DATE
ttrc <- subset( ttrc, select=-date )
which(ttrc$tgt==1)


ttrc <- cbind( ttrc,bbands)
ttrc <- cbind( ttrc,adx=ADX)
ttrc <- cbind( ttrc,ema)
ttrc <- cbind( ttrc,sma)
ttrc <- cbind( ttrc,macd)
ttrc <- cbind( ttrc,stochOsc )

#ttrc <- as.data.frame(lapply(ttrc,Normalize))

#ADD TARGET
ttrc <- cbind( ttrc,tgt=factor(targetFunct( ttrc$close, 6 )))
ttrc <- cbind( ttrc,avgcross=factor(avgCrossFunct(ttrc)))
# bbands have 3 values: dn,mavg,up,pctB


ttrc <- cbind( ttrc,adx1=PrevValue(ttrc$adx))
ttrc <- cbind( ttrc,adx2=PrevValue(ttrc$adx1))
ttrc <- cbind( ttrc,adx3=PrevValue(ttrc$adx2))
ttrc <- cbind( ttrc,adx4=PrevValue(ttrc$adx3))
ttrc <- cbind( ttrc,adx5=PrevValue(ttrc$adx4))




ttrc <- cbind( ttrc,macd1=PrevValue(ttrc$macd))
ttrc <- cbind( ttrc,macd2=PrevValue(ttrc$macd1))
ttrc <- cbind( ttrc,macd3=PrevValue(ttrc$macd2))
ttrc <- cbind( ttrc,macd4=PrevValue(ttrc$macd3))
ttrc <- cbind( ttrc,macd5=PrevValue(ttrc$macd4))
ttrc <- cbind( ttrc,signal1=PrevValue(ttrc$signal))
ttrc <- cbind( ttrc,signal2=PrevValue(ttrc$signal1))
ttrc <- cbind( ttrc,signal3=PrevValue(ttrc$signal2))
ttrc <- cbind( ttrc,signal4=PrevValue(ttrc$signal3))
ttrc <- cbind( ttrc,signal5=PrevValue(ttrc$signal4))

ttrc[200:210,c("adx","adx1","adx2")]


ttrc <- cbind( ttrc,fastK1=PrevValue(ttrc$fastK))
ttrc <- cbind( ttrc,fastK2=PrevValue(ttrc$fastK1))
ttrc <- cbind( ttrc,fastK3=PrevValue(ttrc$fastK2))
ttrc <- cbind( ttrc,fastK4=PrevValue(ttrc$fastK3))
ttrc <- cbind( ttrc,fastK5=PrevValue(ttrc$fastK4))
ttrc <- cbind( ttrc,fastD1=PrevValue(ttrc$fastD))
ttrc <- cbind( ttrc,fastD2=PrevValue(ttrc$fastD1))
ttrc <- cbind( ttrc,fastD3=PrevValue(ttrc$fastD2))
ttrc <- cbind( ttrc,fastD4=PrevValue(ttrc$fastD3))
ttrc <- cbind( ttrc,fastD5=PrevValue(ttrc$fastD4))
ttrc <- cbind( ttrc,slowD1=PrevValue(ttrc$slowD))
ttrc <- cbind( ttrc,slowD2=PrevValue(ttrc$slowD1))
ttrc <- cbind( ttrc,slowD3=PrevValue(ttrc$slowD2))
ttrc <- cbind( ttrc,slowD4=PrevValue(ttrc$slowD3))
ttrc <- cbind( ttrc,slowD5=PrevValue(ttrc$slowD4))


####################################################
## TARGET Moving window looking for 7% increase in 5 days
####################################################
targetFunct <- function ( data,window){
	window<- window -1 
	total<-length(data) 
	spots<-seq(from=1,to=(total-window) )
	result <-vector(length=length(data))
	for( i in 1:length(spots)){
		fst <- data[ spots[i] ]
		mx  <- max( data[ spots[i] : (spots[i]+window) ] )
		mn  <- min( data[ spots[i] : (spots[i]+window) ] )

      	#if( data[ spots[i] ] < data[ (spots[i]+window) ] )
		{
        	  	if( (mx-fst)/fst >0.05) # its 5% now and it makes a big AIC
              	result[i] =1
         	  else
              	result[i] =0 
		}
		#else
			#result[i] =0
       }
	return (result)
}


####################################################
## Moving averages crossing - sma, ema
####################################################
avgCrossFunct <- function ( data){

	total<-length(data$sma) 
	spots<-seq(from=1,to=(total) )
	result <-vector(length=total)
	for( i in 51:length(spots)){
	      smadown   <- data$sma[ spots[i] ] - data$sma[ spots[i-1] ]
            emaup   <- data$ema[ spots[i] ] - data$ema[ spots[i-1] ]

        	if( (smadown<0) & (emaup>0) & (data$sma[i]>data$ema[i])  )
			result[i] =1 
         	else
              	result[i] =0 
       }
	return (result)
}


####################################################
## Function: Normalize
####################################################
Normalize1 <- function(x) { 
   x <- sweep(x, 2, apply(x, 2, min)) 
   sweep(x, 2, apply(x, 2, max), "/") 
} 

Normalize <- function(x) {
(x - min(x, na.rm=TRUE))/(max(x,na.rm=TRUE) - 
min(x, na.rm=TRUE))
} 
####################################################
## Moving window returning prevoius value
####################################################
PrevValue <- function ( data){
	total<-length(data)
	result <-vector(length=total)
	for( i in total:2){
		result[i] <- data[ i-1 ]
       }
	return (result)
}
x<-seq(1 :10)
#PrevValue(ttrc$close)

####################################################
## PAIRS
####################################################

library("psych") # pairs panels


summary(ttrc)
#pairs.panels(ttrc)

####################################################
## MODELS
####################################################val
#
glm.fit <- glm(tgt~.,data=ttrc,family=binomial)
conflicts()

polymodel <- lm(ttrc$close ~ poly(x,25))
predicted.intervals <- predict(polymodel,data.frame(x=x),interval='confidence',level=0.95)

###### STEP REGRESSION
#model <- glm(tgt~.,data=ttrc,family=binomial)
#glm.fit<-step(model, direction='both', criterion='BIC')


####################################################
## PREDICT GLM
####################################################
summary(glm.fit)

glm.probes = predict( glm.fit , type="response" )


#which( glm.probes >0.5)
start <- 1
stop <- length(ttrc$close) 
#start <- 110
#stop <- 150 

#which( ttrc$tgt >0.5)

####################################################
## PLOT 
####################################################
#fix scale to show points on a graph
ttrc$tgt  = ifelse( ttrc$tgt  ==1,   min(ttrc$close[start:stop])+5, 0 )
glm.probes = ifelse( glm.probes >0.5, min(ttrc$close[start:stop])+3, 0 )
ttrc$avgcross = ifelse( ttrc$avgcross ==1,   min(ttrc$close[start:stop])+2,   0 )

#cbind( ttrc[320:360], c("close","tgt","Dval","D20")] , glm.probes[320:360])


plot( ttrc$close[start:stop],pch=20 )
lines(x,predicted.intervals[,1],col='green',lwd=3) #fit
lines(x,predicted.intervals[,2],col='black',lwd=1) #lower confidence
lines(x,predicted.intervals[,3],col='black',lwd=1) #upper confidence

points(ttrc$ema[start:stop],col="red" )
points(ttrc$sma[start:stop],col="blue" )
points(ttrc$tgt[start:stop],col="green",pch=20 )
points(glm.probes[start:stop],col="red",pch=20 )
points(ttrc$avgcross[start:stop],col="magenta",pch=20 )


#fwrite(ttrc, "C:\\try\\R\\Quotes\\ba1.csv")


