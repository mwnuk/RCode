#################################################################
## Read from Drop Box
#################################################################
library( data.table)

#train<-fread("https://www.dropbox.com/s/3bk6sjw0xkfl5xr/AAPL.csv?raw=1") #AAPL
#train<-fread("https://www.dropbox.com/s/1pw6w0r8k4mth04/BA1.csv?raw=1")  #BA
#train<-fread("https://www.dropbox.com/s/oskhoty4jie6dft/C1.csv?raw=1")   #C
#train<-fread("https://www.dropbox.com/s/amnhzzm4k66f766/MSFT.csv?raw=1") #MSFT



#################################################################
## Read from local drive
#################################################################
#library( data.table)
train<-fread("C:\\try\\R\\Quotes\\ba.csv",header=TRUE)
train<-train[order(date)] 

dji<-fread("C:\\try\\R\\Quotes\\dji.csv",header=TRUE)
dji<-dji[order(Date)] 
#train<-train[168:170]
#349 350 351 
x<-seq(1, to=length(train$close))

which( is.na(train$tgt))
which(train$tgt>1)
train<-transform( train,close=as.numeric(close)
                       ,open=as.numeric(open)
                       ,high=as.numeric(high)
                       ,low=as.numeric(low)
                       ,gain= ifelse((as.numeric(close)-as.numeric(open))>0,(as.numeric(close)-as.numeric(open)),0)
                       ,loss= ifelse((as.numeric(close)-as.numeric(open))<0,(as.numeric(open)-as.numeric(close)),0)
                       ,volume=as.numeric(volume)/1000
				)  #,na.rm = FALSE )

summary(train) 
####################################################
## MOVING AVERAGE
####################################################
MovAvgFunct <- function ( data,window){
	window<- window -1 
	total<-length(data) 
	spots<-seq(from=1,to=total)
	result <-vector(length=length(data))
	for( i in total:window){
		result[i]<-mean(data[ (spots[i]-window):spots[i] ] )
	}
	return (result)
}
####################################################
## K value 
####################################################
Kval <- function ( data,window){
	window<- window -1 
	total<-length(data$close) 
	spots<-seq(from=1,to=total)
	result <-vector(length=length(data))
	for( i in total:window ){
		lo5  <- min( data$low[ (spots[i]-window): spots[i] ] )
		hi5 <- max( data$high[ (spots[i]-window): spots[i] ] )
		result[i]<-  (data$close[i] - lo5)/( hi5-lo5)
	}
	return (result)
}
####################################################
## D VALUE
####################################################
D20 <- function ( data){
	total<-length(data) 
	#spots<-seq(from=1,to=total)
	result <-vector(length=length(data))
	for( i in 4:total-1 ){

	   if ( data[ i-1 ] < 0.18  & data[ i+1 ] > 0.21 )
	 	result[i]<-  1
	   else
		result[i]<-  0
	}
	return (result)
}
############################################################
## so D20 happened so what was the raise in next 5 samples
##   this is not used for model, it is a different approach
############################################################
D20Raise <- function ( data,window){
	window<- window -1 
	total<-length(data$close) 
	result <-vector(length=length(data$close))
	for( i in 1:(total-window))
	{
      		if( data$D20[ i ] > 0 ) 
		{
			fst <- data$close[ i ]
			mx  <- max( data$close[ i : ( i+window) ] )
        	 	result[i] = (100*(mx-fst))/fst
		}
       }
	return (result)
}
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
## Moving window looking for 7% increase in 5 days
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
## All values to train data frame as extra columns
####################################################
#library(plyr)
#train<-ddply(close,.(train),'nrow')

train <- cbind( train,avg30=MovAvgFunct( train$close, 30 ))
train <- cbind( train,avg100=MovAvgFunct( train$close, 100 ))
train <- cbind( train,tgt=targetFunct( train$close, 5 ))
train <- cbind( train,Kval=Kval( train, 5 ))
train <- cbind( train,Dval=MovAvgFunct( train$Kval, 3 ))
train <- cbind( train,D20=D20( train$Dval ) )
train <- cbind( train,D20Raise=D20Raise( train,5) )
train <- cbind( train,RSI=RSI( train, 14 ))
#names(train)
####################################################
## PAIRS
####################################################

library("psych") # pairs panels
train[,"date"]<-NULL 
train[,"gain"]<-NULL 
train[,"loss"]<-NULL 

summary(train)
pairs.panels(train)

####################################################
## MODELS
####################################################val

#glm.fit <- glm(tgt~close+open+high+low+high*low+avg30+avg100+Dval+RSI,data=train,family=binomial)
#glm.fit <- glm(tgt~close+open+high+low+gain,data=train,family=binomial)
#glm.fit <- glm(tgt~close+I(close^2)+ I(close^3)+avg30+avg100+D20,data=train,family=binomial)
#glm.fit <- glm(tgt~close+I(close^2)+ I(close^3)+I(close^4)+I(close^5)+avg30+avg100+D20,data=train,family=binomial)

polymodel <- lm(train$close ~ poly(x,25))
predicted.intervals <- predict(polymodel,data.frame(x=x),interval='confidence',level=0.95)



#glm.fit <- glm(tgt~close+open+high+low+avg30+avg100+D20 +predicted.intervals[,1],data=train,family=binomial)
#glm.fit <- glm(tgt~. + high*low,data=train,family=binomial)

###### STEP REGRESSION
model <- glm(tgt~close+open+high+low+high*low+avg30+avg100+Dval+RSI,data=train,family=binomial)
glm.fit<-step(model, direction='both', criterion='BIC')


####################################################
## PREDICT GLM
####################################################
summary(glm.fit)

glm.probes = predict( glm.fit , type="response")




#which( glm.probes >0.2)
start <- 1
stop <- length(train$close) 
#start <- 110
#stop <- 150 



####################################################
## PLOT 
####################################################
#fix scale to show points on a graph
train$tgt  = ifelse( train$tgt  >0,   min(train$close[start:stop])+5, 0 )
glm.probes = ifelse( glm.probes >0.5, min(train$close[start:stop])+3, 0 )
train$D20 = ifelse( train$D20 >0.5,   min(train$close[start:stop]),   0 )

#cbind( train[320:360, c("close","tgt","Dval","D20")] , glm.probes[320:360])


plot( train$close[start:stop],pch=20 )
lines(x,predicted.intervals[,1],col='green',lwd=3) #fit
lines(x,predicted.intervals[,2],col='black',lwd=1) #lower confidence
lines(x,predicted.intervals[,3],col='black',lwd=1) #upper confidence

points(train$avg30[start:stop],col="red" )
points(train$avg100[start:stop],col="blue" )
points(train$tgt[start:stop],col="green",pch=20 )
points(glm.probes[start:stop],col="red",pch=20 )
points(train$D20[start:stop],col="magenta",pch=20 )

####################################################
## NUMERIC RESULTS
####################################################

#length(train$tgt)
#length(which( train$tgt >1) )
#length(which( train$D20 >1) )

#f1<-which( train$D20 >1)%in% which( train$tgt >1)
#which( f1 == TRUE)
which(glm.probes >0)
#which( train$D20 >0 )
#which( train$D20Raise >0 )

#train$D20Raise[which( train$D20Raise >0 )]
#mean(train$D20Raise[which( train$D20Raise >0 )] )

#train[200:255, c("close","gain","loss","D20Raise")]

tail(train)
train[505]

summary(glm.fit)





