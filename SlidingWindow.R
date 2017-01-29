########################################################################
## Sliding Window
########################################################################
val<-seq(from=1, to=50, by=1)
train <- data.frame(val)


MovAvgFunct <- function ( data,window){
	window<- window-1  
	total<-length(data) 
	spots<-seq(from=1,to=total)
	result <-vector(length=length(data))
	#Sliding backwards
	for( i in total:(window+1)){
		result[i]<-mean(data[ (spots[i]-window):spots[i] ] )
	}
	return (result)
}

train <- cbind( train,avg5=MovAvgFunct( train$val, 5 ))

train
