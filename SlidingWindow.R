http://www.cookbook-r.com/Manipulating_data/Calculating_a_moving_average/

########################################################################
## Sliding Window
########################################################################

	set.seed(993)
	x <- 1:300
	y <- sin(x/20) + rnorm(300,sd=.1)
	y[251:255] <- NA

# Plot the unsmoothed data (gray)
	plot(x, y, type="l", col=grey(.5))
# Draw gridlines
	grid()

# Smoothed with lag:
# average of current sample and 19 previous samples (red)
	f20 <- rep(1/20, 20)
	y_lag <- filter(y, f20, sides=1)
	lines(x, y_lag, col="red")

# Smoothed symmetrically:
# average of current sample, 10 future samples, and 10 past samples (blue)
f21 <- rep(1/21,21)
	y_sym <- filter(y, f21, sides=2)
	lines(x, y_sym, col="blue")


########################################################################
# Superfast sliding winows 

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

# Window size is 5, current value and 4 most recent ones
	train <- cbind( train,avg5=MovAvgFunct( train$val, 5 ))
	train





