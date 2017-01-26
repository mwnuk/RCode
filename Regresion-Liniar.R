http://www.r-tutor.com/elementary-statistics/simple-linear-regression/prediction-interval-linear-regression
?faithful
summary(faithful)
faithful
plot(faithful[, -3], main = f.tit,
     xlab = "Eruption time (min)",
     ylab = "Waiting time to next eruption (min)")
lines(lowess(faithful$eruptions, faithful$waiting, f = 2/3, iter = 3),
      col = "red")
)
#  help (lowess)


#SIMPLE LINIAR MODEL
#--------------------------------------------
attach(faithful)     # attach the data frame 
eruption.lm = lm(eruptions ~ waiting) 

newdata = data.frame(waiting=80) 

predict(eruption.lm, newdata, interval="predict") 

#The 95% prediction interval of the eruption duration for the waiting time of 
#80 minutes is between 3.1961 and 5.1564 minutes. 


#help(predict.lm) 

predict(eruption.lm, newdata, interval="predict",level=0.6) 

predict(eruption.lm,  interval="predict",level=0.6) 

###########################################################################
x <- rnorm(15)
y <- x + rnorm(15)
predict(lm(y ~ x))
new <- data.frame(x = seq(-3, 3, 0.5))
predict(lm(y ~ x), new, se.fit = TRUE)
pred.w.plim <- predict(lm(y ~ x), new, interval = "prediction")
pred.w.clim <- predict(lm(y ~ x), new, interval = "confidence")
matplot(new$x, cbind(pred.w.clim, pred.w.plim[,-1]),
        lty = c(1,2,2,3,3), type = "l", ylab = "predicted y")


###########################################################################
cbind - adding columns
help (cbind)
m<-cbind(1, 1:7)
cbind(m, 8:14)

rbind - adding rows 
 x = rbind(c(10, "[]", "[[1,2]]"), c(20, "[]", "[[1,3]]"))
 x
     [,1] [,2] [,3]     
[1,] "10" "[]" "[[1,2]]"
[2,] "20" "[]" "[[1,3]]"






