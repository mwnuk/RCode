http://stats.stackexchange.com/questions/214682/stepwise-regression-in-r-how-does-it-work

step(lm(mpg~wt+drat+disp+qsec,data=mtcars),direction="both")
step(lm(mpg~wt+drat+disp+qsec,data=mtcars),direction="backward")


#install.packages('C:/RPAckages/leaps_3.0.zip', lib='C:/RPackages/mypackages',repos = NULL)
#looking at all 15 possible lm models. 


	library(leaps)
	tmp<-regsubsets(mpg ~ wt + drat + disp + qsec, data=mtcars, nbest=1000, really.big=T, intercept=F)
	all.mods <- summary(tmp)[[1]]
	#all.mods <- lapply(1:nrow(all.mods, function(x)as.formula( paste("mpg~", paste(names(which(all.mods[x,])), collapse="+")))

	head(all.mods)
help(regsubsets)

	all.lm<-lapply(all.mods, lm, mtcars)
#AIC values for each of the model are extracted with:
	sapply(all.lm, extractAIC)[2,]


#Step forward only:
	library(ISLR)
	regfit.fwd=regsubsets(Salary~.,data=Hitters,nvmax=19,method="forward")
# you can see that each new step contains all previous components plus one more       
      summary( regfit.fwd)
      plot(regfit.fwd,scale="Cp")
########################################################################
## STEP ON LOGISTIC REGRESION
########################################################################
# VS - V or straight engine
	
	model <- glm(formula= vs ~ ., data=mtcars, family=binomial)
	summary(model)

	model <-step(model, direction='both', criterion='BIC')
	model
	model1= stepAIC(model,direction='both')
      

#only qsec is relevant - lowest AIC

#What is the probability that this engine is S -Straight
	newdata = data.frame(wt = 2.1, qsec = 17,wt=3500 )
	predict(model, newdata, type="response")

# more qsec, more probability of VS
	newdata = data.frame(wt = 2.1, qsec = 17)
	predict(model, newdata, type="response")

	newdata = data.frame(wt = 2.1, qsec = 17.1)
	predict(model, newdata, type="response")

########################################################################






