#http://www.theanalysisfactor.com/r-glm-model-fit/
#The glm() command is designed to perform on binary outcome data
#GLM model are an extension of linear regression models that allow the dependent 
#variable to be non-normal.


	pairs(mtcars, main = "mtcars data")
	coplot(mpg ~ disp | as.factor(cyl), data = mtcars,panel = panel.smooth, rows = 1)

	?mtcars

#Multiple Regression
	model <- glm(formula= vs ~ wt + disp , data=mtcars, family=binomial)
	summary(model)
# can see that more weight, more vs
#              more disp less VS ( V or straight) 

#What is the probability that this engine is S -Straight
	newdata = data.frame(wt = 2.1, disp = 180)
	predict(model, newdata, type="response")

# more weight, more probability of VS
	newdata = data.frame(wt = 3.1, disp = 180)
	predict(model, newdata, type="response")

# more disp, less probability of VS
	newdata = data.frame(wt = 3.1, disp = 280)
	predict(model, newdata, type="response")

-----------
#PLOT
	model_weight<- glm(formula= vs ~ wt, data=mtcars, family=binomial)
	yweight <- predict(model_weight, list(wt = xweight),type="response")
	xweight <- seq(0, 6, 0.01)
	plot(mtcars$wt, mtcars$vs, pch = 16, xlab = "WEIGHT (g)", ylab = "VS")
	lines(xweight, yweight)


