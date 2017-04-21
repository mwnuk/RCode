http://stats.stackexchange.com/questions/72251/an-example-lasso-regression-using-glmnet-for-binary-outcome

## How to use GLMNET
## Usage: For large amount of corelated independent variables 
# http://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html

# Note glmnet with alpha=1 alpha=1 for Lasso only 
       glmnet with alpha=1 alpha=0 for Ridge only 
       glmnet with alpha between 0,1 ElasticNet
# Liniar regression is have small error from Bias and large from a Variance
# This method will increase Bias but minimize the Variance
# The goal is to find optimal lambda which is a weight between Bias and Varaince
# When shrinkage parameter lambda is zero it becomes regular liniar regression
###############################################################
## LASSO FOR BINARY OUTPUT
###############################################################
	install.packages('glmnet')
	library(glmnet)
	age     <- c(4, 8, 7, 12, 6, 9, 10, 14, 7) 
	gender  <- as.factor(c(1, 0, 1, 1, 1, 0, 1, 0, 0))
	bmi_p   <- c(0.86, 0.45, 0.99, 0.84, 0.85, 0.67, 0.91, 0.29, 0.88) 
	m_edu   <- as.factor(c(0, 1, 1, 2, 2, 3, 2, 0, 1))
	p_edu   <- as.factor(c(0, 2, 2, 2, 2, 3, 2, 0, 0))
	f_color <- as.factor(c("blue", "blue", "yellow", "red", "red", "yellow","yellow", "red", "yellow")) 
	asthma <- c(1, 1, 0, 1, 0, 0, 0, 1, 1)

# age    - (age of child in years)       - continuous
# gender -                               - binary (1 = male; 0 = female)
# bmi_p (BMI percentile)                 - continuous
# m_edu (mother highest education level) - ordinal (0 = less than high school; 1 = high school diploma; 2 = bachelors degree; 3 = post-baccalaureate degree)
# p_edu (father highest education level) - ordinal (same as m_edu)
# f_color (favorite primary color)       - nominal ("blue", "red", or "yellow")
# asthma (child asthma status)           - binary (1 = asthma; 0 = no asthma)

                       


	xfactors <- model.matrix(asthma ~ gender + m_edu + p_edu + f_color)[, -1]
	x        <- as.matrix(data.frame(age, bmi_p, xfactors))

# Note alpha=1 for lasso only and can blend with ridge penalty down to
# alpha=0 ridge only.
	glmmod <- glmnet(x, y=as.factor(asthma), alpha=1, family="binomial")

# Plot variable coefficients vs. shrinkage parameter lambda.
	plot(glmmod, xvar="lambda")
	dput(x)

	coef(glmmod)[, 10]

#We do this by the crossvalidation function of glmnet. Crossvalidation is a predictive criterion 
#that evaluates the sample performance #by splitting the sample into training and validation sets 
#and choosing the value of lambda with which the error of prediction is minimal.

	cv.glmmod <- cv.glmnet(x, y=asthma, alpha=1)
	plot(cv.glmmod)

	best.lambda <- cv.glmmod$lambda.min
#######################################################
dput( x,foo) # write R object to a text file
bar <- dget("foo") # read text file into an R object


###############################################################
## LASSO FOR CONTINUOUS OUTPUT
###############################################################

    set.seed(1)
    x1 <- rnorm(30)
    x2 <- rnorm(30)
    x3 <- rnorm(30)
    X <- matrix( c(x1, x2, x3), byrow = F, ncol = 3)

#Then we need a response as well. This needs to be a vector as you know so let's form a linear combination of the predictors and corrupt it with some noise.
y <- 3 + 4*x1 + 3*x2 + 5*x3 + rnorm(30)

    fit <-glmnet(x = X, y = y, alpha = 1) 
# different values of alpha return different estimators, alpha = 1 is the lasso.
    plot(fit, xvar = "lambda")

# is the penalty or the Lagrange multiplier if you prefer and is always positive, but log makes it negative in <0,1>

	coef(fit, s = 0.3) # s is the value of lambda

#We do this by the crossvalidation function of glmnet again

	crossval <-  cv.glmnet(x = X, y = y)
	plot(crossval)
	penalty <- crossval$lambda.min #optimal lambda
	penalty #minimal shrinkage
	fit1 <-glmnet(x = X, y = y, alpha = 1, lambda = penalty ) #estimate the model with that
	coef(fit1)







