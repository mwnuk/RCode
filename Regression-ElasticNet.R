http://stats.stackexchange.com/questions/72251/an-example-lasso-regression-using-glmnet-for-binary-outcome
install.packages( "c:\\RPAckages\\elasticnet_1.1.zip")
Two ways: glmnet or elasticnet
################################################################
install.packages('elasticnet')
library(elasticnet)

age <- c(4,8,7,12,6,9,10,14,7) 
gender <- c(1,0,1,1,1,0,1,0,0)
bmi_p <- c(0.86,0.45,0.99,0.84,0.85,0.67,0.91,0.29,0.88)
m_edu <- c(0,1,1,2,2,3,2,0,1)
p_edu <- c(0,2,2,2,2,3,2,0,0)
#f_color <- c("blue", "blue", "yellow", "red", "red", "yellow", "yellow", "red", "yellow")
f_color <- c(0, 0, 1, 2, 2, 1, 1, 2, 1)
asthma <- c(1,1,0,1,0,0,0,1,1)
pred <- cbind(age, gender, bmi_p, m_edu, p_edu, f_color)


help(enet)
object1<-enet(x=pred, y=asthma, lambda=0)
plot(object1)
##################################################################
	data(diabetes)
	attach(diabetes)
##fit the lasso model (treated as a special case of the elastic net)
	object1 <- enet(x,y,lambda=0)
	plot(object1)
##fit the elastic net model with lambda=1.
	object2 <- enet(x,y,lambda=1) 
	plot(object2)
##early stopping after 50 LARS-EN steps
	object4 <- enet(x2,y,lambda=0.5,max.steps=50)
	plot(object4)
	detach(diabetes)
##################################################################
install.packages('glmnet')
library(glmnet)

# Note glmnet with alpha=1 alpha=1 for Lasso only 
       glmnet with alpha=1 alpha=0 for Ridge only 
       glmnet with alpha between 0,1 ElasticNet
# Liniar regression is have small error from Bias and large from a Variance
# This method will increase Bias but minimize the Variance
# The goal is to find optimal lambda which is a weight between Bias and Varaince

