http://stats.stackexchange.com/questions/72251/an-example-lasso-regression-using-glmnet-for-binary-outcome

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



enet(x=pred, y=asthma, lambda=0)

