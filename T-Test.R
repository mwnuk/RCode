# p-value is the probability for a given statistical model that, when the null hypothesis is true,

       P > 0.10 No evidence against the null hypothesis. 
0.05 < P < 0.10 Weak evidence against the null hypothesis in favor of the alternative.
0.01 < P < 0.05 Moderate evidence against the null hypothesis in favor of the alternative.
0.001 <P < 0.01 Strong evidence against the null hypothesis in favor of the alternative.
       P < 0.001 Very strong evidence against the null hypothesis in favor of the alternative.

t-value measures the size of the difference relative to the variation in your sample data-signal to noice ratio. 
  Put another way, T is simply the calculated difference represented in units of standard error. 
  The greater the magnitude of T (it can be either positive or negative), the greater the evidence 
  against the null hypothesis that there is no significant difference. The closer T is to 0, 
  the more likely there isn't a significant difference.
t-distribution is a bell curve, with area under curve equal to probability p-value, function pt in R


http://blog.minitab.com/blog/adventures-in-statistics-2/understanding-t-tests:-1-sample,-2-sample,-and-paired-t-tests

# independent 2-group t-test
	t.test(y~x) # where y is numeric and x is a binary factor 
# independent 2-group t-test
	t.test(y1,y2) # where y1 and y2 are numeric 
# paired t-test
	t.test(y1,y2,paired=TRUE) # where y1 & y2 are numeric 
# one sample t-test
	t.test(y,mu=3) # Ho: mu=3 

#######################################################################
# EXAMPLE  one sample t-test
# Raw dataset -- 32 observations
data = c(21.75, 18.0875, 18.75, 23.5, 14.125, 16.75, 11.125, 11.125, 14.875, 15.5, 20.875,
            17.125, 19.075, 25.125, 27.75, 29.825, 17.825, 28.375, 22.625, 28.75, 27, 12.825, 
            26, 32.825, 25.375, 24.825, 25.825, 15.625, 26.825, 24.625, 26.625, 19.625)
plot(data)
mean(data)

# NULL Hypotesis: Mean Value is not equal to 20

# Student t-Test
	t.test(x=data, mu=20, conf.level=0.95)

# Manually calculate p-value

	t.value = (mean(data) - 20) / (sd(data) / sqrt(length(data))) 
	p.value = 2*pt(-abs(t.value), df=length(data)-1)
	p.value
	?pt


#######################################################################
# ANOVA - Analysis of variance can determine whether the means of three 
#         or more groups are different. - 
# F-statistic is simply a ratio of two variances, can estimate signifficance of 
#   a regression model, or test equality of means
#
#See more at: http://blog.minitab.com/blog/adventures-in-statistics-2/understanding-analysis-of-variance-anova-and-the-f-test
# 


http://www.gardenersown.co.uk/Education/Lectures/R/basics.htm#t_test






