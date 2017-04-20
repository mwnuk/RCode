http://trevorstephens.com/kaggle-titanic-tutorial/r-part-5-random-forests/

Random Forests process has the two sources of randomness:
1.Bagging takes a randomized sample of the rows in your training set, with replacement
2.Random Forests take only a subset of variables, typically the square root of the number available.


install.packages('randomForest')
library(randomForest)

library(titanic)
