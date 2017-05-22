#DECISION TREES
# - split on Gini Index, Chi-Square or Entropy
# - pruning or setting constrains on tree size
# - R packages: ctree, rpart, tree
#ENSAMBLE METHODS
# - BAGGING - Bootstrap Sampling - sub-samples
# - Random Forest - random split decision, good for classification
# - Boosting - every split improves the chances

https://www.analyticsvidhya.com/blog/2016/04/complete-tutorial-tree-based-modeling-scratch-in-python/

http://trevorstephens.com/kaggle-titanic-tutorial/r-part-5-random-forests/

Random Forests process has the two sources of randomness:
1.Bagging takes a randomized sample of the rows in your training set, with replacement
2.Random Forests take only a subset of variables, typically the square root of the number available.


install.packages('randomForest')
library(randomForest)

library(titanic)
