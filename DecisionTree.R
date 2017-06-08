#DECISION TREES
# - split on Gini Index, Chi-Square or Entropy for categorical target variable
# - split on Reduction in Varaince for continuous targetvariables
# - pruning or setting constrains on tree size to avoid overfitting
#   package RPART provides a function to prune
# - R packages: ctree, rpart, tree
#We have choices like NaiveBayes, K nearest neighbours
#using Decision Tree or Classification Tree


install.packages("C50")
	library(C50)
	data(iris)
	head(iris)

#predict flower species using Classification Tree from package C50


#don't want any order - irisr is randomized
	set.seed(9850)
	g<-runif(nrow(iris)) 
	irisr<-iris[order(g),]
	head(irisr)

#C5.0(training data, test data)
# training data: take first 100 rows as  and eliminate target feature at 5 column
# target data is a column 5 Species
	
	m1<-C5.0(irisr[1:100,-5], irisr[1:100,5])

# so m1 has 4 leafs

summary(m1) is more informative 

Shows:

Petal.Length <= 1.9: setosa (34)          ---> root  <1.9   --> leaf1(setosa)
Petal.Length > 1.9:                           >1.9  >decision(Width) >1.6 --> leaf2(virginica)
:...Petal.Width > 1.6: virginica (29)                                <1.6 --> decision(Length)<4.9 --->leaf4(versicolor)
    Petal.Width <= 1.6:
    :...Petal.Length <= 4.9: versicolor (35)        add leafs(..) = 100                                          >4.9 --leaf5(virginica)                          
        Petal.Length > 4.9: virginica (2)



Evaluation on training data (100 cases):

            Decision Tree   
          ----------------  
          Size      Errors                       --prediction is too perfect

             4    0( 0.0%)   <<


           (a)   (b)   (c)    <-classified as       ---confusion matrix
          ----  ----  ----
            34                (a): class setosa
                  35          (b): class versicolor
                        31    (c): class virginica
#################################################################################
	
	p1<-predict(m1,irisr[101:150,])   # model against test data 

#construct confusion matrix  Actual vs Predicted

	table( irisr[101:150,5],Predicted=p1)

             Predicted
             setosa versicolor virginica
  setosa         16          0         0
  versicolor      0         12         3-------->little error - there were 3 versicolor the model predicted as virginica
  virginica       0          0        19                    

performance is 47/50 


## draw the plot, works well on small sets
plot(m1)





#############################################################################################

## ANOTHER PACKAGE RPART and RPART.PLOT
	library(rpart)
	library(rpart.plot)
We are predicting categorical value Species in a 5th column, we use classification tree

#don't want any order - shuffle the deck
	set.seed(9850)
	g<-runif(nrow(iris)) 
	irisr<-iris[order(g),]
	head(irisr)

#rpart works similar to lm ( target~predictors,dataset,method=classification tree) where . means all possible predictors
#first 100 rows are training
	m3<-rpart( Species ~ .,irisr[1:100,], method="class" ) 
	m3


outputs:
node), split, n, loss, yval, (yprob)
      * denotes terminal node

1) root 100 65 versicolor (0.34000000 0.35000000 0.31000000)                  TREE 
  2) Petal.Length< 2.6 34  0 setosa (1.00000000 0.00000000 0.00000000) *   root(length) <2.6 ---->Leaf( setosa) 
  3) Petal.Length>=2.6 66 31 versicolor (0.00000000 0.53030303 0.46969697)              >2.6 decision width <1.65 then leaf2(versicolor)
    6) Petal.Width< 1.65 37  2 versicolor (0.00000000 0.94594595 0.05405405) *                              >1.65 leaf3 ( virginica) 
    7) Petal.Width>=1.65 29  0 virginica (0.00000000 0.00000000 1.00000000) *
> 


#rpart creates nice plot 

	rpart.plot(m3) 
#plot with some options
	rpart.plot(m3,type=3,extra=101,fallen.leaves=T) 

	summary(m3)

#validate prediction 
	p3<-predict(m3,irisr[101:150,],type="class")

#construct CONFUSION MATRIX  Actual vs Predicted
	table( irisr[101:150,5],predicted=p3)


outputs:
       predicted
             setosa versicolor virginica
  setosa         16          0         0
  versicolor      0         13         2     -- little error 2 versicolor were recognized as virginica
  virginica       0          2        17
                             --error 
Prediction performance 46/50  

##############################################################################33

   Regression Decision Tree 
	
##############################################################################33
	library(rpart)
	library(rpart.plot)
      library (ggplot2) # for mamal sleep data
	data() # show all data sets

	data(sleep) #from package ggplot2
# msleep dataset has 11 variables, 83 observations	
	msleep
#take only subset by eliminate some columns 
      df<-msleep[,c(3,6,10,11)]

#variables: vore is carnivore or omnivore , sleep total, brain weight, body weight 

# goal is to predict total sleep hours for a given mammal 

	m1<-rpart(sleep_total~ .,data=df,method="anova") 
 
      m1
#so, there were 83 observations, first split was on BodyWeight which was most predictive 
#show plot
	rpart.plot(m1)
      rpart.plot(m1,type=3,digits=3,fallen.leaves=TRUE)

# test value is same as train in this case 
	p1=predict(m1,df) 

# asses the accuracy by calculation Mean Absolute Error - create a small function

    MAE<-function( actual, predicted) { mean(abs(actual-predicted))}

    MAE( df$sleep_total, p1) 
# MAE is 2.45

############################################################################################################




################################################
# Another classification tree using package Tree( from islr)

	library(tree)
	library(ISLR)
	attach(Carseats)
#visualize
      hist(Sales)
#convert  sales to categorical variable - to demonstrate classification tree
	High=ifelse(Sales<=8,"No","Yes")
#put High back on the dataframe
	Carseats=data.frame(Carseats,High)
#exclude Sales from a dataset 
	tree.carseats=tree(High~.-Sales,Carseats)
	summary(tree.carseats)	
	plot(tree.carseats)
	text(tree.carseats,pretty=0)
	tree.carseats

#split set int to training and test
	set.seed(2)
	train=sample(1:nrow(Carseats), 200)
	Carseats.test=Carseats[-train,]
	High.test=High[-train]
#retrain using training subset
	tree.carseats=tree(High~.-Sales,Carseats,subset=train)
#predict on train data 
	tree.pred=predict(tree.carseats,Carseats.test,type="class")
	table(tree.pred,High.test)
(86+57)/200

#cv.tree is using Cross Validation to prune 
	set.seed(3)
	cv.carseats=cv.tree(tree.carseats,FUN=prune.misclass)
	names(cv.carseats)
	cv.carseats
#fit two plots
	par(mfrow=c(1,2))
	plot(cv.carseats$size,cv.carseats$dev,type="b")
	plot(cv.carseats$k,cv.carseats$dev,type="b")

#Cross validation picks 9 as an optimal value 

#try different pruning -------------------------
	prune.carseats=prune.misclass(tree.carseats,best=9)
	plot(prune.carseats)
	text(prune.carseats,pretty=0)
	tree.pred=predict(prune.carseats,Carseats.test,type="class")
	table(tree.pred,High.test)
(94+60)/200
	prune.carseats=prune.misclass(tree.carseats,best=15)
	plot(prune.carseats)
	text(prune.carseats,pretty=0)
	tree.pred=predict(prune.carseats,Carseats.test,type="class")
	table(tree.pred,High.test)
(86+62)/200




################################################
# Another regression tree using package Tree( from islr)

	library(MASS)
	library(tree)
	set.seed(1)
	train = sample(1:nrow(Boston), nrow(Boston)/2)
	tree.boston=tree(medv~.,Boston,subset=train)
	summary(tree.boston)
	plot(tree.boston)
	text(tree.boston,pretty=0)
	cv.boston=cv.tree(tree.boston)
#Pruning	
	plot(cv.boston$size,cv.boston$dev,type='b')
	prune.boston=prune.tree(tree.boston,best=5)
	plot(prune.boston)
	text(prune.boston,pretty=0)
	yhat=predict(tree.boston,newdata=Boston[-train,])
#Test set	
	boston.test=Boston[-train,"medv"]
	plot(yhat,boston.test)
	abline(0,1)
	mean((yhat-boston.test)^2)




