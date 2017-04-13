Supervised Learning we have choices like NaiveBayes, K nearest neighbours
using Decision Tree or Classification Tree

PACKAGE C50

install.packages("C50")
library(C50)
data(iris)
head(iris)

#predict flower species using Classification Tree from package C50

#don't want any order
set.seed(9850)
g<-runif(nrow(iris)) 
irisr<-iris[order(g),]
head(irisr)

C5.0(training data, test data)
m1<-C5.0(irisr[1:100,-5], irisr[1:100,5])
m1  #4 leafs

summary(m1)

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
###########
p1<-predict(m1,irisr[101:150,])

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

## ANOTHER PACKAGE
library(rpart) # another package 
install.packages("rpart.plot")
library(rpart.plot)

#don't want any order - shuffle the deck
set.seed(9850)
g<-runif(nrow(iris)) 
irisr<-iris[order(g),]
head(irisr)

#rpart works similar to lm ( target~predictors,dataset,method=classification tree)

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


rpart creates plot 

rpart.plot(m3) 
#plot with some options
rpart.plot(m3,type=3,extra=101,fallen.leaves=T) 

summary(m3)

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



