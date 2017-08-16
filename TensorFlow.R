#https://tensorflow.rstudio.com/index.html

library(tensorflow)
library(reticulate)
datasets <- tf$contrib$learn$datasets
mnist <- datasets$mnist$read_data_sets("MNIST-data", one_hot = TRUE)

#The MNIST data is split into three parts: 
#55,000 data points of training data (mnist$train), 
#10,000 points of test data (mnist$test), 
#and 5,000 points of validation data (mnist$validation). 


x <- tf$placeholder(tf$float32, shape(NULL, 784L))
W <- tf$Variable(tf$zeros(shape(784L, 10L)))
b <- tf$Variable(tf$zeros(shape(10L)))

#BUILD MODEL
y <- tf$nn$softmax(tf$matmul(x, W) + b)
y

#TRAIN MODEL
y_ <- tf$placeholder(tf$float32, shape(NULL, 10L))

cross_entropy <- tf$reduce_mean(-tf$reduce_sum(y_ * tf$log(y), reduction_indices=1L))

optimizer <- tf$train$GradientDescentOptimizer(0.5)
train_step <- optimizer$minimize(cross_entropy)

init <- tf$global_variables_initializer()
sess <- tf$Session()
sess$run(init)

# run batches of random 100  - it called stochastic training
for (i in 1:1000) {
  batches <- mnist$train$next_batch(100L)
  batch_xs <- batches[[1]]
  batch_ys <- batches[[2]]
  sess$run(train_step,
           feed_dict = dict(x = batch_xs, y_ = batch_ys))
}

#EVALUATING MODEL - argmax gives index of a highest entry in the tensor
correct_prediction <- tf$equal(tf$argmax(y, 1L), tf$argmax(y_, 1L))
accuracy <- tf$reduce_mean(tf$cast(correct_prediction, tf$float32))

sess$run(accuracy, feed_dict=dict(x = mnist$test$images, y_ = mnist$test$labels))
# gives 92% accuracy which is not good enough
##############################################################################






