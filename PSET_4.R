# Create training and test data like this
library(dslabs)
if (!exists("mnist")) mnist <- read_mnist()
set.seed(2024-2-14)
index <- sample(nrow(mnist$train$images), 10000)
x <- mnist$train$images[index,]
y <- factor(mnist$train$labels[index])
index <- sample(nrow(mnist$test$images), 1000)
x_test <- mnist$test$images[index,]
y_test <- factor(mnist$test$labels[index])

# 1: Train a KNN model. Report the error rate on the test data only after you
# decide on a model.

# 2: Train a Random Forest Model. Report the error rate on the test data only after
# you decide on a model.

# 3: Train a model of your choosing. Report the error rate on the test data only
# after you decide on a model.

# 4: Build an ensemble with the three methods. Feel free to add more if you want.
# Report the error rate on the test data only after you build your ensemble.