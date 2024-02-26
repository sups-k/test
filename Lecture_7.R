library(dslabs)
# library(caret) # For machine learning: Can use to split data into test & train
library(tidyverse)

x <- heights$height
y <- heights$sex

#1
guess <- function(x){
  sample(factor(c("Female", "Male")), length(x), replace = TRUE)
}
  
# 2
y_hat <- guess(x)
round(mean(y_hat == y), 3)

# 3
# theta is a "tuning parameter"
cutoff <- function(x, theta){
  # If x > theta, it is female else male
  # as.factor(c("Female", "Male")[as.numeric(x > theta) + 1]) OR use code below
  factor(if_else(x > theta, "Male", "Female"), levels = c("Female", "Male"))
}
# If theta = 66, guess the sex of each of the heights in the heights dataset (which is "x")
y_hat <- cutoff(x, 66)
round(mean(y_hat == y), 3)
# This model is suspiciously accurate with 80.2% accuracy

# 4: Tuning the parameters
thetas <- seq(60, 70, 0.5)
acc <- sapply(thetas, function(theta){
  y_hats <- cutoff(x, theta)
  round(mean(y_hats == y), 5)
})
plot(thetas, acc, type = "l")
thetas[which.max(acc)]
# This means there is more male data. That's why the accuracy is so suspiciously high.
# How can a cut-off of just 55 give a high accuracy of guesses? It cannot. The data
# is biased towards male.

# 5: Get separate parameters for males and females
acc <- sapply(thetas, function(theta){
  y_hats <- cutoff(x, theta)
  accuracy_male <- sum(y_hats[y == "Male"] == "Male") / sum(y == "Male")
  accuracy_female <- sum(y_hats[y == "Female"] == "Female") / sum(y == "Female")
  c(accuracy_male = round(accuracy_male, 5), accuracy_female = round(accuracy_female, 5))
})
matplot(thetas, t(acc), type = "l")

set.seed(2024 - 02 - 12)
test_index <- sample(nrow(heights), round(nrow(heights)/2))
test_set <- heights[test_index, ] 
train_set <- heights[-test_index, ] 

thetas <- seq(60, 70, 0.5)
acc <- function(train_x, train_y, test_x, test_y, thetas) {
  train_acc <- sapply(thetas, function(theta) {
    y_hats_train <- cutoff(train_x, theta)
    accuracy <- sum(y_hats_train == train_y) / length(train_y)
    return(accuracy)
  })
  
  best_theta <- thetas[which.max(train_acc)]
  y_hats_test <- cutoff(test_x, best_theta)
  test_accuracy <- sum(y_hats_test == test_y) / length(test_y)
  
  return(list(best_theta = best_theta, train_accuracy = max(train_acc), test_accuracy = test_accuracy))
}

result <- acc(train_set$height, train_set$sex, test_set$height, test_set$sex, thetas)

result$train_accuracy
result$test_accuracy
