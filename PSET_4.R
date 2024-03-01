#### Problem Set: 4
#### Author: Suparna Kumar

# Load libraries
library(tidyverse)
library(dslabs)
library(caret)
library(matrixStats)
library(randomForest)
library(glmnet)

# Load MNIST dataset
if (!exists("mnist")) mnist <- read_mnist()

# Prepare the data for training and testing the models

set.seed(2024-2-14)

# Sample 10,000 random rows from the training set
index <- sample(nrow(mnist$train$images), 10000)
x <- mnist$train$images[index,]
y <- factor(mnist$train$labels[index])

# Sample 1000 random rows from the test set
index <- sample(nrow(mnist$test$images), 1000)
x_test <- mnist$test$images[index,]
y_test <- factor(mnist$test$labels[index])

##########------------------kNN------------------------------###################

# 1: Train a KNN model. Report the error rate on the test data only after you
# decide on a model.

# Remove features with near-zero variance
nzv <- nearZeroVar(x) # Prints column names of pixels that are on the edge
col_index <- setdiff(1:ncol(x), nzv)
# length(col_index)

# Add column names to feature matrices (training & testing)
colnames(x) <- 1:ncol(mnist$train$images)
colnames(x_test) <- colnames(x)

# 5-fold cross-validation
control <- trainControl(method = "cv", number = 5, p = .9)

# Find best training parameter (k)
# set.seed(2008)
train_knn_cv <- train(x = x[, col_index],
                      y = y,
                      method = "knn",
                      tuneGrid = data.frame(k = seq(3, 13, 2)),
                      trControl = control)

# plot(train_knn_cv) # will show you the MSE in a graph
# If MSE is too wiggly, it means do more cross-validation with bigger `k` in "k-cross validation"

# Train kNN model with best k value
# fit_knn <- knn3(x[, col_index], y,  k = train_knn_cv$bestTune)
# This is NOT REQUIRED. `train_knn_cv` already has the BEST one stored in it

# Calculate error rate of the trained kNN model
# DON'T DO THIS `predict`. ONLY USE `train_knn_cv` FOR `predict`
# y_hat_knn <- predict(fit_knn, x_test[, col_index], type = "class")
y_hat_knn <- predict(train_knn_cv, x_test[, col_index], type = "class")
acc <- confusionMatrix(y_hat_knn, factor(y_test))$overall["Accuracy"]
error_rate <- 1 - acc
print(paste("Error rate of kNN: ", error_rate))


##########----------Random Forest----------------------###################

# 2: Train a Random Forest Model. Report the error rate on the test data only after
# you decide on a model.

# We have already removed features with near-zero variance,
# added column names to the feature matrices, and defined the 5-fold cross-validation

# Since this is a classification problem, I selected "gini" and "1" as they are the
# default values specified in the "ranger" documentation
# Using ranger for reduced computation time 
grid <- expand.grid(mtry = seq(4, 24, 5), splitrule = "gini", min.node.size = 1)

# Tune Random Forest model parameters
train_rf_cv <- train(x = x[, col_index],
                     y = y, 
                     method = "ranger", 
                     tuneGrid = grid,
                     trControl = control)

# Train Random Forest model with best mtry
fit_rf <- randomForest(x[, col_index], y, mtry = train_rf_cv$bestTune$mtry)

# Find optimal number of trees
plot(fit_rf)
# Error rate stabilises after 100 trees

# Train Random Forest model with best mtry and number of trees
fit_rf_100 <- randomForest(x[, col_index], y, mtry = train_rf_cv$bestTune$mtry, ntree = 100)

# Remove previous Random Forest model
rm(fit_rf)

# Calculate error rate of the trained Random Forest model
y_hat_rf <- predict(fit_rf_100, x_test[, col_index], type = "class")
acc <- confusionMatrix(y_hat_rf, factor(y_test))$overall["Accuracy"]
error_rate <- 1 - acc
print(paste("Error rate of Random Forest: ", error_rate))


##########-------------Generalised Linear Model----------------###################

# 3: Train a model of your choosing. Report the error rate on the test data only
# after you decide on a model.

# We have already removed features with near-zero variance,
# added column names to the feature matrices, and defined the 5-fold cross-validation

# Tune glm model parameter
# Alpha: mixing parameter for elastic net (0 for ridge, 1 for lasso)
train_glm_cv <- train(x = x[, col_index],
                     y = y, 
                     method = "glmnet", 
                     tuneGrid = data.frame(alpha = 0:1),
                     trControl = control)

# Train glm model with best parameters
fit_glm <- glmnet(x[, col_index], y, family = "multinomial", alpha = train_glm_cv$bestTune$alpha, lambda = 1)

# Calculate error rate of the trained glm model
y_hat_glm <- predict(fit_glm, x_test[, col_index], type = "class")
acc <- confusionMatrix(factor(y_hat_glm), y_test)$overall["Accuracy"]
error_rate <- 1 - acc
print(paste("Error rate of GLM: ", error_rate))


##########---------Ensemble----------------------------#######################
# 4: Build an ensemble with the three methods. Feel free to add more if you want.
# Report the error rate on the test data only after you build your ensemble.

# kNN
p_knn  <- predict(fit_knn,  x_test[,col_index],  type="prob")
p_knn <- p_knn / rowSums(p_knn)

# Random Forest
p_rf <- predict(fit_rf_100, x_test[,col_index], type = "prob")  
p_rf <- p_rf / rowSums(p_rf)

# GLM
# TYPE MUST BE "PROB" HERE OTHERWISE "RAW"
p_glm <- predict(fit_glm,  x_test[,col_index],  type="response") |>
  dplyr::as_tibble() |>
  dplyr::rename_with(~str_remove(., '.s0')) |> 
  as.matrix()
p_glm <- p_glm / rowSums(p_glm)

# Ensemble
p <- (p_rf + p_knn + p_glm)/3

y_pred <- factor(apply(p, 1, which.max) - 1)

# Error rate of ensemble
acc <- confusionMatrix(y_pred, y_test)$overall["Accuracy"]
error_rate <- 1 - acc
print(paste("Error rate of ensemble: ", error_rate))


######## Parallelisation ######
library(doParallel)
n_cores <- detectCores() # 8 cores in my Mac
print(n_cores)
registerDoParallel(cores = n_cores - 1)
control <- trainControl(method = "cv", number = 10, p = 0.9, allowParallel = TRUE)
