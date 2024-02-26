library(dslabs)
library(tidyverse)
library(caret)

x <- heights$height
y <- heights$sex

# 1: Use the caret package to split the heights dataset into training and testing sets. Use 50% of the data to train.

set.seed(2024 - 02 - 12)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]

# 2: Use the glm function to fit a logistic regression model to the training set.
# Then use the predict.glm function obtain an estimate of Pr_hat(Y=1|X=x) for the
# x in the test set. Logistic regression will provide an estimate of Pr_hat. Plot
# Pr_hat vs x. We will use the caret package default of defining the first level
# of Y as a 1, so Y=1 implies female.

fit <- glm(sex ~ height, family = "binomial", data = train_set)
p_hat <- 1 - predict(fit, newdata = test_set, type = "response")

# Plot Pr_hat vs x
ggplot(test_set, aes(x = height, y = p_hat, color = sex)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(x = "Height", y = "Estimated Probability (p_hat)", color = "Sex") +
  theme_minimal()

# 3: Use a cutoff of theta = 0.5 to produce predictions y_hat by defining y_hat = 1
# if Pr_hat < theta. Then use the caret package to obtain sensitivity, specificity, and prevalence.

y_hat <- ifelse(p_hat < 0.5, 1, 2)
y_hat <- as.factor(if_else(y_hat == 1, "Female", "Male"))

# Compute confusion matrix
conf_matrix <- confusionMatrix(data = y_hat, reference = test_set$sex)

# Obtain sensitivity, specificity, and prevalence
sensitivity <- conf_matrix$byClass["Sensitivity"]
specificity <- conf_matrix$byClass["Specificity"]
prevalence <- conf_matrix$byClass["Prevalence"]

# Print the results
print(paste("Sensitivity:", sensitivity))
print(paste("Specificity:", specificity))
print(paste("Prevalence:", prevalence))

# 4: Find the cutoff that maximizes accuracy.
thetas <- seq(0.1, 0.9, length = 100)
acc <- sapply(thetas, function(theta){
  # Generate predictions based on the current cutoff threshold
  y_hats <- if_else(p_hat < theta, 1, 2)
  y_hats <- as.factor(if_else(y_hats == 1, "Female", "Male"))
  # Compute accuracy
  accuracy <- round(mean(y_hats == test_set$sex), 5)
  return(accuracy)
})
optimal_cutoff <- thetas[which.max(acc)]
plot(thetas, acc, type = "l")
abline(v = optimal_cutoff, col = "red", lty = 2)


#-------------------------------------------------------------#
# 5: Repeat exercise 4 but show sensitivity and specificity and which cutoff maximizes each.

acc <- sapply(thetas, function(theta){
  y_hats <- if_else(p_hat < theta, 1, 2)
  y_hats <- as.factor(if_else(y_hats == 1, "Female", "Male"))
  conf_matrix <- confusionMatrix(y_hats, test_set$sex)
  sensitivity <- conf_matrix$byClass["Sensitivity"]
  specificity <- conf_matrix$byClass["Specificity"]
  return(c(sensitivity, specificity))
})
matplot(thetas, t(acc), type = "l")
# Find the cutoff value that maximizes sensitivity
thetas[which.max(acc[1, ])]
# Find the cutoff value that maximizes specificity
thetas[which.max(acc[2, ])]

# 6: Use F1 to pick a cutoff instead of overall accuracy

# Define a function to compute F1 score for a given cutoff value
compute_f1_score <- function(theta) {
  # Generate y_hat based on the current cutoff threshold
  y_hats <- if_else(p_hat < theta, 1, 2)
  y_hats <- as.factor(if_else(y_hats == 1, "Female", "Male"))
  
  # Compute confusion matrix
  conf_matrix <- confusionMatrix(y_hats, test_set$sex)
  
  # Extract true positives, false positives, and false negatives from the confusion matrix
  tp <- conf_matrix$table[2, 2]
  fp <- conf_matrix$table[1, 2]
  fn <- conf_matrix$table[2, 1]
  
  # Compute precision, recall, and F1 score
  precision <- tp / (tp + fp)
  recall <- tp / (tp + fn)
  f1_score <- 2 * (precision * recall) / (precision + recall)
  
  return(f1_score)
}

# Compute F1 score for each cutoff value
f1_scores <- sapply(thetas, compute_f1_score)

# Find the cutoff value that maximizes F1 score
optimal_cutoff_f1 <- thetas[which.max(f1_scores)]

# Plot F1 score vs. cutoff values
plot(thetas, f1_scores, type = "l", xlab = "Cutoff Value", ylab = "F1 Score")

# Add a vertical line for the optimal cutoff
abline(v = optimal_cutoff_f1, col = "red", lty = 2)

# Print the optimal cutoff value
print(paste("Optimal Cutoff for F1 Score:", optimal_cutoff_f1))

# 7: Repeat the splitting of the data into halves 100 times. Plot the 250 F1 versus cutoff plots.

# Define the range of cutoff values
thetas <- seq(0.1, 0.9, length = 100)

# Initialize a matrix to store F1 scores for each repetition
f1s <- matrix(0, nrow = 100, ncol = 250)

# Initialize vector to store optimal cutoff values
optimal_cutoffs <- numeric(250)

# Repeat the splitting and F1 score computation 250 times
for (i in 1:250) {
  # Split the data into training and testing sets
  test_index <- createDataPartition(y, p = 0.5, list = FALSE)
  test_set <- heights[test_index, ]
  train_set <- heights[-test_index, ]
  
  # Fit logistic regression model to the training set
  fit <- glm(sex ~ height, family = "binomial", data = train_set)
  
  # Predict probabilities for the test set
  p_hat <- 1 - predict(fit, newdata = test_set, type = "response")
  
  # Compute F1 score for each cutoff value
  f1_scores <- sapply(thetas, compute_f1_score)
  
  # Store F1 scores for the current repetition
  f1s[, i] <- f1_scores
  
  # Compute accuracies
  accuracies <- sapply(thetas, function(theta){
    y_hats <- if_else(p_hat < theta, 1, 2)
    y_hats <- as.factor(if_else(y_hats == 1, "Female", "Male"))
    
    # Compute confusion matrix
    conf_matrix <- confusionMatrix(y_hats, test_set$sex)
    
    # Extract accuracy from the confusion matrix
    accuracy <- conf_matrix$overall["Accuracy"]
    
    return(accuracy)
  })
  
  # Find the optimal cutoff value that maximizes accuracy
  optimal_cutoff <- thetas[which.max(accuracies)]
  
  # Store the optimal cutoff value for this iteration
  optimal_cutoffs[i] <- optimal_cutoff
}

# Plot F1 score versus cutoff plots
matplot(thetas, f1s, type = "l", col = "grey", xlab = "Cutoff Value", ylab = "F1 Score")



# 8: Make a histogram of the thetas that maximize accuracy in each iterations.

# Plot histogram of optimal cutoff values
hist(optimal_cutoffs, main = "Histogram of Optimal Cutoff Values", xlab = "Cutoff Value", ylab = "Frequency", col = "pink")


# 9: Repeat exercise 8 but this time splitting into 80% training 20% testing.
# How do the distributions compare?

# Initialize vector to store optimal cutoff values
optimal_cutoffs_80 <- numeric(250)

# Repeat the splitting and F1 score computation 250 times
for (i in 1:250) {
  # Split the data into training and testing sets
  train_index_80 <- createDataPartition(y, p = 0.8, list = FALSE)
  train_set_80 <- heights[train_index_80, ]
  test_set_20 <- heights[-train_index_80, ]
  
  # Fit logistic regression model to the training set
  fit <- glm(sex ~ height, family = "binomial", data = train_set_80)
  
  # Predict probabilities for the test set
  p_hat <- 1 - predict(fit, newdata = test_set_20, type = "response")
  
  # Compute accuracies
  accuracies <- sapply(thetas, function(theta){
    y_hats <- if_else(p_hat < theta, 1, 2)
    y_hats <- as.factor(if_else(y_hats == 1, "Female", "Male"))
    
    # Compute confusion matrix
    conf_matrix <- confusionMatrix(y_hats, test_set_20$sex)
    
    # Extract accuracy from the confusion matrix
    accuracy <- conf_matrix$overall["Accuracy"]
    
    return(accuracy)
  })
  
  # Find the optimal cutoff value that maximizes accuracy
  optimal_cutoff <- thetas[which.max(accuracies)]
  
  # Store the optimal cutoff value for this iteration
  optimal_cutoffs_80[i] <- optimal_cutoff
}

hist(optimal_cutoffs_80, main = "Histogram of Optimal Cutoff Values", xlab = "Cutoff Value", ylab = "Frequency", col = "skyblue")



