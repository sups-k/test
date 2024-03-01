library(dslabs)
load("mnist.rda")
x <- mnist$train$images
y <- mnist$train$labels

x_1 <- x[6,]
x_2 <- x[17,]
x_3 <- x[16,]

# Distance between a pair of vectors is the norm of the 3 pairs of vectors, x1-x2, x2-x3, x1-x3:
c(crossprod(x_1 - x_2), crossprod(x_2 - x_3), crossprod(x_1 - x_3)) |> sqrt()
# [1] 2319.867 2518.969 2331.210

# Labels of images 6, 17, 16
y[c(6, 17, 16)]
# [1] 2 2 7
# i.e., x_1 = 2
# x_2 also = 2, and
# x_3 = 7

# This means the distance between "2" and "2" (2319) is smaller than the distance
# between "2" and "7" (2518) and "2" and "7" (2331).


# We can compute all the Euclidean distances at once with the `dist` function
d <- dist(x[c(6, 17, 16),])

# 1        2
# 2 2319.867         
# 3 2331.210 2518.969

# We can use the`image` function to see an image of distances between observations
# For the first 300 observations:
d <- dist(x[1:300,])
image(as.matrix(d))

# Order the distance by labels
image(as.matrix(d)[order(y[1:300]), order(y[1:300])])
# Yellow squares at the diagonal represent observations from the same digits
# Yellow means the distance is short

# Ex 1: Generate two matrix, A and B, containing randomly generated and normally
# distributed numbers. The dimensions of these two matrices should be 4 x 3 and 3 x 6,
# respectively. Confirm that C <- A %*% B produces the same results as:
# m <- nrow(A)
# p <- ncol(B)
# C <- matrix(0, m, p)
# for(i in 1:m){
#   for(j in 1:p){
#     C[i,j] <- sum(A[i,] * B[,j])
#   }
# }

A <- matrix(rnorm(12), 4, 3)
B <- matrix(rnorm(18), 3, 6)
C <- A %*% B # Performing matrix multiplication

# Yes it does!

# Ex 2: Solve the following system of equations using R:
# x + y + z + w = 10
# 2x + 3y - z - w = 5
# 3x - y + 4z - 2w = 15
# 2x + 2y - 2z - 2w = 20

cf <- matrix(c(1,1,1,1,2,3,-1,-1,3,-1,4,-2,2,2,-2,-2), 4, 4, byrow = TRUE)
sols <- matrix(c(10, 5, 15, 20), 4, 1)

ans <- solve(cf) %*% sols

# Ex 3: Generate a boxplot showing the distances for the second row of x
# stratified by digits. Do not include the distance to itself, which we know is
# 0. Can you predict what digit is represented by the second row of x?

x <- mnist$train$images[1:300,]
y <- mnist$train$labels[1:300]
d <- as.matrix(dist(x))

# Extract the distances for the second row of d stratified by digits
distances <- list()
for (digit in 0:9) {
  indices <- which(y == digit)
  digit_distances <- d[2, indices] # Extract distances for the digit
  digit_distances <- digit_distances[digit_distances != 0] # Exclude the distance to itself
  distances[[as.character(digit)]] <- digit_distances
}

# Create a boxplot
boxplot(distances, xlab = "Digits", ylab = "Distance", 
        main = "Distances for Each Digit Stratified by Digits")

# The second digit is 0 because it has the lowest distance compared to other digits

# Ex 4: Use the apply function and matrix algebra to compute the distance between
# the fourth digit mnist$train$images[4,] and all other digits represented in
# mnist$train$images. Then generate a boxplot as in exercise 2 and predict what
# digit is the fourth row.









