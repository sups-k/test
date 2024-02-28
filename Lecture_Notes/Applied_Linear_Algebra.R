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
