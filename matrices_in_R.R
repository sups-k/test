# Create a 100 by 10 matrix of randomly generated normal numbers. Put the result in x.
x <- matrix(rnorm(1000), 100, 10)

# Get dimensions, number of rows, and number of columns of x.
dim(x)
nrow(x)
ncol(x)

# Add the scalar 1 to row 1, scalar 2 to row 2, and so on to the matrix x.
x <- sweep(x, 1, c(1:nrow(x)), FUN = "+")

# Add the scalar 1 to column 1, scalar 2 to column 2, and so on to the matrix x.
x <- sweep(x, 2, c(1:ncol(x)), FUN = "+")

# Compute the average of each row of x.
library(matrixStats)
row_avg <- rowMeans(x)

# Compute the average of each column of x.
col_avg <- colMeans(x)

# For each digit in the MNIST training data, compute the proportion of pixels that
# are in a grey area, defined as values between 50 and 205.

# Load MNIST training data and labels
mnist <- dslabs::read_mnist()
imgs <- mnist$train$images
lbls <- mnist$train$labels
rm(mnist)

# Calculate proportion of pixels in the grey area for each image
props <- rowSums2(imgs >= 50 & imgs <= 205) / ncol(imgs)

# Calculate the average proportion of pixels in the grey area per digit
props_digit <- cbind(lbls, props) |>
  dplyr::as_tibble() |> 
  dplyr::group_by(lbls) |> 
  dplyr::summarise(avg = mean(props, na.rm = TRUE)*100)

