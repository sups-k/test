library(dslabs)
male <- heights$height[heights$sex == "Male"]
female <- heights$height[heights$sex == "Female"]

# EX 6
x <- heights$height[heights$sex=="Male"]
# Assuming x contains the heights of males
proportion_between_69_and_72 <- mean(x > 69 & x <= 72)

# EX 7
# Assuming x contains the heights of males
mean_height <- mean(x)
sd_height <- sd(x)

# Calculate the proportion between 69 and 72 inches
proportion_between_69_and_72 <- pnorm(72, mean = mean_height, sd = sd_height) - pnorm(69, mean = mean_height, sd = sd_height)

# Print the result
print(proportion_between_69_and_72)

# EX 8
# Assuming x contains the heights of males
boxplot(x, main = "Boxplot of Heights", ylab = "Height (inches)")

# EX 9
# Assuming x contains the heights of males
median_height <- median(x)
mad_height <- mad(x)

# Calculate the proportion between 69 and 72 inches
proportion_between_69_and_72 <- pnorm(72, mean = median_height, sd = 1.4826 * mad_height) -
  pnorm(69, mean = median_height, sd = 1.4826 * mad_height)

# Print the result
print(proportion_between_69_and_72)

# EX 10
# Assuming x contains the heights of males
median_height <- median(x)
mad_height <- mad(x)

# Define the height threshold in inches (7 feet = 84 inches)
height_threshold <- 84

# Calculate the proportion of males 7 feet tall or taller
proportion_7_feet_tall_or_taller <- 1 - pnorm(height_threshold, mean = median_height, sd = 1.4826 * mad_height)

# Calculate the number of males in the specified age range
total_males_in_us <- 850e6

# Calculate the estimated number of males 7 feet tall or taller
estimated_males_7_feet_tall_or_taller <- total_males_in_us * proportion_7_feet_tall_or_taller

# Print the result
print(estimated_males_7_feet_tall_or_taller)

