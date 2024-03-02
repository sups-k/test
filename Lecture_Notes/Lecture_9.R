# Genomics PCA exercise
# Author: Suparna Kumar

# 1. Import these files gtex/gene_read_counts_by_tissue/gene_reads_2017-06-05_v8_artery_aorta.gct
# and gtex/gene_read_counts_by_tissue/gene_reads_2017-06-05_v8_artery_tibial.gct
# into two data frames. Hint: consider using the data.table function fread.

# setwd()

library(data.table)
# Gene counts by tissue
# Have to skip 2 lines of this tsv file
fn1 <- "../gtex/gene_read_counts_by_tissue/gene_reads_2017-06-05_v8_artery_aorta.gct"
fn2 <- "../gtex/gene_read_counts_by_tissue/gene_reads_2017-06-05_v8_artery_tibial.gct"

df1 <- fread(fn1, skip = 2)
df2 <- fread(fn2, skip = 2)

# We get number of genes x number of samples
dim(df1)
dim(df2)

# 2. Confirm that the rows represent the same genes in the two data frames.

# Verify that genes are in the same order in both the data frames, e.g., gene 1
# in df1 is also gene 1 in df2
identical(df1$Name, df2$Name)

# 3. Save the three columns of one of the data frames. This will serve as a map
# for genenames. Call the resulting object gene_map.

gene_map <- df1[, c("id", "Name", "Description")]
# gene_map[1:5, ]

# 4. Remove the columns related to gene identification from both tables and
# create two numeric matrices x and y. Once you create the two matrices, remove
# the data frames to save space.

x <- as.matrix(df1[, c("id", "Name", "Description"):=NULL])
y <- as.matrix(df2[, c("id", "Name", "Description"):=NULL])
## free memory by removing data frames
rm(df1)
rm(df2)
## gc stands for garbage collection. it checks that memory is returned
## we run it twice can't hurt, but it can catch some memory left behind by first run
gc();gc()

# 5. Compute a vector d with the average differences between aorta and tibial for each gene.
log_x <- log2(x+0.5)
log_y <- log2(y+0.5)
d <- rowMeans(log_y) - rowMeans(log_x)

# 6. Compute the average across all samples (combine x and y) and keep it in a.
# Make a histogram of d, a histogram of a, and plot d versus a. Use log
# transformations where you deem necessary. Comment on what you observe

a <- rowMeans(cbind(log_x, log_y))
par(mfrow=c(2,2)) ## this puts R base plot in 2 x 2 grid
hist(d, nc = 100)
hist(a)
plot(a, d)
par(mfrow=c(1,1))

# 7. For the aorta, compute the sample standard deviation of each gene. Plot this
# standard deviation against the average count in the aorta samples for each gene.
# Use a log transformation for both axes. Remove genes with average counts of 0.
# Add the identity line in red. Comment on the variance-mean relationship.

library(matrixStats)
s <- rowSds(x)
a <- rowMeans(x)

plot(log10(a), log10(s))
abline(0, 1, col = "red")

## There is very strong relationship between mean and variance.

# 8. Data with this type of relationship is unlikely to be normally distributed.
# This and the the heavy tail of the gene averages are often used to justify the
# use a log transformation. However, there is a reason this won't work well here.
# Make a plot of the percent of 0s versus the sum of the counts for each sample
# (called coverage in genomics). Use color to distinguish the two tissues.

library(tidyverse)
### make a temporary data frame to then use with ggplot
df <- data.frame(zeros = c(colMeans(x == 0), colMeans(y == 0)),
                 coverage = c(colSums(x), colSums(y)),
                 tissue = c(rep("Aorta", ncol(x)), rep("Tibial", ncol(y))))
## Now use ggplot
df |> ggplot(aes(x = coverage, y = zeros, color = tissue)) +
  geom_point() +
  labs(x = "Sum of Gene Counts", y = "Percent of Zeros") +
  ggtitle("Percent of Zeros vs Sum of Gene Counts") +
  theme_minimal()

round(colMeans(x == 0), 2)
# 50% of each gene are 0.

# 9. We observe the following: a substantial proportion of the data are 0 counts,
# the proportion of counts varies substantially across samples, and the proportion
# of 0s correlated with the total number of counts for each samples. This all
# seems to be consistent with Poisson distributed data with varying population
# sizes. To explore this, make histograms for the genes ranked 25%, 50%, 75%, and
# highest (confirm that the 0% will be all 0s.) Comment on what distribution
# seems to fit the data.

ind <- order(rowMeans(x))
## the first one is the smallest row mean. Let's check if it is 0:
all(x[ind[1],]==0)
## Now let's pick 4 indeces that represent 25%, 50%, 75% and largest:
ind <- ind[round(seq(0.25, 1, 0.25)*length(ind))]

par(mfrow = c(2,2))
## Write a forloop that makes each histogram
for (i in 1:length(ind)) {
  # Subset data for each percentile
  data_subset <- x[ind[i], ]
  
  # Create histogram
  hist(data_subset, main = paste("Gene Rank:", i * 25, "%"), xlab = "Counts for Aorta", col = "skyblue", border = "white")
}

par(mfrow = c(1,1))

ind <- order(rowMeans(y))
## the first one is the smallest row mean. Let's check if it is 0:
all(y[ind[1],]==0)
## Now let's pick 4 indices that represent 25%, 50%, 75% and largest:
ind <- ind[round(seq(0.25, 1, 0.25)*length(ind))]

par(mfrow = c(2,2))
## Write a forloop that makes each histogram
for (i in 1:length(ind)) {
  # Subset data for each percentile
  data_subset <- y[ind[i], ]
  
  # Create histogram
  hist(data_subset, main = paste("Gene Rank:", i * 25, "%"), xlab = "Counts for Tibial Artery", col = "skyblue", border = "white")
}

par(mfrow = c(1,1))

# A Poisson distribution seems to fit the data

# 10. A popular approach used for RNA-Seq count data is to model the data as a
# negative binomial distribution (similar to Poisson, but allows for different
# variances). Under this assumption we can find a transformation that removes
# the dependence of variance on mean and can handle 0s. The transformation ends
# up being similar to the log except for smaller values. Run the transformation
# vst from the DESeq2 package on both x and y like this z <- DESeq2::vst(cbind(x,y)).
# Then compute the average difference d and average across the combined data a
# and plot these against each other.

## we can combine the matrices into one matrix then apply vst
library(DESeq2)
z <- vst(cbind(x,y))
dim(z)
## we then the right column indexes
## we can then subset z to get what we want
ind_x <- 1:ncol(x)
ind_y <- (ncol(x) + 1):(ncol(x) + ncol(y))

# Subset z to get what we want
z_x <- z[, ind_x]
z_y <- z[, ind_y]

# Compute average difference (d) and average across the combined data (a)
d <- rowMeans(z_x) - rowMeans(z_y)
a <- rowMeans(cbind(z_x, z_y))

# Plot average difference against average across the combined data
plot(a, d, xlab = "Average across combined data", ylab = "Average difference", main = "Average difference vs Average across combined data")

# 11. Remake the histograms from exercise 9. Note that, at least for the higher
# expressed genes, the data looks closer to normally distributed.

ind <- order(rowMeans(z_x))
## Now let's pick 4 indeces that represent 25%, 50%, 75% and largest:
ind <- ind[round(seq(0.25, 1, 0.25)*length(ind))]

par(mfrow = c(2,2))
## Write a forloop that makes each histogram
for (i in 1:length(ind)) {
  # Subset data for each percentile
  data_subset_x <- z_x[ind[i], ]
  # Create histogram
  hist(data_subset_x, main = paste("Gene Rank:", i * 25, "%"), xlab = "Counts for Aorta", col = "skyblue", border = "white")
}
par(mfrow = c(1,1))
ind <- order(rowMeans(z_y))
## Now let's pick 4 indices that represent 25%, 50%, 75% and largest:
ind <- ind[round(seq(0.25, 1, 0.25)*length(ind))]

par(mfrow = c(2,2))
## Write a forloop that makes each histogram
for (i in 1:length(ind)) {
  data_subset_y <- z_y[ind[i], ]
  # Create histogram
  hist(data_subset_y, main = paste("Gene Rank:", i * 25, "%"), xlab = "Counts for Tibial Artery", col = "skyblue", border = "white")
}
par(mfrow = c(1,1))

# 12. The next few exercises will walk you through computing a t-test and p-value
# for the t-test. You have already computed the average difference. Now compute
# vectors sx and sy with the sample standard deviations for each gene in aorta
# and tibial respectively.

sx <- matrixStats::rowSds(z_x)
sy <- matrixStats::rowSds(z_y)

# 13. Determine the number of samples in aorta and tibial respectively and save
# in nx and ny.

nx <- ncol(z_x)
ny <- ncol(z_y)

# 14. Construct a t-test testing the difference in expression between aorta and
# tibial for each gene. Use matrix operations to do this.

# Calculate the standard error of the difference
se_diff <- sqrt((sx^2 / nx) + (sy^2 / ny))

# Construct the t-statistic
t_stat <- d / se_diff

# 15. Compute a p-value for each gene.

# Degrees of freedom
df <- nx + ny - 2

# Calculate the p-value
p_value <- 2 * pt(-abs(t_stat), df)

# 16. List the 10 genes with differences larger than 2 in absolute value and
# pvalues lower than the Bonferroni correction cutoff 0.05/nrow(gene_map). Do
# some Google searches of the top hits.

# Bonferroni correction cutoff
bonferroni_cutoff <- 0.05 / nrow(gene_map)

# Identify significant genes - gives logical vector
significant_genes <- p_value < bonferroni_cutoff

# Get indices of genes with differences larger than 2 in absolute value - logical vector
large_diff_indices <- abs(d) > 2

# Get indices of genes meeting both criteria - logical vector
selected_genes_indices <- significant_genes & large_diff_indices

# Sort genes based on absolute difference - numerical vector
sorted_indices <- order(abs(d), decreasing = TRUE)

# Select top 10 genes meeting both criteria
top_10 <- sorted_indices[selected_genes_indices][1:10]
top_10_genes <- gene_map[top_10, 3]

# Result:
# 1:       HSPA6: heat shock protein
# 2:         NRK: required for JNK activation
# 3:      TM6SF2: undetermined function
# 4:    IGHV4-34: autoreactive BCR
# 5:  AC013275.2: lncRNA
# 6:       SPARC: glycoprotein in bone that binds calcium
# 7:       STPG1: positive regulation of apoptosis
# 8:      VANGL2
# 9:      PCDH17: a protocadherin
# 10:      SLC6A6: transports taurine and beta-alanine

# 17. Earlier we noticed substantial variation across samples in the proportion
# of 0s and the total counts for each sample. We are going to explore possible
# batch effect with pca and see if any of the information about samples explains
# it. Read in the file "gtex/metadata/GTEx_Analysis_v8_Annotations_SampleAttributesDS.txt"
# and find the column that contains the column names of x.

pd <- fread("../gtex/metadata/GTEx_Analysis_v8_Annotations_SampleAttributesDS.txt")
## write a line of code that checks if colnames(x) are in pd$SAMPID
all(colnames(x) %in% pd$SAMPID)

# 18. Subset and reorder the sample information so that rows of sample information
# data match columns of x.

# Match sample IDs with column names of 'x'
ind <- match(colnames(x), pd$SAMPID)

# Subset and reorder sample information data
sample_info_reordered <- pd[ind, ]


# 19. Make boxplots of total counts for the samples by Center.

temp_df <- data.frame(SMCENTER = sample_info_reordered$SMCENTER, Total_Counts = colSums(x))

temp_df |> ggplot(aes(x = SMCENTER, y = Total_Counts, fill = SMCENTER)) +
  geom_boxplot() +
  labs(x = "Center", y = "Total Counts", title = "Boxplot of Total Counts by Center") +
  theme_minimal()

rm(temp_df)

# 20. Run the transformation vst on x then apply PCA. Plot the first two dimensions
# for each sample against each other with color representing center. Remember
# that x has samples in the columns, which is different than the material covered
# in the slides.

# Run vst transformation on 'x'
vst_x <- vst(x)

# Apply PCA
pca_result <- prcomp(t(vst_x))

# Create data frame for PCA results
pca_df <- data.frame(PC1 = pca_result$x[, 1],
                     PC2 = pca_result$x[, 2],
                     SMCENTER = sample_info_reordered$SMCENTER)

# Plot first two dimensions with color representing 'SMCENTER'
ggplot(pca_df, aes(x = PC1, y = PC2, color = SMCENTER)) +
  geom_point() +
  labs(x = "PC1", y = "PC2", color = "Center") +
  ggtitle("PCA Plot of vst Transformed Data x") +
  theme_minimal()

# 21. We notice that the second PC is associated with the center. Try it again
# but removing the average value from each sample to see that the effect is still observed

# Center the data by subtracting the mean of each sample
centered_data <- scale(t(vst_x), center = TRUE, scale = FALSE)

# Apply PCA
pca_result <- prcomp(centered_data)

# Create data frame for PCA results
pca_df <- data.frame(PC1 = pca_result$x[, 1],
                     PC2 = pca_result$x[, 2],
                     SMCENTER = sample_info_reordered$SMCENTER)

# Plot first two dimensions with color representing 'SMCENTER'
ggplot(pca_df, aes(x = PC1, y = PC2, color = SMCENTER)) +
  geom_point() +
  labs(x = "PC1", y = "PC2", color = "Center") +
  ggtitle("PCA Plot of vst Transformed Data x (Centered)") +
  theme_minimal()












