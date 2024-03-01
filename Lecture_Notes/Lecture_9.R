# Genomics PCA exercise
# Author: Suparna Kumar

# 1. Import these files gtex/gene_read_counts_by_tissue/gene_reads_2017-06-05_v8_artery_aorta.gct
# and gtex/gene_read_counts_by_tissue/gene_reads_2017-06-05_v8_artery_tibial.gct
# into two data frames. Hint: consider using the data.table function fread.

# setwd()

library(data.table)
# Gene counts by tissue
# Have to skip 2 lines of this tsv file
fn1 <- "gene_reads_2017-06-05_v8_artery_aorta.gct"
fn2 <- "gene_reads_2017-06-05_v8_artery_tibial.gct"

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
gene_map[1:5, ]

# 4. Remove the columns related to gene identification from both tables and
# create two numeric matrics x and y. Once you create the two matrices, remove
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
x <- log2(x+0.5)
y <- log2(y+0.5)
#d <- rowMeans(y) - rowMeans(x)
d <- rowMeans(x) - rowMeans(y)

# 6. Compute the average across all samples (combine x and y) and keep it in a.
# Make a histogram of d, a histogram of a, and plot d versus a. Use log
# transformations where you deem necessary. Comment on what you observe

a <- rowMeans(cbind(x, y))
par(mfrow=c(2,2)) ## this puts R base plot in 2 x 2 grid
hist(d, nc = 100)
hist(a)
plot(a, d)

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

### make a temporary data frame to then use wiht ggplot
df <- data.frame(zeros = c(colMeans(x == 0), colMeans(y == 0)),
                 coverage = c(colSums(x), colSums(y)),
                 tissue = c(rep("Aorta", ncol(x)), rep("Tibial", ncol(y))))
## Now use ggplot
# df |> 
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


# 10. A popular approach used for RNA-Seq count data is to model the data as a
# negative binomial distribution (similar to Poisson, but allows for different
# variances). Under this assumption we can find a transformation that removes
# the dependence of variance on mean and can handle 0s. The transformation ends
# up being similar to the log except for smaller values. Run the transformation
# vst from the DESeq2 package on both x and y like this z <- DESeq2::vst(cbind(x,y)).
# Then compute the average difference d and average across the combined data a
# and plot these against each other.

## we can combine the matrices into one matric then apply vst
library(DESeq2)
z <- vst(cbind(x,y))
dim(z)
## we then the right column indexes
## we can then subset z to get what we want
ind_x <- 1:ncol(x)
ind_y <- (ncol(x) + 1):(ncol(x) + ncol(y))


# 11. Remake the histograms from exercise 9. Note that, at least for the higher
# expressed genes, the data looks closer to normally distributed.



# 12. The next few exercises will walk you through computing a t-test and p-value
# for the t-test. You have already computed the average difference. Now compute
# vectors sx and sy with the sample standard deviations for each gene in aorta
# and tibial respectively.



# 13. Determine the number of samples in aorta and tibial respectively and save
# in nx and ny.



# 14. Construct a t-test testing the difference in expression between aorta and
# tivial for each gene. Use matrix operations to do this.



# 15. Compute a p-value for each gene.



# 16. List the 10 genes with differences larger than 2 in absolute value and
# pvalues lower than the Bonferroni correction cutoff 0.05/nrow(gene_map). Do
# some Google searches of the top hits.



# 17. Earlier we noticed substantial variation across samples in the proportion
# of 0s and the total counts for each sample. We are going to explore possible
# batch effect with pca and see if any of the information about samples explains
# it. Read in the file "gtex/metadata/GTEx_Analysis_v8_Annotations_SampleAttributesDS.txt"
# and find the column that contains the column names of x.

pd <- fread("/course/data/gtex/metadata/GTEx_Analysis_v8_Annotations_SampleAttributesDS.txt")
## wrtie a line of code that checks if colnames(x) are in pd$SAMPID


# 18. Subset and reorder the sample information so that rows of sample information
# data fromm match columns of x.

## The following index gives us the ent
ind <- match(colnames(x), pd$SAMPID)

# 19. Make boxplots of total counts for the samples by Center.



# 20. Run the transformation vst on x then apply PCA. Plot the first two dimensions
# for each sample against each other with color representing center. Remember
# that x has samples in the columns, which is different than the material covered
# in the slides.



# 21. We notice that the second PC is associated with the center. Try it again
# but removing the average value from each sample to see that the effect is still observed














