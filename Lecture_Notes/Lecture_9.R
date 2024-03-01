# Genomics PCA exercise

library(data.table)
# Gene counts by tissue
# Have to skip 2 lines of this tsv file
fns1 <- "gene_reads_2017-06-05_v8_artery_aorta.gct"
fns2 <- "gene_reads_2017-06-05_v8_artery_tibial.gct"

df1 <- fread(fn1, skip = 2)
df2 <- fread(fn2, skip = 2)

# We get number of genes x number of samples
dim(df1)
dim(df2)

# Verify that genes are in the same order in both the data frames, e.g., gene 1
# in df1 is also gene 1 in df2
identical(df1$Name, df2$Name)

gene_map <- df1[, c("id", "Name", "Description")]
gene_map[1:5, ]

x <- as.matrix(df1[, c("id", "Name", "Description"):=NULL])
y <- as.matrix(df2[, c("id", "Name", "Description"):=NULL])

d <- rowMeans(x) - rowMeans(y)

a <- rowMeans(cbind(x, y))

hist(d)

hist(log10(a))

plot(log10(a), d)

library(matrixStats)
s <- rowSds(x)
a <- rowMeans(x)

plot(log10(a), log10(s))
abline(0, 1, col = "red")

round(colMeans(x == 0), 2)
# 50% of each gene are 0.


x <- log2(x+0.5)
y <- log2(y+0.5)
d <- rowMeans(y) - rowMeans(x)
a <- rowMeans(cbind(x,y))
plot(a, d)






