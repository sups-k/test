# Final PSET
# Author: Suparna Kumar

# Your task is to build a machine learning algorithm that can predict tissue type
# from gene expression data. To do this we will use the Genotype-Tissue Expression
# (GTEx) project data. These data were collected across several centers. You will
# train on the data was processed in center B1 and will report the accuracy you
# obtain in the other centers. In cases where other centers include a tissue not
# represented in B1, you can exclude it from the testing. You can only use B1 to train.

# We start with some data wrangling and provide some guidance. However once the
# data is organized you will be on your own designing an algorithm.

# To accomplish this project you will be using all the skills you used during the course.

####--------Data and metadata files-------------######

# We have downloaded files from the GTEX project that include both gene expression
# counts obtained from thousands of samples processed in RNA-Seq experiments and a
# sample annotation file describing each sample. Exploring the files on our Unix
# system we can start to understand how the data is organized explore the data:

list.files("/course/data/gtex/metadata")
# GTEx_Analysis_v8_Annotations_SampleAttributesDD.xlsx, GTEx_Analysis_v8_Annotations_SampleAttributesDS.txt
# GTEx_Analysis_v8_Annotations_SubjectPhenotypesDD.xlsx, GTEx_Analysis_v8_Annotations_SubjectPhenotypesDS.txt


# 1. Read the sample annotation files using the fread function from data.table as
# it is very fast. Call the object pd for phenotype data.

library(data.table)

# Read phenotype data
pd <- fread("/course/data/gtex/metadata/GTEx_Analysis_v8_Annotations_SampleAttributesDS.txt")

# 2. Read the annotation on subjects and join them with the sample annotation.
# Note subject might contribute more than one sample:

# Read subject data
subjects <- fread("/course/data/gtex/metadata/GTEx_Analysis_v8_Annotations_SubjectPhenotypesDS.txt")

# 3. To connect them we need to extract the subject ID from the sample ID in pd.
# Add a new variable to the pd data frame called SUBJID with the subject ID.

# Takes a pattern & subs with another
pd$SUBJID <- gsub("(\\w*-\\w)-.*", "\\1", pd$SAMPID)
# pd <- left_join(pd, subjects, by = "SUBJID")
pd <- merge(pd, subjects, by = "SUBJID", all.x = TRUE)
# For right join, all.y = T, for full join, all = T

# 4. The expression data is stored in gct files

list.files("/course/data/gtex/gene_read_counts_by_tissue")
# Each tissue has its own gct (like csv) file
# Rows in metadata are columns here

## If you look at these, say with less, you will notice that the first two lines
## are information about the files and then its a tab delimited file. We also
## notice that the first three columns are a row id, a gene id, and a gene symbol.

# Read the files in one by one. Because we know we will be merging them, remove
# the gene id and gene symbol. We will later create a table to match ids to genes.
# Read all those files in:

fns <- list.files("/course/data/gtex/gene_read_counts_by_tissue", full.name = TRUE)
tables <- lapply(fns, function(fn){
  cat("Reading file ", fn, "\n")
  x <- fread(fn, skip = 2)
  #  return(x[,-c(1,2)])
  return(as.matrix(x[, c("id", "Name", "Description") :=NULL])) # keep ID if you're going to merge
})

# 5. Now we are going to join all those tables into one big one. We will make
# sure that the rows match by using merge (data.table's join). This is overkill
# here because the GTEX project made sure that the rows were ordered the same way,
# but we will merge by id just to be sure. Hint: use the Reduce function.

# x <- Reduce(function(x,y), merge(x,y,by="id", all+TRUE)) # Staying safe because what if the rows are not in the same order?
x <- do.call(cbind, tables)
# In machine learning, observations are stored in rows
# In genomics, observations (samples) are in columns and genes are in rows

# Check if all the column names in the files are in the metadata
all(colnames(x) %in% pd$SAMPID)
# If output is FALSE, check if there were accidental capital or small letters, dots instead of dash, prefix, other formatting issues
# Otherwise exclude the sample from the analysis

# 6. Create a data frame with just the gene information.

pd <- pd[match(colnames(x), pd$SAMPID),]

# 7. Make sure the pd table is in the same order as the columns of x.

x[ ,which(pd$SMTSD == "Adipose - Subcutaneous")]
table(pd$SMCENTER) # Use B1 to train and test on C1
train_x <- x[, pd$SMCENTER=="B1"] # Have to take transpose of x too

# lda computes centres for each tissue

# 8. Now that everything is in the right order, remove the id from x so we can
# create a numeric matrix.


######----------Predict tissue type----------##########

# You now have the predictors in x and all the sample information in pd. Divide
# both x and pd into training and test. Build your algorithm on the B1 center
# and show your accuracy in the rest.



