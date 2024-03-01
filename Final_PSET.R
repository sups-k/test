list.files("/course/data/gtex/metadata")
# GTEx_Analysis_v8_Annotations_SampleAttributesDD.xlsx, GTEx_Analysis_v8_Annotations_SampleAttributesDS.txt
# GTEx_Analysis_v8_Annotations_SubjectPhenotypesDD.xlsx, GTEx_Analysis_v8_Annotations_SubjectPhenotypesDS.txt

library(data.table)

# Read phenotype data
pd <- fread("/course/data/gtex/metadata/GTEx_Analysis_v8_Annotations_SampleAttributesDS.txt")

# Read subject data
subjects <- fread("/course/data/gtex/metadata/GTEx_Analysis_v8_Annotations_SubjectPhenotypesDS.txt")

# Takes a pattern & subs with another
pd$SUBJID <- gsub("(\\w*-\\w)-.*", "\\1", pd$SAMPID)
# pd <- left_join(pd, subjects, by = "SUBJID")
pd <- merge(pd, subjects, by = "SUBJID", all.x = TRUE)
# For right join, all.y = T, for full join, all = T

list.files("/course/data/gtex/gene_read_counts_by_tissue")
# Each tissue has its own gct (like csv) file
# Rows in metadata are columns here

fns <- list.files("/course/data/gtex/gene_read_counts_by_tissue", full.name = TRUE)
tables <- lapply(fns, function(fn){
  cat("Reading file ", fn, "\n")
  x <- fread(fn, skip = 2)
#  return(x[,-c(1,2)])
  return(as.matrix(x[, c("id", "Name", "Description") :=NULL])) # keep ID if you're going to merge
})

# x <- Reduce(function(x,y), merge(x,y,by="id", all+TRUE)) # Staying safe because what if the rows are not in the same order?
x <- do.call(cbind, tables)

# In machine learning, observations are stored in rows
# In genomics, observations (samples) are in columns and genes are in rows

# Check if all the column names in the files are in the metadata
all(colnames(x) %in% pd$SAMPID)
# If output is FALSE, check if there were accidental capital or small letters, dots instead of dash, prefix, other formatting issues
# Otherwise exclude the sample from the analysis

# Make data frame with only gene information
pd <- pd[match(colnames(x), pd$SAMPID),]

x[ ,which(pd$SMTSD == "Adipose - Subcutaneous")]
table(pd$SMCENTER) # Use B1 to train and test on C1
train_x <- x[, pd$SMCENTER=="B1"] # Have to take transpose of x too

# lda computes centres for each tissue


