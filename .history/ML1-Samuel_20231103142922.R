# Load dataset:
df_support <- read.csv("support2.csv", header = TRUE, sep = ",")

# Inspect dataset
head(df_support)

View(df_support)

# Check structure of dataset
str(df_support)
dim(df_support)


# Check for Missing Values:
sum(is.na(df_support))

# Check for Missing Values in each column:
na_counts_row <- colSums(is.na(df_support))
na_counts 

# Check for Missing Values in each row:
na_counts_row <- rowSums(is.na(df_support))
na_counts_row

# Names of columns with missing values:
na_columns <- names(df_support)[which(na_counts > 0)]

# Print names of columns with missing values:
print(data.frame(Column = na_columns, Missing_Values = na_counts[na_columns]))

# Number of rows with missing values:
sum(complete.cases(df_support))

complete.cases(df_support)

x <- cbind(x1 = 3, x2 = c(4:1, 2:5))
rowSums(x); colSums(x)
dimnames(x)[[1]] <- letters[1:8]
rowSums(x); colSums(x); rowMeans(x); colMeans(x)
x[] <- as.integer(x)
rowSums(x); colSums(x)
