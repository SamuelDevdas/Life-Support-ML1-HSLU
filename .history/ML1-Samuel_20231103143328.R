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


# Assuming df_support is your data frame
complete_rows <- complete.cases(df_support)

# To get the data frame with complete cases only
df_complete <- df_support[complete_rows, ]
df_complete

# Counting complete cases
num_complete_cases <- sum(complete.cases(df_support))
num_complete_cases
