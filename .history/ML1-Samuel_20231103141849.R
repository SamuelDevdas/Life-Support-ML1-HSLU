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

# ``
na_counts <- colSums(is.na(df_support))
na_counts 

na_columns <- names(df_support)[which(na_counts > 0)]

print(data.frame(Column = na_columns, Missing_Values = na_counts[na_columns]))





