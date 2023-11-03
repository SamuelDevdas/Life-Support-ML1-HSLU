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

simple_miss_summary <- sapply(data, function(x) sum(is.na(x)))
print(data.frame(Variable = names(simple_miss_summary)Missing_Values = simple_miss_summary))


