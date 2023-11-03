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

print(data.frame(Variable = names(simple_miss_summary),Missing_Values = simple_miss_summary))


# Function to count missing values
count_na <- function(x) {
  if (is.list(x) || is.data.frame(x)) {
    sum(unlist(lapply(x, is.na)))
  } else {
    sum(is.na(x))
  }
}

# Apply function to each column
simple_miss_summary <- sapply(data, count_na)

# Print result
print(data.frame(Variable = names(simple_miss_summary), Missing_Values = simple_miss_summary))
