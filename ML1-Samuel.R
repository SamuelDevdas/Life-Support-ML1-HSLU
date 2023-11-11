# Load dataset:
df_support <- read.csv("support2.csv", header = TRUE, sep = ",")

df_description <- read.csv("support-variables-description.csv", header = TRUE, sep = ",")

# Inspect dataset
head(df_support)
View(df_support)
View(df_description)

# Check structure of dataset
str(df_support)
dim(df_support)

# Group the categorical variables
cat_interest <- c("sex", "death","hospdead", "dzgroup", "dzclass", "income", "race", "diabetes", "dementia", "ca", "dnr", "adlp", "adls", "sfdm2")

# Apply unique function to validate categorical variables and check their structure
unique_values <- sapply(df_support[cat_interest], unique)
unique_values
str(unique_values)
#After Inspecting closely, We see that some categorical variables datatype are not factors eg. $ sex : chr  "male" "female" # nolint (? check if num.co is categorical or ordinal)

# convert categorical and ordinal variables to factors  # nolint
df_support$sex <- factor(df_support$sex)

df_support$death <- factor(df_support$death)

df_support$hospdead <- factor(df_support$hospdead)

df_support$dzgroup <- factor(df_support$dzgroup)

df_support$dzclass <- factor(df_support$dzclass)

df_support$income <- factor(df_support$income, ordered = TRUE, levels = c("under $11k","$11-$25k", "$25-$50k", ">$50k"))

df_support$race <- factor(df_support$race)

df_support$diabetes <- factor(df_support$diabetes)  

df_support$dementia <- factor(df_support$dementia)

factor(c("metastatic", "no", "yes"), 
                     levels = c("no", "yes", "metastatic"), 
                     ordered = TRUE)

df_support$adlp <- factor(df_support$adlp, ordered = TRUE, levels = c(1, 2, 3, 4, 5, 6, 7))

df_support$adls <- factor(df_support$adls, ordered = TRUE, levels = c(1, 2, 3, 4, 5, 6, 7))

df_support$sfdm2 <- factor(df_support$sfdm2, ordered = TRUE, levels = c("no(M2 and SIP pres)", "adl>=4 (>=5 if sur)", "SIP>=30", "Coma or Intub", "<2 mo. follow-up"))

# Check structure of dataset to confirm changes
str(df_support[cat_interest])

# MISSING VALUES ANALYSIS
# Check for Missing Values:
sum(is.na(df_support))

# Check for Missing Values in each column:
na_counts_col <- colSums(is.na(df_support))
na_counts_col

# Counting complete cases
num_complete_cases <- sum(complete.cases(df_support))
num_complete_cases




