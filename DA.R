# Inspect dataset
colnames(data)
head(data)

# Check structure of dataset
str(data)
dim(data)

# Group the categorical variables
cat_interest <- c("sex", "death","hospdead", "dzgroup", "dzclass", "income", "race", "diabetes", "dementia", "ca", "dnr", "adlp", "adls", "sfdm2")

# Apply unique function to validate categorical variables and check their structure
unique_values <- sapply(data[cat_interest], unique)
unique_values
str(unique_values)
#After Inspecting closely, We see that some categorical variables datatype are not factors eg. $ sex : chr  "male" "female" # nolint (? check if num.co is categorical or ordinal)

# convert categorical and ordinal variables to factors  # nolint
data$sex <- factor(data$sex)

data$death <- factor(data$death)

data$hospdead <- factor(data$hospdead)

data$dzgroup <- factor(data$dzgroup)

data$dzclass <- factor(data$dzclass)

data$income <- factor(data$income, ordered = TRUE, levels = c("under $11k","$11-$25k", "$25-$50k", ">$50k"))

data$race <- factor(data$race)

data$diabetes <- factor(data$diabetes)  

data$dementia <- factor(data$dementia)

factor(c("metastatic", "no", "yes"), 
                     levels = c("no", "yes", "metastatic"), 
                     ordered = TRUE)

data$adlp <- factor(data$adlp, ordered = TRUE, levels = c(1, 2, 3, 4, 5, 6, 7))

data$adls <- factor(data$adls, ordered = TRUE, levels = c(1, 2, 3, 4, 5, 6, 7))

data$sfdm2 <- factor(data$sfdm2, ordered = TRUE, levels = c("no(M2 and SIP pres)", "adl>=4 (>=5 if sur)", "SIP>=30", "Coma or Intub", "<2 mo. follow-up"))

# Check structure of dataset to confirm changes
str(data[cat_interest])

# MISSING VALUES ANALYSIS
# Check for Missing Values:
sum(is.na(data))

# Check for Missing Values in each column:
na_counts_col <- colSums(is.na(data))
na_counts_col

# Counting complete cases
num_complete_cases <- sum(complete.cases(data))
num_complete_cases




