#data <- read.csv("support2.csv", header = TRUE, sep = ",")
#data_stage1 <- data

# following predictors should be removed, as they are findings from a previous model
#data[ , c("aps", "dnr", "dnrday", "prg2m", "prg6m", "sps", "surv2m", "surv6m")] <- list(NULL)

# Not relevant for our study:
# urine, sod and hrt are physiological measure, we decided to keep urine, because its easiest to interpret !!!! also removed urine, maybe rethink !!!!
# dzclass, dzgroup and ca has similar information, we will keep dzgroup
# Redundant or less impactful variables: 'diabetes', 'd.time', 'death', 'race' 'resp', 'sex', 'wblc'
#data[ , c("avtisst", "ca", "diabetes", "death", "d.time", "dzclass", "hrt", "race", "resp", "scoma", "sex", "sod", "urine", "wblc")] <- list(NULL)

# According to the HBiostat Repository (<https://hbiostat.org/data/repo/supportdesc>, Professor Frank Harrell) the following default values have been found to be useful in imputing missing baseline physiologic data:
#default_values <- c(alb = 3.5, pafi = 333.3, bili = 1.01, crea = 1.01, bun = 6.51)

#for (var in names(default_values)) {
#  data[[var]][is.na(data[[var]])] <- default_values[var]
#}

### Data Cleaning and Preprocessing
# After Inspecting closely, We see that for many categorical variables', 
# the datatype are not 'Factor'
# Identify and Group the categorical variables as categories of interest
cat_interest <- c("adlp", "adls", "dementia", "dzgroup", 
                  "hospdead", "income", "sfdm2")

# Apply unique function to validate categorical variables and check their structure
unique_values <- sapply(data[cat_interest], unique)

# convert categorical and ordinal variables to factors  # nolint
data$adlp <- factor(data$adlp, ordered = TRUE, levels = c(1, 2, 3, 4, 5, 6, 7))
data$adls <- factor(data$adls, ordered = TRUE, levels = c(1, 2, 3, 4, 5, 6, 7))
data$dementia <- factor(data$dementia)
data$dzgroup <- factor(data$dzgroup)
data$hospdead <- factor(data$hospdead)
data$income <- factor(data$income, ordered = TRUE, 
                      levels = c("under $11k", "$11-$25k", "$25-$50k", ">$50k"))
data$sfdm2 <- factor(data$sfdm2, ordered = TRUE, 
                     levels = c("no(M2 and SIP pres)", "adl>=4 (>=5 if sur)", 
                                "SIP>=30", "Coma or Intub", "<2 mo. follow-up"))

# Check structure of dataset to confirm changes
# str(data[cat_interest])

# MISSING VALUES ANALYSIS
# Count the total number of complete cases (Rows with no missing values)
#sum(is.na(data))
#dim(data)

# We have only 205 complete cases (rows with no missing values) out of 9105 rows. 
# This is a very small number of complete cases. In order to make the dataset usable,
# we will have to find identify which variables have the most missing values and drop them from the dataset.

# Check for Missing Values in each column and sort in descending order
#data %>% summarise_all(~ sum(is.na(.))) %>% stack() %>% subset(values > 0) %>% arrange(desc(values))

# Variables with the most missing values are:
# 1. adlp : 7490 (Too many missing values, and highly correlated with other 'Activities of Daily Living Index' variable (adlsc) 
# 2. adls : 5975 (same as above)
# 3. glucose : 4500
# 4. totmcst (Total micro cost): 3475
# 5. income : 2982

# To start, We will drop the variables with more than 65% missing values.
# These are: adlp, adls, glucose, totmcst

# Drop variables with high missing values from the list
data_stage2 <- data
data <- subset(data, select = -c(adlp, adls, glucose, totmcst))

# Recheck the total number of complete cases (Rows with no missing values) after
# dropping variables with high missing values
sum(complete.cases(data))

# Still, we have only 3490 complete cases. 

# To determine further, which variables to drop, 
# we will create a correlation matrix and identify the variables with high correlation.
# Create df with only numeric columns for creating a correlation matrix
numeric_df <- data[sapply(data, is.numeric)]

# Create a correlation matrix to visualize the correlation between the variables
#install.packages("stringr")
#library(corrplot)
#library(reshape2)

# Assuming df is your data frame
# Flatten and filter
top_correlations <- melt(cor(numeric_df, use = "complete.obs"))

# Remove self-correlations and duplicates
top_correlations <- subset(top_correlations, Var1 != Var2)
top_correlations <- top_correlations[!duplicated(t(apply(top_correlations[, 1:2], 1, sort))), ]

# Sort by absolute correlation value
top_correlations <- top_correlations[order(-abs(top_correlations$value)), ]
#print(head(top_correlations, 40))

# We find that the following variables are highly correlated with other variables:
# 1. totcst : charges (cost to charge ratio : total charges) - (Cor = 0.87)
# 2. totcst : slos (Cor = 0.78)
# 4. charges : slos (Cor = 0.62)
# 5. charges : hday (Cor = 0.51)

# Drop the column with the highest correlation (totcst)
# data <- subset(data, select = -c(totcst, sfdm2))
data <- subset(data, select = -c(totcst))

# Check complete cases
#sum(complete.cases(data))

# Check for Missing Values in data and sort in descending order
#data %>% summarise_all(~ sum(is.na(.))) %>% stack() %>% subset(values > 0) %>% arrange(desc(values))

### Imputing Missing Values
data_stage3 <- data
# Impute other variables:
# - income
# - edu
# - ph

# We will drop the 'rows' with missing values for the following variables:
# 1. charges
# 6. meanbp

# Impute missing values of the ph column with the mean value
data$ph[is.na(data$ph)] <- mean(data$ph, na.rm = TRUE)

# Impute the variable "income" with the mode value
# Check the frequency of each category in the variable "income"
# Define a Function to Calculate the Mode
getMode <- function(v) {
  uniqv <- unique(na.omit(v))
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Impute missing values with the mode value
data$income[is.na(data$income)] <- getMode(data$income)

# Impute missing values with the mode value
data$edu[is.na(data$edu)] <- getMode(data$edu)


# Check for Missing Values in data and sort in descending order
data %>% summarise_all(~ sum(is.na(.))) %>% stack() %>% subset(values > 0) %>% arrange(desc(values))

### Drop all the remaining rows with missing values.
# Subset all rows with complete cases
data <- data[complete.cases(data), ]
#sum(is.na(data))
#dim(data)
#sort(colnames(data))

