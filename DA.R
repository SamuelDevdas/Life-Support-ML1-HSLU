# Group the categorical variables
cat_interest <- c("sex", "death","hospdead", "dzgroup", "dzclass", "income", "race", "diabetes", "dementia", "ca", "dnr", "adlp", "adls", "sfdm2")

# Apply unique function to validate categorical variables and check their structure
unique_values <- sapply(data[cat_interest], unique)

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
data$ca <- factor(c("metastatic", "no", "yes"), 
                  levels = c("no", "yes", "metastatic"), 
                  ordered = TRUE)
data$adlp <- factor(data$adlp, ordered = TRUE, levels = c(1, 2, 3, 4, 5, 6, 7))
data$adls <- factor(data$adls, ordered = TRUE, levels = c(1, 2, 3, 4, 5, 6, 7))
data$sfdm2 <- factor(data$sfdm2, ordered = TRUE, levels = c("no(M2 and SIP pres)", "adl>=4 (>=5 if sur)", "SIP>=30", "Coma or Intub", "<2 mo. follow-up"))

# Check structure of dataset to confirm changes
# str(data[cat_interest])

# MISSING VALUES ANALYSIS
# Check for Missing Values:
#sum(is.na(data))

# Count the total number of complete cases (Rows with no missing values)
# sum(complete.cases(data))

# We have only 106 complete cases (rows with no missing values) out of 9105 rows. This is a very small number of complete cases. In order to make the dataset usable, we will have to find identify which variables have the most missing values and drop them from the dataset.

# Check for Missing Values in each column and sort in descending order

#na_counts_col_1 <- colSums(is.na(data))
#na_counts_col_1 <- sort(na_counts_col, decreasing = TRUE) 
#==> stack(sort(colSums(is.na(data)), decreasing = TRUE))
#na_counts_col[1:15]


# Drop variables with high missing values from the list

# And 'column_to_drop' is the name of the column you want to drop
df_updated_mis <- subset(data, select = -c(adlp, adls, glucose))


# Recheck the total number of complete cases (Rows with no missing values) after dropping variables with high missing values
#sum(complete.cases(df_updated_mis))

# Create df with only numeric columns for creating a correlation matrix

numeric_columns <- sapply(df_updated_mis, is.numeric)
numeric_df <- df_updated_mis[numeric_columns]

# Assuming df is your data frame
correlation_matrix <- cor(numeric_df, use = "complete.obs")

# Flatten and filter
corr_melted <- melt(correlation_matrix)

# Remove self-correlations and duplicates
corr_melted <- subset(corr_melted, Var1 != Var2)
corr_melted <- corr_melted[!duplicated(t(apply(corr_melted[, 1:2], 1, sort))), ]

# Sort by absolute correlation value
corr_melted <- corr_melted[order(-abs(corr_melted$value)), ]
top_correlations <- head(corr_melted, 40)

#print(top_correlations)


# Missing values for prg2m, prg6m, surv2m, surv6m, totcst, charges, aps, sps, avtisst, sfdm2
# Check for Missing Values in each column and sort in descending order
na_counts_cor <- colSums(is.na(data[c("prg2m", "prg6m", "surv2m", "surv6m", "totcst", "charges", "aps", "sps", "avtisst", "sfdm2")]))
na_counts_cor <- sort(na_counts_cor, decreasing = TRUE)
#na_counts_cor


# We drop the 'redundant' and with 'high missing values' variables ie. prg2m, prg6m and surv6m, totcst, sps, avtisst
# And 'column_to_drop' is the name of the column you want to drop
df_updated_mis <- subset(df_updated_mis, select = -c(prg2m, prg6m, surv6m, totcst, sps, avtisst, sfdm2))

# Check complete cases
#num_complete_cases <- sum(complete.cases(df_updated_mis))

# Check for Missing Values in df_updated_mis and sort in descending order
na_counts_col <- colSums(is.na(df_updated_mis))
na_counts_col <- sort(na_counts_col, decreasing = TRUE)
# na_counts_col[1:15]

###Imputing Missing Values
# Impute the variable "ph" with mean value
# Calculate the mean for the ph Column
mean_ph <- mean(df_updated_mis$ph, na.rm = TRUE)

# Impute missing values with the mean value
df_updated_mis$ph[is.na(df_updated_mis$ph)] <- mean_ph

# Impute the variable "income" with the mode value
# Check the frequency of each category in the variable "income"
# Define a Function to Calculate the Mode

getMode <- function(v) {
  uniqv <- unique(na.omit(v))
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Calculate the Mode for the Income Column
mode_value <- getMode(df_updated_mis$income)

# Impute missing values with the mode value
df_updated_mis$income[is.na(df_updated_mis$income)] <- mode_value

# Impute the variable "edu" with the mode value
# Calculate the Mode for the edu Column
mode_value <- getMode(df_updated_mis$edu)

# Impute missing values with the mode value
df_updated_mis$edu[is.na(df_updated_mis$edu)] <- mode_value


# Check for Missing Values in df_updated_mis and sort in descending order

na_counts_col <- colSums(is.na(df_updated_mis))

na_counts_col <- sort(na_counts_col, decreasing = TRUE)

na_counts_col[1:15]
