### Data Loading and Inspection
# Load dataset:
df_support <- read.csv("support2.csv", header = TRUE, sep = ",")

# df_description <- read.csv("support-variables-description.csv", header = TRUE, sep = ",")

# Inspect dataset
head(df_support, 3)

# Check structure of dataset
str(df_support)
dim(df_support)

### After Inspecting closely, We see that for many categorical variables', the datatype are not 'Factor' eg. $ sex : chr  "male" "female" etc.
### Data Cleaning and Preprocessing
# Identify and Group the categorical variables as categories of interest
cat_interest <- c("death", "sex", "hospdead", "dzgroup", "dzclass", "income", "race", "diabetes", "dementia", "ca", "dnr", "adlp", "adls", "sfdm2")

# Apply unique function to validate that the cat_interest have unique categorical values and check their structure
unique_values <- sapply(df_support[cat_interest], unique)

str(unique_values)

# convert categorical and ordinal variables to factors  # nolint
df_support$sex <- factor(df_support$sex)

df_support$death <- factor(df_support$death)

df_support$hospdead <- factor(df_support$hospdead)

df_support$dzgroup <- factor(df_support$dzgroup)

df_support$dzclass <- factor(df_support$dzclass)

df_support$income <- factor(df_support$income, ordered = TRUE, levels = c("under $11k", "$11-$25k", "$25-$50k", ">$50k"))

df_support$race <- factor(df_support$race)

df_support$diabetes <- factor(df_support$diabetes)

df_support$dementia <- factor(df_support$dementia)

df_support$ca <- factor(c("metastatic", "no", "yes"),
    levels = c("no", "yes", "metastatic"),
    ordered = TRUE
)

df_support$adlp <- factor(df_support$adlp, ordered = TRUE, levels = c(1, 2, 3, 4, 5, 6, 7))

df_support$adls <- factor(df_support$adls, ordered = TRUE, levels = c(1, 2, 3, 4, 5, 6, 7))

df_support$sfdm2 <- factor(df_support$sfdm2, ordered = TRUE, levels = c("no(M2 and SIP pres)", "adl>=4 (>=5 if sur)", "SIP>=30", "Coma or Intub", "<2 mo. follow-up"))

# Check structure of dataset to confirm changes
str(df_support[cat_interest])

### MISSING VALUES ANALYSIS
# Count the total number of complete cases (Rows with no missing values)
num_complete_cases <- sum(complete.cases(df_support))
num_complete_cases

dim(df_support)

# We have only 106 complete cases (rows with no missing values) out of 9105 rows. This is a very small number of complete cases. In order to make the dataset usable, we will have to find identify which variables have the most missing values and drop them from the dataset.

# Check for Missing Values in each column and sort in descending order

na_counts_col <- colSums(is.na(df_support))

na_counts_col <- sort(na_counts_col, decreasing = TRUE)

na_counts_col[1:15]

# Variables with the most missing values are:
# 1. adlp : 7490 (Too many missing values, and highly correlated with other 'Activities of Daily Living Index' variable (adlsc) 
# 2. adls : 7490 (same as above)
# 3. urine : 4862 (To be imputed)
# 4. glucose : 4500
# 5. bun : 4352 (To be imputed)
# 6. totmcst (Total micro cost): 3475 (also, Highly correlated with totcst: total cost)


# To start, We will drop the variables with more than 50% missing values. These are:
# 1. adlp, adls, totmcst, glucose.


# Drop variables with high missing values from the list

# And 'column_to_drop' is the name of the column you want to drop
df_updated_mis <- subset(df_support, select = -c(adlp, adls, totmcst, glucose))

# Recheck the total number of complete cases (Rows with no missing values) after dropping variables with high missing values
num_complete_cases <- sum(complete.cases(df_updated_mis))
num_complete_cases

# Still, we have only 857 complete cases. 

# To determine further, which variables to drop, we will create a correlation matrix and identify the variables with high correlation.
# Create df with only numeric columns for creating a correlation matrix

numeric_columns <- sapply(df_support, is.numeric)
numeric_df <- df_support[numeric_columns]

# Create a correlation matrix to visualize the correlation between the variables
#install.packages("stringr")
library(corrplot)
library(reshape2)

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

print(top_correlations)

# We find that the following variables are highly correlated with other variables:
# 1. sur6m : surv2m (model predicted 6 months and 2 months survival estimates for patient ) - (Cor = 0.96)
# 2. totcst : charges (cost to charge ratio : total charges) - (Cor = 0.83)
# 3. aps (APACHE III day 3 physiology score : SUPPORT physiology score) : sps (predicted SUPPORT physiology score on day 3 )  - (Cor = 0.77)
# 4. aps : avtisst (Average TISS score - quantifies type and number of intensive care treatments. ) - (Cor = 0.59)
# 5. sfdm2 (Level of functional disability of the patient in a 1-5 scale. (Naturally correlated with other physiological scores ))

# Identify out which other variables are also Survival estimates and check their respective number of missing values.
# 1. prg2m : Physician’s 2-month survival estimate for patient.
# 2. prg6m : Physician’s 6-month survival estimate for patient.
# 3. prg2m : surv2m - (Cor = 0.55)
# 4. prg6m : surv6m - (Cor = 0.53)

# Missing values for prg2m, prg6m, surv2m, surv6m, totcst, charges, aps, sps, avtisst, sfdm2
# Check for Missing Values in each column and sort in descending order

na_counts_cor <- colSums(is.na(df_support[c("prg2m", "prg6m", "surv2m", "surv6m", "totcst", "charges", "aps", "sps", "avtisst", "sfdm2")]))

na_counts_cor <- sort(na_counts_cor, decreasing = TRUE)

na_counts_cor

# We drop the 'redundant' and with 'high missing values' variables ie. prg2m, prg6m and surv6m, totcst, sps, avtisst
# And 'column_to_drop' is the name of the column you want to drop

df_updated_mis <- subset(df_updated_mis, select = -c(prg2m, prg6m, surv6m, totcst, sps, avtisst, sfdm2))

# Check complete cases
num_complete_cases <- sum(complete.cases(df_updated_mis))
num_complete_cases

# Check for Missing Values in df_updated_mis and sort in descending order

na_counts_col <- colSums(is.na(df_updated_mis))

na_counts_col <- sort(na_counts_col, decreasing = TRUE)

na_counts_col[1:15]

### Imputing Missing Values

# According to the HBiostat Repository (https://hbiostat.org/data/repo/supportdesc, Professor Frank Harrell) the following default values have been found to be useful in imputing missing baseline physiologic data:
# Baseline Variable	Normal Fill-in Value
# - Serum albumin (alb)	3.5
# - PaO2/FiO2 ratio (pafi) 	333.3
# - Bilirubin (bili)	1.01
# - Creatinine (crea)	1.01
# - bun	6.51
# - White blood count (wblc)	9
# - Urine output (urine)	2502

# Impute other variables:
# - income
# - edu
# - ph

# We will drop the 'rows' with missing values for the following variables:
# 1. charges
# 2. dnrday
# 3. scoma
# 4. aps
# 5. surv2m
# 6. meanbp
# 7. hrt

# Create a named vector with imputation values for each variable
impute_values <- c(
    alb = 3.5, pafi = 333.3, bili = 1.01,
    crea = 1.01, bun = 6.51, wblc = 9.0, urine = 2502
)

# Loop through the vector to impute values
for (var in names(impute_values)) {
    df_updated_mis[[var]][is.na(df_updated_mis[[var]])] <- impute_values[var]
}


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

### Drop all the remaining rows with missing values.
# Subset all rows with complete cases

final_df <- df_updated_mis[complete.cases(df_updated_mis), ]

sum(is.na(final_df))

dim(final_df)

sort(colnames(final_df))

# Drop the variables that are findings from the previous model as per Datasource repository

final_df <- subset(final_df, select = -c(
    aps,
    dnr,
    dnrday,
    surv2m
))

# Check
sort(colnames(final_df))

#################### Exploratory Data Analysis#####################
str(final_df)

head(final_df)


# Create scatterplots for  charges variable against all the numeric variables one by one

# Check the distribution of charges
hist(final_df$charges, breaks = 50, main = "Histogram of Charges", xlab = "Charges", col = "blue", border = "black")

# The "charges" variable is highly right skewed with majority <100000 

# Take LOG of charges to Linearize relationships
final_df$log_charges <- log(final_df$charges)

# To handle cases where 'charges' might be 0 or negative, you might want to add a small constant:
final_df$log_charges <- log(final_df$charges + 1)


numeric_vars <- names(final_df)[sapply(final_df, is.numeric)]
numeric_vars

################## for LOG charges#####################
#Exclude 'log_charges' from Numeric Columns
numeric_vars <- numeric_vars[numeric_vars != "log_charges"]
numeric_vars

# Loop through each numeric variable and create a scatterplot against 'charges'
for (var in numeric_vars) {
  print(
    ggplot(final_df, aes_string(x = var, y = "log_charges")) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +  # Add a linear regression line
      labs(title = paste("Scatterplot of LOG_charges vs", var), x = var, y = "charges")
  )
}

# From the scatterplots of all variables against 'charges',
# some of the interesting relationships are:
# Positive: slos,edu,scoma, hday,wblc, hrt,temp, bili, crea, bun
# Negative: age, num.co,  pafi, alb

####################################################################
#Predicting Charges for Life Support Patients: A Critical Aspect of Healthcare Management

#Accurate prediction of charges for life support patients is crucial for:

#Financial Management: Aids healthcare providers and patients in budgeting and resource allocation.
#Insurance Processing: Informs patients about potential expenses for insurance and out-of-pocket costs.
#Cost-Effectiveness: Supports decision-making regarding the viability and sustainability of life support treatments.
#Policy and Research: Guides healthcare policy decisions and economic studies, impacting overall healthcare quality and accessibility.
#Effective charge prediction is vital for balancing economic considerations with ethical and patient-centered care in life support scenarios.
#####################################################################

# WHICH ARE THE MOST IMPORTANT VARIABLES ON WHICH 'log_charges' DEPEND UPON? 
# LINEAR MODEL FOR RELATIONSHIP BETWEEN CHARGES AND OTHER VARIABLES

#1. Fitting the starting model using all the numeric variables except charges

numeric_columns <- sapply(final_df, is.numeric)
numeric_df <- subset(final_df[numeric_columns], select = -charges) 

lm.charges1 <- lm(log_charges~ ., data = numeric_df)
summary(lm.charges1)

# We test to see which variables can be dropped:
drop1(lm.charges1, test = "F")

# Based on high p-values indicating statistical insignificance, 
#the variables wblc, sod, and adlsc are candidates for removal
# Including mildly significant variables, resp (p = 0.01321), urine (p = 0.04144), alb (0.011)

# Updated model without the mildly significant variables to simplify the model
lm.charges2 <- update(lm.charges1, . ~ . - wblc - sod - adlsc - resp - urine -alb)
summary(lm.charges2)

# We test to see which variables can be dropped:
drop1(lm.charges2, test = "F")


# Backward selection
stepwise_model_backward <- step(lm.charges2, direction = "backward")









