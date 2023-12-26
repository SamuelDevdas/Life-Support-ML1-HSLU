### Data Loading and Inspection
# Load dataset:
df_support <- read.csv("support2.csv", header = TRUE, sep = ",")

# df_description <- read.csv("support-variables-description.csv", header = TRUE, sep = ",")

# Inspect dataset
head(df_support, 3)

# Check structure of dataset
str(df_support)
dim(df_support)

### After Inspecting closely, We see that for many categorical variables', 
# the datatype are not 'Factor' eg. $ sex : chr  "male" "female" etc.

### Data Cleaning and Preprocessing
# Identify and Group the categorical variables as categories of interest
cat_interest <- c("sex", "hospdead", "dzgroup", "dzclass",
                  "income", "race", "diabetes", "dementia", "ca", "dnr", "adlp", "adls", "sfdm2")

# Apply unique function to validate that the cat_interest have unique categorical values and check their structure
unique_values <- sapply(df_support[cat_interest], unique)

str(unique_values)

# convert categorical and ordinal variables to factors  # nolint
df_support$sex <- factor(df_support$sex)

df_support$hospdead <- factor(df_support$hospdead)

df_support$dzgroup <- factor(df_support$dzgroup)

df_support$dzclass <- factor(df_support$dzclass)

df_support$income <- factor(df_support$income, ordered = TRUE, 
                            levels = c("under $11k", "$11-$25k", "$25-$50k", ">$50k"))

df_support$race <- factor(df_support$race)

df_support$diabetes <- factor(df_support$diabetes)

df_support$dementia <- factor(df_support$dementia)

df_support$ca <- factor(c("metastatic", "no", "yes"),
    levels = c("no", "yes", "metastatic"),
    ordered = TRUE
)

df_support$adlp <- factor(df_support$adlp, ordered = TRUE, levels = c(1, 2, 3, 4, 5, 6, 7))

df_support$adls <- factor(df_support$adls, ordered = TRUE, levels = c(1, 2, 3, 4, 5, 6, 7))

df_support$sfdm2 <- factor(df_support$sfdm2, ordered = TRUE, 
                           levels = c("no(M2 and SIP pres)", "adl>=4 (>=5 if sur)", 
                                      "SIP>=30", "Coma or Intub", "<2 mo. follow-up"))

# Check structure of dataset to confirm changes
str(df_support[cat_interest])

### MISSING VALUES ANALYSIS
# Count the total number of complete cases (Rows with no missing values)
num_complete_cases <- sum(complete.cases(df_support))
num_complete_cases

dim(df_support)

# We have only 106 complete cases (rows with no missing values) out of 9105 rows. 
# This is a very small number of complete cases. In order to make the dataset usable,
# we will have to find identify which variables have the most missing values and drop them from the dataset.

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

# Recheck the total number of complete cases (Rows with no missing values) after
# dropping variables with high missing values
num_complete_cases <- sum(complete.cases(df_updated_mis))
num_complete_cases

# Still, we have only 857 complete cases. 

# To determine further, which variables to drop, 
# we will create a correlation matrix and identify the variables with high correlation.
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

# We drop the 'redundant' and with 'high missing values' variables 
# ie. prg2m, prg6m and surv6m, totcst, sps, avtisst
# And 'column_to_drop' is the name of the column you want to drop

df_updated_mis <- subset(df_updated_mis, select = -c(death,prg2m, prg6m, surv6m, totcst, sps, avtisst, sfdm2))

# Check complete cases
num_complete_cases <- sum(complete.cases(df_updated_mis))
num_complete_cases

# Check for Missing Values in df_updated_mis and sort in descending order

na_counts_col <- colSums(is.na(df_updated_mis))

na_counts_col <- sort(na_counts_col, decreasing = TRUE)

na_counts_col[1:15]

### Imputing Missing Values

# According to the HBiostat Repository (https://hbiostat.org/data/repo/supportdesc, 
# Professor Frank Harrell) the following default values have been found to be useful in imputing missing baseline physiologic data:
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
# To find out if linear relationship exists with some variables

# Check the distribution of charges
hist(final_df$charges, breaks = 50, main = "Histogram of Charges", xlab = "Charges", col = "blue", border = "black")

# The "charges" variable is highly right skewed with majority <100000 

# Take LOG of charges to Linearize relationships
final_df$log_charges <- log(final_df$charges)

# To handle cases where 'charges' might be 0 or negative, 
# you might want to add a small constant:
final_df$log_charges <- log(final_df$charges + 1)

# Check the distribution of log_charges
hist(final_df$log_charges, breaks = 50, 
     main = "Histogram of log_Charges", 
     xlab = "Charges", col = "blue", border = "black")
# Now the the charges data look more normally distributed

# Make data ready for plotting
numeric_vars <- names(final_df)[sapply(final_df, is.numeric)]
numeric_vars

#Exclude 'log_charges' from Numeric Columns

numeric_vars <- numeric_vars[!(numeric_vars %in% c("log_charges", "charges"))]

numeric_vars

# Loop through each numeric variable and create a scatterplot against 'log_charges'
library(ggplot2)
for (var in numeric_vars) {
  print(
    ggplot(final_df, aes_string(x = var, y = "log_charges")) +
      geom_point(shape = 19, size = 1, alpha = 0.5, color = "blue") +
      geom_smooth(method = "lm", se = FALSE, color = "red") +  # Add a linear regression line
      labs(title = paste("Scatterplot of log_charges vs", var), x = var, y = "log_charges")
  )
}

# From the scatterplots of all variables against 'charges',
# some of the interesting relationships are:
# Positive: slos,edu,scoma, hday,wblc, hrt,temp, bili, crea, bun
# Negative: age, num.co,  pafi

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

#1. Fitting the starting model using all the variables except 'charges'

#numeric_columns <- sapply(final_df, is.numeric)
full_df <- subset(final_df, select = -charges) 

lm.charges1 <- lm(log_charges~ ., data = full_df)
summary(lm.charges1)

# Linear Model Summary: lm.charges3 on full_df
# ----------------------------------------------------
# Adjusted R-squared: 0.6849 - Model explains ~68.64% of variability
# F-statistic highly significant - Strong overall model predictive power

# Significant Predictors Include: age, death1, hospdead1, slos, hday,
# various 'dzgroup' categories, edu, num.co, and health indicators.

# Issues to Address:
# - Multicollinearity: Remove 'dzclass' categories (NA coefficients)
# - Remove less significant predictors: sexmale, diabetes1, wblc, crea, sod

# Next Steps:
# - Refine model by addressing multicollinearity and removing non-significant variables
# - Conduct diagnostic checks for model assumptions
# ----------------------------------------------------
# Based on high p-values indicating statistical insignificance, 
#the variables wblc, sod, and adlsc are candidates for removal
# Including mildly significant variables, resp (p = 0.01321), urine (p = 0.04144), alb (0.011)

# We test to see which variables can be dropped:
drop1(lm.charges1, test = "F")

# Updating lm.charges1 Model:
# ----------------------------------------------------
# Based on drop1() analysis, the following variables are identified for removal due to their 
# minimal impact on model AIC and lack of statistical significance:
# - 'sex', 'diabetes', 'crea', 'sod', 'wblc': High p-values.
# - 'dzclass', 'ca': High p-values or multicollinearity issues (NA coefficients).
# Removal of these variables aims at model simplification without substantially impacting model performance.
# The updated model (lm.charges2) will be reassessed for performance and adherence to regression assumptions.
# ----------------------------------------------------
# Update the model by excluding identified variables
lm.charges2 <- update(lm.charges1, . ~ . - sex - diabetes - crea - sod - wblc - dzclass - ca)
summary(lm.charges2)

# Refined Model Summary: lm.charges2
# ----------------------------------------------------
# - Adjusted R-squared: 0.685; Residual SE: 0.7145
# - Key Predictors: age, death1, hospdead1, slos, hday, dzgroup categories
# - Some race predictors less significant (higher p-values)
# - Strong overall model (F-statistic p-value < 2.2e-16)
# - Next Steps: Reassess less significant predictors, validate model, check assumptions
# ----------------------------------------------------

# Example: removing race based on higher p-values
lm.charges3 <- update(lm.charges2, . ~ . - race)
summary(lm.charges3)

# We test to see which variables can be dropped:
drop1(lm.charges3, test = "F")

# Assessing Variable Significance in lm.charges3 Model:
# ----------------------------------------------------
# The drop1() analysis indicates the relative importance of variables:
# - Variables with the least impact on model AIC (smallest increase upon removal):
#   1. 'adlsc': Minor change in AIC to -5844.8
#   2. 'pafi': AIC changes slightly to -5844.4
#   3. 'urine': AIC changes to -5843.2 upon removal
#   4. 'alb', 'scoma', 'resp': Small changes in AIC (-5840.0, -5840.0, -5838.3, respectively)
# - These variables are less crucial to the model's predictive accuracy.

# ----------------------------------------------------

# Update the model by removing variables with the least impact
lm.charges4 <- update(lm.charges3, . ~ . 
                      - adlsc - pafi - urine - alb - scoma - resp)

# Check the summary of the updated model
summary(lm.charges4)

#################CROSS VALIDATION###########################
# Load caret library
library(caret)

# Setting a seed for reproducibility of results
set.seed(123)

# Set up 10-fold cross-validation
train_control <- trainControl(method = "cv", number = 10)  # Using 10 folds

# Extracting the formula from the lm.charges4 model
# This ensures we only use the variables that were in the lm.charges4 model
model_formula <- formula(lm.charges4)

# Performing 10-fold cross-validation using the extracted formula
# The train function from the caret package is used for cross-validation
# full_df is the dataset used to fit lm.charges4
cv_model <- train(model_formula, data = full_df, method = "lm", trControl = train_control)

# Displaying the results of cross-validation
# The output includes important metrics such as RMSE and R-squared
# These metrics help in evaluating the model's predictive accuracy
print(cv_model)

##################SIMPLIFYING THE MODEL#######################

# Extracting coefficients
model_coefs <- coef(lm.charges4)

# The names of these coefficients are the variable names (excluding the intercept)
variable_names <- names(model_coefs)[-1]  # Exclude the intercept
print(variable_names)

# Create a correlation plot to further investigate correlated variables'
#and possibility of  elimination using domain knowledge

# Using 'full_df' filter out non-numeric variables
df_numeric <- full_df[sapply(full_df, is.numeric)]

# Subsetting full_df to exclude variables not in 'lm.charges4'
reduced_df <- subset(df_numeric, select = -c(adlsc, pafi, urine, alb, scoma, resp))


cor_matrix <- cor(reduced_df, use = "complete.obs")  # Handling missing values

# Flatten and filter
corr_melted <- melt(cor_matrix)

# Remove self-correlations and duplicates
corr_melted <- subset(corr_melted, Var1 != Var2)
corr_melted <- corr_melted[!duplicated(t(apply(corr_melted[, 1:2], 1, sort))), ]

# Sort by absolute correlation value
corr_melted <- corr_melted[order(-abs(corr_melted$value)), ]
top_correlations <- head(corr_melted, 40)

print(top_correlations)

# Analysis of Significant Predictor Correlations in the Model:
# ----------------------------------------------------------------------
# Analyzing correlations among predictors, we identify several with strong to mild correlation:

# 1. temp and hrt (0.2648): Though below the strong correlation threshold, this relationship is noteworthy.
#    Further Validation: Investigate physiological literature,  
#    to explore the linkage between body temperature and heart rate.

# 2. hday and slos (0.1992): Reflects a relationship between hospitalization days and length of stay.


# ----------------------------------------------------------------------
# Conducting drop1 analysis
drop1_analysis <- drop1(lm.charges4, test = "F")

# Printing the results
print(drop1_analysis)

# Model Simplification Analysis Based on drop1:
# - 'slos', 'hday', 'dzgroup': Critical for the model due to high AIC impact. Retain these.
# - 'hospdead', 'bili', 'bun': Moderately impactful. Consider keeping unless strong rationale for removal.
# - 'age', 'edu', 'meanbp': Lower impact. Could be candidates for simplification.
# - 'd.time', 'num.co', 'dementia', 'hrt', 'temp', 'ph': Least impact. Potential to be dropped for a more streamlined model.


# Updating the model by dropping variables with the least impact
lm.charges5 <- update(lm.charges4, . ~ . - d.time - num.co 
                      - dementia - hrt - temp - ph -meanbp -slos -hday )

# Summary of the updated model
summary(lm.charges5)

# Conducting drop1 analysis
drop1_analysis <- drop1(lm.charges5, test = "F")
# Printing the results
print(drop1_analysis)






