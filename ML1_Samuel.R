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

df_updated_mis <- subset(df_updated_mis, select = -c(prg2m, prg6m, surv6m, totcst, sps, avtisst, sfdm2))

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

# Refining the Dataset for Predictive Modeling
# Removing variables based on domain knowledge and previous model insights:
# - Variables like 'aps', 'dnr', 'dnrday', 'surv2m' are post-admission metrics, not relevant at initial assessment.
# - Clinical measurements 'sod', 'ca', 'urine' show limited predictive value and redundancy.
# - Redundant or less impactful variables: 'diabetes', 'dzclass', 'hrt', 'd.time', 'death', 'resp', wblc

final_df <- subset(final_df, select = -c(
    aps,
    dnr,
    dnrday,
    surv2m,
    sod, 
    ca, 
    diabetes, 
    dzclass, 
    urine, 
    hrt,
    d.time,
    death, 
    resp,
    sex,
    race, wblc
))

# Check
sort(colnames(final_df))

#################### Exploratory Data Analysis#####################
str(final_df)

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
# DEVELOP MULTIPLE LINEAR MODELS TO  RELATIONSHIP BETWEEN CHARGES AND OTHER VARIABLES

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
#   1. 'adlsc': Minor change in AIC to -5844.8 and lower significance
#   2. 'pafi': AIC changes slightly to -5844.4
#   3. 'urine': AIC changes to -5843.2 and lower significance
#   4. 'alb', 'scoma', 'resp': Significant changes in AIC 
#   5. 'd.time', 'slos': Keeping only one time variable i.e hday,
#       as dropping slos changes AIC a lot to 1982, and d.time is statistically less significant

# ----------------------------------------------------

# Update the model by removing variables with the least impact
lm.charges4 <- update(lm.charges3, . ~ . 
                      - adlsc - pafi - urine - alb - scoma - resp -slos -d.time)

# Check the summary of the updated model
summary(lm.charges4)


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
reduced_df <- subset(df_numeric, select = -c(adlsc, pafi, urine, 
                                             alb, scoma, resp,slos,d.time))
                                  
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

# temp and hrt (0.2648): Though below the strong correlation threshold, this relationship is noteworthy.
#    Further Validation: Investigating physiological literature,  
#    to the linkage between body temperature and heart rate is validated true.
#   Insert reference link :
# ----------------------------------------------------------------------
# Conducting drop1 analysis
drop1_analysis <- drop1(lm.charges4, test = "F")

# Printing the results
print(drop1_analysis)

# Model Simplification Analysis Based on drop1:
# - 'hday','dzgroup': Drop both. Critical for simplicity due to high AIC impact.
# -  'bili', 'bun': Drop 'bili' and retain 'bun' for blood factor.
# - 'age', 'edu', 'income' : Drop 'age','income' and 'dementia', retain 'edu' for dual temporal and categorical aspects.
# - 'dementia', 'hrt', 'ph','meanbp': Drop 'hrt', as redundant with 'temp'. Drop 'ph' and 'meanbp'

# Updating the model by dropping variables with the least impact
lm.charges5 <- update(lm.charges4, . ~ . -dzgroup -hday -bili 
                      -age -income -hrt -ph -meanbp -dementia)

# Summary of the updated model
summary(lm.charges5)

# Conducting drop1 analysis
drop1_analysis <- drop1(lm.charges5, test = "F")
# Printing the results
print(drop1_analysis)

# Finally the drop1 analysis does not yield further decreases in AIC values,
# suggesting the model cannot be further simplified without loss of predicttive power.

##############CROSS VALIDATION AND BEST MODEL SELECTION###############

#################CROSS VALIDATION###########################
library(caret)

# Setting a seed for reproducibility
set.seed(123)

# Create a function for cross-validation
cross_validate_model <- function(model, full_df, number_folds = 10) {
  # Extract the formula from the model
  model_formula <- formula(model)
  
  # Set up cross-validation control
  train_control <- trainControl(method = "cv", number = number_folds)
  
  # Train the model using cross-validation
  cv_model <- train(model_formula, data = full_df, method = "lm", trControl = train_control)
  
  # Return the results
  return(cv_model$results)
}

# Perform cross-validation for each of the remaining models
results1 <- cross_validate_model(lm.charges1, full_df)
results2 <- cross_validate_model(lm.charges2, full_df)
results3 <- cross_validate_model(lm.charges3, full_df)
results4 <- cross_validate_model(lm.charges4, full_df)
results5 <- cross_validate_model(lm.charges5, full_df)

# Print the results for each as a table
library(knitr)
library(DT)

# Rounding the values in the data frame
results_df <- data.frame(
  Model = c("lm.charges1", "lm.charges2", "lm.charges3", "lm.charges4", "lm.charges5"),
  RMSE = round(c(results1$RMSE, results2$RMSE, results3$RMSE, results4$RMSE, results5$RMSE), 3),
  Rsquared = round(c(results1$Rsquared, results2$Rsquared, results3$Rsquared, results4$Rsquared, results5$Rsquared), 3),
  MAE = round(c(results1$MAE, results2$MAE, results3$MAE, results4$MAE, results5$MAE), 3)
)

datatable(results_df, caption = 'Cross-validation Results', options = list(pageLength = 5))

########### Cross-validation Results Brief###########

# Models lm.charges1 and lm.charges2 show the highest predictive accuracy (RMSE ~0.718-0.719, R-squared ~0.682-0.683). 
# lm.charges3 slightly underperforms compared to the first two.
# lm.charges4, while less accurate (RMSE: 0.897, R-squared: 0.504), offers a balance between simplicity and accuracy.
# lm.charges5 exhibits significantly lower performance (RMSE: 1.165, R-squared: 0.164), indicating potential overfitting.

# Conclusion: Despite a minor compromise on accuracy, lm.charges4 may be preferred for its balance of model simplicity and predictive power.

##########INTERACTION-TERMS EFFECT ON model 'lm.charges4'######
# Are there any interesting interactions effects on charges due to 
# variables?
# 1. Does the impact of age on healthcare charges vary significantly
# across different disease groups in the dataset?

# Adding an interaction term between age and disease group to the model
lm_age_dzgroup <- lm(log_charges ~ age * dzgroup 
                                 + hospdead + num.co + edu + income
                                 + hday + dementia + meanbp + hrt + temp + bili + ph + bun, 
                                 data = full_df)

# Checking the summary of the model to see the interaction effect
summary(lm_age_dzgroup)

# Summary of Interaction Effects between Age and Disease Groups
# - CHF: Significant decrease in charges with age.
# - Coma: Significant decrease in charges with age.
# - MOSF w/Malignancy: Significant decrease in charges with age.
# - Cirrhosis, Colon Cancer, COPD, Lung Cancer: No significant interaction.

# 2. Is the effect of hospital death on healthcare charges modified 
#  by the type of disease a patient has?
# Adding an interaction term between hospital stay length (hospdead) and disease group
lm_hospdead_dzgroup <- lm(log_charges ~ hospdead * dzgroup 
                                      + age + num.co + edu + income
                                      + hday + dementia + meanbp + hrt + temp + bili + ph + bun, 
                                      data = full_df)

# Checking the summary of the model to see the interaction effect
summary(lm_hospdead_dzgroup)

# Summary of Interaction Effects between Hospital Death and Disease Group
# - CHF, Cirrhosis, COPD: Significant increase in charges with hospital death.
# - Coma: Significant decrease in charges with hospital death.
# - Colon Cancer, Lung Cancer, MOSF w/Malignancy: No significant interaction.

#########CROSS VALIDATE AND COMPARE RESULTS##########

set.seed(123)

# Perform cross-validation for the Age-Disease Group Interaction Model
results_age_dzgroup <- cross_validate_model(lm_age_dzgroup, full_df)

# Perform cross-validation for the Hospdead-Disease Group Interaction Model
results_hospdead_dzgroup <- cross_validate_model(lm_hospdead_dzgroup, full_df)

# Compile the results into a table
results_df <- data.frame(
  Model = c("lm.charges4", "Age-Disease Group Interaction", "Hospdead-Disease Group Interaction"),
  RMSE = round(c(results4$RMSE, results_age_dzgroup$RMSE, results_hospdead_dzgroup$RMSE), 3),
  Rsquared = round(c(results4$Rsquared, results_age_dzgroup$Rsquared, results_hospdead_dzgroup$Rsquared), 3),
  MAE = round(c(results4$MAE, results_age_dzgroup$MAE, results_hospdead_dzgroup$MAE), 3)
)

# Use knitr and DT packages to display the results table
library(knitr)
library(DT)

datatable(results_df, caption = 'Cross-validation Results for Interaction Models and lm.charges4', options = list(pageLength = 5))

# Cross-validation Results Summary
# - Baseline (lm.charges4): RMSE: 0.897, R-squared: 0.504, MAE: 0.714.
# - Age Interaction: Marginal improvement over baseline.
# - Hospdead Interaction: Best performance with lowest RMSE (0.887) and highest R-squared (0.516).

#########Visual Exploration of Non-Linear Trends#############

# Loop through each numeric variable to visually explore potential non-linear relationships
library(ggplot2)

for (var in numeric_vars) {
  # Creating a scatterplot with a smoother to detect non-linear trends
  print(
    ggplot(final_df, aes_string(x = var, y = "log_charges")) +
      geom_point(shape = 19, size = 1, alpha = 0.5, color = "blue") +
      geom_smooth(se = TRUE, color = "red", fill = "orange") +  # Loess smoother for non-linear trends
      labs(title = paste("2Exploring Non-Linear Relationship of", var, "with log_charges"),
           x = var, y = "log_charges")
  )
  
  # Comment: Each plot shows the relationship between 'log_charges' and a numeric variable
  # The smoother (red line with orange confidence area) helps identify potential non-linear patterns
}
# EDA Summary: Non-Linear Effects

# Quadratic Effects: meanbp, bun, ph, alb
# Polynomial Effects: temp, wblc, edu, hrt, hday, bili, num.co
# No significant non-linear trend in age.


#######Iterating on the lm.charges4, with non linear effects######

# Building a model incorporating observed non-linear effects

# Update the lm.charges4 model with non-linear terms
nonlinear.lm.charges4 <- update(lm.charges4, . ~ . - meanbp - bun - ph - alb 
                                - temp - wblc - edu - hrt - hday - bili - num.co +
                                  poly(meanbp, 2) + 
                                  poly(bun, 2) +
                                  poly(ph, 2) +
                                  poly(alb, 2) +
                                  poly(temp, 3) + 
                                  poly(wblc, 3) + 
                                  poly(edu, 3) +
                                  poly(hrt, 3) +
                                  poly(hday, 3) +
                                  poly(bili, 3) +
                                  poly(num.co, 3))


# Summary of the updated model
summary(nonlinear.lm.charges4)

# Drop impact analysis
drop1(nonlinear.lm.charges4, test = "F")

# Simplify model, step removes: alb & wblc higher order poly terms
lm.nonlinear.trim <- step(nonlinear.lm.charges4, direction = "backward")
summary(lm.nonlinear.trim)

# CROSS VALIDATE AND COMPARE RESULTS
# Perform cross-validation for nonlinear.lm.charges4
results_nonlinear_charges4 <- cross_validate_model(nonlinear.lm.charges4, full_df)

# Perform cross-validation for the final trimmed model
results_trimmed <- cross_validate_model(lm.nonlinear.trim, full_df)

# Combine the results into one data frame in the specified order
all_results_df <- data.frame(
  Model = c(
    "lm.charges1", "lm.charges2", "lm.charges3", "lm.charges4", "lm.charges5",
    "Age-Disease Group Interaction", "Hospdead-Disease Group Interaction",
    "Nonlinear lm.charges4", "Trimmed Nonlinear Model"
  ),
  RMSE = round(c(
    results1$RMSE, results2$RMSE, results3$RMSE, results4$RMSE, results5$RMSE,
    results_age_dzgroup$RMSE, results_hospdead_dzgroup$RMSE,
    results_nonlinear_charges4$RMSE, results_trimmed$RMSE
  ), 3),
  Rsquared = round(c(
    results1$Rsquared, results2$Rsquared, results3$Rsquared, results4$Rsquared, results5$Rsquared,
    results_age_dzgroup$Rsquared, results_hospdead_dzgroup$Rsquared,
    results_nonlinear_charges4$Rsquared, results_trimmed$Rsquared
  ), 3),
  MAE = round(c(
    results1$MAE, results2$MAE, results3$MAE, results4$MAE, results5$MAE,
    results_age_dzgroup$MAE, results_hospdead_dzgroup$MAE,
    results_nonlinear_charges4$MAE, results_trimmed$MAE
  ), 3)
)

# Display the combined results table
datatable(all_results_df, caption = 'Comprehensive Cross-validation Results Comparison', options = list(pageLength = 10))

# Cross-validation Results Summary:

# - **Linear Models (lm.charges1-3)**: Good predictive accuracy with RMSE ~0.72, R-squared ~0.68.
# - **Simplified Models (lm.charges4-5)**: Lower performance; lm.charges5 particularly poor (RMSE: 1.165, R-squared: 0.164).
# - **Interaction Models**: Comparable to lm.charges4; slight improvements in R-squared but not significant.
# - **Poly Model**: Slight improvement over lm.charges4 (RMSE: 0.882, R-squared: 0.521).
# - **Trimmed Poly Model**: Reduced performance (RMSE: 1.006, R-squared: 0.386).
# 
# **Key Insight**: Nonlinear.lm.charges4 balances complexity with predictive accuracy


##############GENERALIZED ADDITIVE MODEL############
# We update our most performant model to check if it can be further improved

# Load GAM library
library(mgcv)

# Update the model to a default GAM
gam.charges4 <- gam(log_charges ~ hospdead + sex + age + dzgroup + 
                                s(meanbp) + 
                                s(bun) +
                                s(ph) +
                                s(alb) +
                                s(temp) + 
                                s(wblc) + 
                                s(edu) +
                                s(hrt) +
                                s(hday) +
                                s(bili) +
                                s(num.co),
                              data = full_df)

# Summary of the GAM model
summary(gam.charges4)
# GAM Summary Insights
# 1. Significant Linear Predictors: hospdead, age, dzgroup categories.
# 2. Non-Linear Relationships: Complex relationships identified for meanbp, bun, ph, temp, edu, hrt, hday, bili.
# 3. Model Fit: Adjusted R-squared of 0.54; GCV score of 0.7519.
# 4. Key Takeaway: Model captures both linear and non-linear dynamics, indicating the complexity of factors affecting log_charges.

library(gam)
library(Metrics)

set.seed(123) # For reproducibility

# Define the number of folds for cross-validation
n_folds <- 10
folds <- cut(seq(1, nrow(full_df)), breaks = n_folds, labels = FALSE)

# Initialize variables to store metrics
rmse_values <- numeric(n_folds)
rsquared_values <- numeric(n_folds)
mae_values <- numeric(n_folds)

# Cross-validation loop
for(i in 1:n_folds){
  # Split data into training and test sets
  test_indices <- which(folds == i, arr.ind = TRUE)
  train_data <- full_df[-test_indices, ]
  test_data <- full_df[test_indices, ]
  
  # Fit GAM model
  gam_model <- gam(log_charges ~ hospdead + sex + age + dzgroup + s(meanbp) + s(bun) + 
                     s(ph) + s(alb) + s(temp) + s(wblc) + s(edu) + s(hrt) + s(hday) + 
                     s(bili) + s(num.co), data = train_data)
  
  # Predict on test set
  predictions <- predict(gam_model, test_data)
  
  # Calculate metrics
  rmse_values[i] <- rmse(test_data$log_charges, predictions)
  rsquared_values[i] <- cor(test_data$log_charges, predictions)^2
  mae_values[i] <- mae(test_data$log_charges, predictions)
}

# Calculate average metrics
mean_rmse <- mean(rmse_values)
mean_rsquared <- mean(rsquared_values)
mean_mae <- mean(mae_values)

# Manually computed cross-validation metrics for the GAM model
mean_rmse_gam <- mean_rmse
mean_rsquared_gam <- mean_rsquared
mean_mae_gam <- mean_mae

# Combine the results into one data frame in the specified order
all_results_df <- data.frame(
  Model = c(
    "lm.charges1", "lm.charges2", "lm.charges3", "lm.charges4", "lm.charges5",
    "Age-Disease Group Interaction", "Hospdead-Disease Group Interaction",
    "Nonlinear lm.charges4", "Trimmed Nonlinear Model", "GAM Model"
  ),
  RMSE = round(c(
    results1$RMSE, results2$RMSE, results3$RMSE, results4$RMSE, results5$RMSE,
    results_age_dzgroup$RMSE, results_hospdead_dzgroup$RMSE,
    results_nonlinear_charges4$RMSE, results_trimmed$RMSE, mean_rmse_gam
  ), 3),
  Rsquared = round(c(
    results1$Rsquared, results2$Rsquared, results3$Rsquared, results4$Rsquared, results5$Rsquared,
    results_age_dzgroup$Rsquared, results_hospdead_dzgroup$Rsquared,
    results_nonlinear_charges4$Rsquared, results_trimmed$Rsquared, mean_rsquared_gam
  ), 3),
  MAE = round(c(
    results1$MAE, results2$MAE, results3$MAE, results4$MAE, results5$MAE,
    results_age_dzgroup$MAE, results_hospdead_dzgroup$MAE,
    results_nonlinear_charges4$MAE, results_trimmed$MAE, mean_mae_gam
  ), 3)
)

# Display the combined results table
library(DT)
datatable(all_results_df, caption = 'Comprehensive Cross-validation Results Comparison', options = list(pageLength = 10))



###########GLM : LOGISTIC REGRESSION##################
# What are the key predictors that significantly influence the likelihood of 
#a patient's death in the hospital?

# Including log_charges in the dataset for visualizing hospdead
# Make data ready for plotting
numeric_vars <- names(final_df)[sapply(final_df, is.numeric)]
numeric_vars

#Exclude 'log_charges' from Numeric Columns

numeric_vars <- numeric_vars[!(numeric_vars %in% c("charges"))]
numeric_vars

library(ggplot2)

# Assuming 'final_df' is your dataframe and it contains a binary variable 'hospdead'
# and 'numeric_vars' is a vector of all numeric variable names you want to plot.

for (var in numeric_vars) {
  print(ggplot(final_df, aes_string(x = var, y = 'hospdead')) +
    geom_jitter(aes(color = as.factor(hospdead)), width = 0.1, height = 0.1) +
    scale_color_manual(values = c('0' = 'red', '1' = 'blue')) +
    labs(title = paste("Scatterplot of", var, "against hospdead status"),
         x = var, y = "Hospdead Status") +
    theme_minimal())
}

# OBSERVATION: The visual inspection of scatterplots comparing each numeric 
# variable with the hospdead status reveals no clear patterns or 
# distinctions that could reliably classify a patient's hospital death status.

# Building an initial logistic regression model using all variables
# to evaluate their initial significance.
logistic_model1 <- glm(hospdead ~ . , family = "binomial", data = final_df)
summary(logistic_model1)

# The initial model yields no significant terms, indicating potential issues with multicollinearity or irrelevance.
# Therefore, we start building the model from scratch, beginning with the most promising predictor: age.
logistic_1 <- glm(hospdead ~ age , family = "binomial", data = final_df)
summary(logistic_1)

# Adding 'sex' to the model, as gender differences may influence hospital death outcomes.
logistic_2 <- glm(hospdead ~ age + sex , family = "binomial", data = final_df)
summary(logistic_2)

# Incorporating 'adlsc' (Activities of Daily Living Score) as it can be a strong indicator of patient health status.
logistic_3 <- glm(hospdead ~ age + adlsc, family = "binomial", data = final_df)
summary(logistic_3)

# Adding 'slos' (Days from Study Entry to Discharge) to explore if shorter treatment time correlate with mortality.
logistic_4 <- glm(hospdead ~ age + adlsc + slos, family = "binomial", data = final_df)
summary(logistic_4)

# Including 'hday' (Hospital Day) to assess the impact of the duration of hospitalization on patient outcomes.
logistic_5 <- glm(hospdead ~ age + adlsc + hday, family = "binomial", data = final_df)
summary(logistic_5)

# Expanding the model with clinical variables like 'alb' (Albumin levels), 'bili' (Bilirubin levels), 
# 'crea' (Creatinine levels), 'pafi' (Partial Pressure of Arterial Oxygen), and 'dzclass' for disease classification.
# These clinical measures are often critical in understanding patient health and predicting outcomes.
logistic_6 <- glm(hospdead ~ age + adlsc + hday + alb + bili + crea + dzclass + pafi, family = "binomial", data = final_df)
summary(logistic_6)

# Applying backward stepwise regression to the expanded model to refine it by removing statistically insignificant variables.
# This helps in achieving a simpler model that still captures essential predictors for hospital death.
step(logistic_6, direction = "backward")


##########
library(caret)

# Set a seed for reproducibility
set.seed(123)

# Define the control method for cross-validation
train_control <- trainControl(method = "cv", number = 10, classProbs = TRUE, summaryFunction = twoClassSummary)

# Ensure 'hospdead' is a factor
final_df$hospdead <- as.factor(final_df$hospdead)

# Rename the factor levels to be valid R variable names
levels(final_df$hospdead) <- make.names(levels(final_df$hospdead))

# Now proceed with the cross-validation
cv_logistic <- train(hospdead ~ age + adlsc + hday + alb + bili + crea + dzclass + pafi, 
                     data = final_df, 
                     method = "glm", 
                     family = "binomial",
                     trControl = train_control,
                     metric = "ROC")

# Print the results
print(cv_logistic)








