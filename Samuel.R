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
# Adjusted R-squared: 0.676 - Model explains ~67.6% of variability
# F-statistic highly significant - Strong overall model predictive power

# All variables significant except 'crea'

# Issues to Address:
# - Remove less significant predictor: crea

# ----------------------------------------------------
# We test to see which variables can be dropped:
drop1(lm.charges1, test = "F")

# Update the model by excluding identified variables
lm.charges2 <- update(lm.charges1, . ~ . - crea)
summary(lm.charges2)

# Refined Model Summary: lm.charges2
# ----------------------------------------------------
# - Adjusted R-squared: 0.676; Residual SE: 0.724
# ----------------------------------------------------
# We test to see which variables can be dropped:
drop1(lm.charges2, test = "F")

# Simplyfying model  based on higher p-values and AIC impact,
# keeping variables of interest in the model
lm.charges3 <- update(lm.charges2, . ~ . -adlsc -alb - pafi -scoma )
summary(lm.charges3)
# - Adjusted R-squared: 0.675; Residual SE: 0.725, Same fit but simpler

# We test to see which variables can be dropped:
drop1(lm.charges3, test = "F")


# Continue, Simplifying the model by removing variables with 
# Higher p-values and high AIC impact, keeping variables of interest in the model
lm.charges4 <- update(lm.charges3, . ~ . 
                      - ph -dementia -num.co -meanbp )
summary(lm.charges4)
# - Adjusted R-squared: 0.671; Residual SE: 0.729, Same fit much simpler
# We test to see which variables can be dropped:
drop1(lm.charges4, test = "F")

# Continue, Simplifying the model by removing variables with 
# Higher p-values and high AIC impact, keeping variables of interest in the model
lm.charges5 <- update(lm.charges4, . ~ . - temp -income)
summary(lm.charges5)
# - Adjusted R-squared: 0.666; Residual SE: 0.735, Still similar fit much more simpler
drop1(lm.charges5, test = "F")


# Now, keep Updating the model by dropping variables with Highest AIC impact
# as p-values are the same for all variables
lm.charges5 <- update(lm.charges4, . ~ . - slos )

# Summary of the updated model
summary(lm.charges5)
# - Adjusted R-squared: 0.49; Residual SE: 0.9, Worse fit much more simpler

drop1(lm.charges5, test = "F")

# Updating the model by dropping variable with higher p-values
lm.charges6 <- update(lm.charges5, . ~ . - income )

summary(lm.charges6)
# - Adjusted R-squared: 0.494; Residual SE: 0.9, better fit much more simpler

# Conducting drop1 analysis
drop1(lm.charges6, test = "F")

# Updating the model by dropping variables higher p-values
lm.charges7 <- update(lm.charges6, . ~ . - bili )

# Summary of the updated model
summary(lm.charges7)
# - Adjusted R-squared: 0.491; Residual SE: 0.9, similar fit much more simpler

# Conducting drop1 analysis
drop1(lm.charges7, test = "F")

# Updating the model by dropping variables highest AIC impact
lm.charges8 <- update(lm.charges7, . ~ . - hday )

# Summary of the updated model
summary(lm.charges8)
# - Adjusted R-squared: 0.42; Residual SE: 0.96, worse fit much more simpler

# Conducting drop1 analysis
drop1(lm.charges8, test = "F")

# Finally the model cannot be further simplified without loss of predictive power.

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
results6 <- cross_validate_model(lm.charges6, full_df)
results7 <- cross_validate_model(lm.charges7, full_df)
results8 <- cross_validate_model(lm.charges8, full_df)

# Print the results for each as a table
library(knitr)
library(DT)

# Rounding the values in the data frame
results_df <- data.frame(
  Model = c("lm.charges1", "lm.charges2", "lm.charges3", "lm.charges4",
            "lm.charges5", "lm.charges6", "lm.charges7", "lm.charges8"),
  RMSE = round(c(results1$RMSE, results2$RMSE, results3$RMSE, 
                 results4$RMSE, results5$RMSE, results6$RMSE, results7$RMSE, results8$RMSE), 3),
  Rsquared = round(c(results1$Rsquared, results2$Rsquared, 
                     results3$Rsquared, results4$Rsquared, results5$Rsquared, results6$Rsquared, results7$Rsquared, results8$Rsquared), 3),
  MAE = round(c(results1$MAE, results2$MAE, results3$MAE, 
                results4$MAE, results5$MAE, results6$MAE, results7$MAE, results8$MAE), 3)
)

datatable(results_df, caption = 'Linear Model development Cross-validation Results', 
          options = list(pageLength = 8))

########## Cross-validation Results Brief ###########

#lm.charges1 and lm.charges2: Best accuracy (RMSE ~0.726, R-squared ~0.676).
#lm.charges3: Slightly lower performance.
#lm.charges4: Decent trade-off (RMSE: 0.731, R-squared: 0.671).
#lm.charges5: Lower accuracy (RMSE: 0.904, R-squared: 0.497), possible overfitting.
#Conclusion: lm.charges4 stands out for balancing simplicity and effectiveness.

##########INTERACTION-TERMS EFFECT ON model 'lm.charges4'######
# Are there any interesting interactions effects on charges due to 
# any variables?
# 1. Does the impact of age on healthcare charges vary significantly
# across different disease groups in the dataset?

# Adding an interaction term between age and disease group to the model
lm_age_dzgroup <- lm(log_charges ~ age * dzgroup 
                                 + hospdead + edu + income
                                 + hday + temp + bili + bun, 
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
                                      + age + + edu + income + hday 
                                      + temp + bili + bun, 
                                      data = full_df)

# Checking the summary of the model to see the interaction effect
summary(lm_hospdead_dzgroup)




#########CROSS VALIDATE AND COMPARE RESULTS##########

set.seed(123)

# Perform cross-validation for the Age-Disease Group Interaction Model
results_age_dzgroup <- cross_validate_model(lm_age_dzgroup, full_df)

# Perform cross-validation for the Hospdead-Disease Group Interaction Model
results_hospdead_dzgroup <- cross_validate_model(lm_hospdead_dzgroup, full_df)

# Rounding and creating a data frame for the results
results_df2 <- data.frame(
  Model = c("lm.charges4", "lm_age_dzgroup", "lm_hospdead_dzgroup"),
  RMSE = round(c(results4$RMSE, results_age_dzgroup$RMSE, results_hospdead_dzgroup$RMSE), 3),
  Rsquared = round(c(results4$Rsquared, results_age_dzgroup$Rsquared, results_hospdead_dzgroup$Rsquared), 3),
  MAE = round(c(results4$MAE, results_age_dzgroup$MAE, results_hospdead_dzgroup$MAE), 3)
)

# Displaying the results in a datatable
datatable(results_df2, caption = 'Selected Interaction Cross-validation Results', options = list(pageLength = 5))

########## Cross-validation Results Brief ###########
# Built a Non-Linear model upon lm.charges4, lm_age_dzgroup and lm_hospdead_dzgroup, 
# with RMSEs over 0.890 and R-squared around 0.500, are less precise but offer 
#insights into specific age and disease group dynamics.

#########Visual Exploration of Non-Linear Trends#############

# Loop through each numeric variable to visually explore potential non-linear relationships
library(ggplot2)

for (var in numeric_vars) {
  # Creating a scatterplot with a smoother to detect non-linear trends
  print(
    ggplot(final_df, aes_string(x = var, y = "log_charges")) +
      geom_point(shape = 19, size = 1, alpha = 0.5, color = "blue") +
      geom_smooth(method = "loess",se = TRUE, color = "red", fill = "orange") +  # Loess smoother for non-linear trends
      labs(title = paste("Exploring Non-Linear Relationship of", var, "with log_charges"),
           x = var, y = "log_charges")
  )
  
  # Each plot shows the relationship between 'log_charges' and a numeric variable
  # The smoother (red line with orange confidence area) helps identify potential non-linear patterns
}
# EDA Summary: Non-Linear Effects
# From the graph we can see that there are some non-linear effects in the following variables:
# Probable Quadratic Effects: meanbp, bun, ph,slos 
# Polynomial Effects: temp, wblc, edu, hrt, hday, bili, num.co, alb
# No significant non-linear trend in age.


#######Iterating on the lm.charges4, with non linear effects######

# Building a model incorporating observed non-linear effects

# Update the lm.charges4 model with non-linear terms
nonlinear.lm.charges4 <- update(lm.charges4, . ~ . -slos - meanbp - bun - ph - alb 
                                - temp - edu - hday - bili - num.co +
                                  poly(slos, 2) +
                                  poly(meanbp, 2) + 
                                  poly(bun, 2) +
                                  poly(ph, 2) +
                                  poly(alb, 2) +
                                  poly(temp, 3) + 
                                  poly(edu, 3) +
                                  poly(hday, 3) +
                                  poly(bili, 3) +
                                  poly(num.co, 3))


# Summary of the updated model
summary(nonlinear.lm.charges4)

# num.co - Higher order terms are insignificant, change to linear
# bili - poly3 is insignificant, change to poly2
# edu - higher order terms are insignificant, change to linear
# meanbp - quadratic term is insignificant, change to linear


# Simplify model, remove alb
lm.nonlinear.trim <- update(lm.charges4, . ~ . -slos - bun - ph - alb 
                            - temp - hday - bili +
                              poly(slos, 2) +
                              poly(bun, 2) +
                              poly(ph, 2) +
                              poly(alb, 2) +
                              poly(temp, 3) + 
                              poly(hday, 3) +
                              poly(bili, 2)
                              )
summary(lm.nonlinear.trim)

# CROSS VALIDATE AND COMPARE RESULTS
# Perform cross-validation for nonlinear.lm.charges4
results_nonlinear_charges4 <- cross_validate_model(nonlinear.lm.charges4, full_df)

# Perform cross-validation for the final trimmed model
results_trimmed <- cross_validate_model(lm.nonlinear.trim, full_df)

# Combine the selected results into one data frame
selected_results_df <- data.frame(
  Model = c(
    "lm.charges4", "Nonlinear lm.charges4", "Trimmed Nonlinear Model"
  ),
  RMSE = round(c(
    results4$RMSE, results_nonlinear_charges4$RMSE, results_trimmed$RMSE
  ), 3),
  Rsquared = round(c(
    results4$Rsquared, results_nonlinear_charges4$Rsquared, results_trimmed$Rsquared
  ), 3),
  MAE = round(c(
    results4$MAE, results_nonlinear_charges4$MAE, results_trimmed$MAE
  ), 3)
)

# Display the selected results table
datatable(selected_results_df, caption = 'Selected Non-Linear Cross-validation Results Comparison', options = list(pageLength = 10))

########## Cross-validation Results Brief ###########

# Nonlinear adaptations of lm.charges4 led to two distinct models: Nonlinear lm.charges4 and Trimmed Nonlinear Model.
# Nonlinear lm.charges4, with an RMSE of 0.649 and R-squared of 0.741, shows marked improvement over the linear lm.charges4 (RMSE: 0.731, R-squared: 0.671).
# The Trimmed Nonlinear Model, further refined with fewer variables, achieves a similar performance (RMSE: 0.647, R-squared: 0.742), suggesting effective optimization without loss of predictive ability.

# Conclusion: The transition to nonlinear modeling significantly enhances the accuracy of the original lm.charges4, with the trimmed version balancing model complexity and predictive precision.

 

##############GENERALIZED ADDITIVE MODEL############
# We use variables from our most performant model:"lm.nonlinear.trim",
# to try to build a model that captures non-linear dynamics completely.

# Load GAM library
library(mgcv)

# Using the variables from lm.nonlinear.trim to a build base GAM
gam.model1 <- gam(log_charges ~ s(slos) + 
                                s(bun) +
                                s(ph) +
                                s(alb) +
                                s(temp) + 
                                s(hday) +
                                s(bili),
                              data = full_df)

# Summary of the GAM model
summary(gam.model1)

#########################################
library(mgcv)

#Prepare Data for K-Fold Cross-validation

set.seed(123)  # for reproducibility
k <- 10  # number of folds
n <- nrow(full_df)
fold_ids <- sample(rep(1:k, length.out = n))

# Define a function to perform k-fold cross-validation
perform_cv_for_gam <- function(model, data, k) {
  # Initialize vectors to store performance metrics
  rmse_values <- numeric(k)
  r_squared_values <- numeric(k)
  
  # Generate fold IDs for cross-validation
  set.seed(123)  # For reproducibility
  fold_ids <- sample(rep(1:k, length.out = nrow(data)))
  
  # Perform k-fold cross-validation
  for (i in 1:k) {
    train_data <- data[fold_ids != i, ]
    test_data <- data[fold_ids == i, ]
    
    # Make predictions and calculate RMSE and R-squared
    predictions <- predict(model, test_data, type = "response")
    rmse_values[i] <- sqrt(mean((predictions - test_data$log_charges)^2))
    r_squared_values[i] <- cor(predictions, test_data$log_charges)^2
  }
  
  # Calculate and return the average RMSE and R-squared
  list(mean_rmse = mean(rmse_values), mean_r_squared = mean(r_squared_values))
}

# Results of 10-fold cross-validation
cv_results <- perform_cv_for_gam(gam.model1, full_df, 10)
gam_mean_rmse <- cv_results$mean_rmse
gam_mean_r_squared <- cv_results$mean_r_squared

# Combine the Selected Results into One Data Frame
selected_results_df <- data.frame(
  Model = c(
    "lm.charges4", "Nonlinear lm.charges4", "Trimmed Nonlinear Model", "GAM Model"
  ),
  RMSE = round(c(
    results4$RMSE, results_nonlinear_charges4$RMSE, results_trimmed$RMSE, gam_mean_rmse
  ), 3),
  Rsquared = round(c(
    results4$Rsquared, results_nonlinear_charges4$Rsquared, results_trimmed$Rsquared, gam_mean_r_squared
  ), 3),
  MAE = c(
    round(results4$MAE, 3), round(results_nonlinear_charges4$MAE, 3), round(results_trimmed$MAE, 3), NA  # Assuming MAE is not calculated for the GAM model
  )
)

# Display the selected results table
datatable(selected_results_df, caption = 'Selected GAM model Cross-validation Results Comparison', options = list(pageLength = 10))

########## Cross-validation Results Brief ###########

# Cross-validation Results Summary:

# lm.charges4:
# - RMSE: 0.731, R-squared: 0.671, MAE: 0.57
# - Provides a basic level of predictive accuracy, serving as a baseline.

# Nonlinear lm.charges4:
# - RMSE: 0.649, R-squared: 0.741, MAE: 0.502
# - Shows improved accuracy over the basic model, indicating the benefit of introducing nonlinearity.

# Trimmed Nonlinear Model:
# - RMSE: 0.647, R-squared: 0.742, MAE: 0.503
# - Offers a slight improvement in accuracy with a simpler, more interpretable model compared to the Nonlinear lm.charges4.

# GAM Model:
# - RMSE: 0.659, R-squared: 0.732
# - Despite its flexibility in capturing complex patterns, it slightly underperforms compared to the Nonlinear models, with potential overfitting due to complexity.

# Conclusion:
# - The Trimmed Nonlinear Model emerges as the best choice, offering high accuracy with a simpler, more interpretable model structure.
# - The GAM Model, while powerful, may require careful handling to avoid overfitting and ensure reliable predictions on new data.


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

# Set the sample size for better visibility of the scatterplots
sample_size <- 500  # Adjust this number based on your data size and needs

for (var in numeric_vars) {
  # Take a random sample of the data
  sampled_data <- final_df[sample(nrow(final_df), sample_size), ]
  
  # Create the scatterplot with the sampled data
  print(ggplot(sampled_data, aes_string(x = var, y = 'hospdead')) +
          geom_jitter(aes(color = as.factor(hospdead)), width = 0.1, height = 0.1) +
          scale_color_manual(values = c('0' = 'red', '1' = 'blue')) +
          labs(title = paste("Scatterplot of", var, "against hospdead status (Sampled)"),
               x = var, y = "Hospdead Status") +
          theme_minimal())
}


# OBSERVATION: The visual inspection of scatterplots comparing each numeric 
# variable with the hospdead status reveals a few potential separation patterns in variables like:
# log_charges, num.co, age, adlsc and alb.

# Building an initial logistic regression model using all variables
# to evaluate their initial significance.
logistic_full <- glm(hospdead ~ . , family = "binomial", data = final_df)
summary(logistic_full)

# The initial full model yields many significant terms, indicating variables of interest.
# like log_charges, age, adlsc, scoma, bun, bili, slos, dzgroup, income

# Therefore, we start building the model from scratch, beginning with the most promising predictor: age.
logistic_1 <- glm(hospdead ~ age , family = "binomial", data = final_df)
summary(logistic_1)

# Adding 'hday' to the model, as time spent under critical care could be a strong indicator of patient health status.

logistic_2 <- update(logistic_1, . ~ . + hday)
summary(logistic_2)

# Incorporating 'scoma' (Activities of Daily Living Score) as it can be a strong indicator of patient health status.
logistic_3 <- update(logistic_2, . ~ . + scoma)
summary(logistic_3)

# Adding 'adlsc' () to explore if Daily living activity score correlate with mortality.
logistic_4 <- update(logistic_3, . ~ . + adlsc)
summary(logistic_4)

# Including 'num.co' to assess the impact of number of comorbidities patient outcomes.
logistic_5 <- update(logistic_4, . ~ . + num.co)
summary(logistic_5)

# Check if log_charges is significant now
logistic_6 <- update(logistic_5, . ~ . + log_charges)
summary(logistic_6)

# Log_charges is highly insignificant, but num.co is insignificant,
# due to redundancy, so remove num.co from the model
logistic_6 <- update(logistic_6, . ~ . - num.co)
summary(logistic_6)

# Expanding the model with clinical variables like 'alb' (Albumin levels), 'bili' (Bilirubin levels), 
# 'crea' (Creatinine levels), 'pafi' (Partial Pressure of Arterial Oxygen), ph is insignificant, so remove it from the model
# These clinical measures are often critical in understanding patient health and predicting outcomes.
logistic_7 <- update(logistic_6, . ~ . + alb + bili + crea + 
                       bun + pafi + meanbp)
summary(logistic_7)

# Expanding the model with categorical variable income 
logistic_8 <- update(logistic_7, . ~ . + income)
summary(logistic_8)

# Income turns out to be very mildly significant, so we remove it from the model and 
# add 'dzgroup' (Disease Group) to assess the impact of disease type on patient outcomes.
logistic_9 <- update(logistic_8, . ~ . - income + dzgroup)
summary(logistic_9)

# As expected dzgroup is significant 
# Check if edu is significant now
logistic_10 <- update(logistic_9, . ~ . + edu)
summary(logistic_10)

# edu is insignificant, so remove it from the model and check dementia
logistic_10 <- update(logistic_10, . ~ . - edu + dementia)
summary(logistic_10)

# Dementia is mildly significant, so keep it in the model

# Applying backward stepwise regression to the expanded model to simplify
# it by removing statistically insignificant variables.
# This helps in achieving a simpler model that still captures essential predictors for hospital death.
logistic.simple<- step(logistic_10, direction = "backward")
summary(logistic.simple)
# Backward stepwise regression doesnt remove any variables, so keep the model as it is

#################CROSS VALIDATION###########################
##########
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
  cv_model <- train(model_formula, data = full_df, method = "glm", trControl = train_control)
  
  # Return the results
  return(cv_model$results)
}

# Perform cross-validation for each of the remaining models
results1 <- cross_validate_model(logistic_1, full_df)
results2 <- cross_validate_model(logistic_2, full_df)
results3 <- cross_validate_model(logistic_3, full_df)
results4 <- cross_validate_model(logistic_4, full_df)
results5 <- cross_validate_model(logistic_5, full_df)
results6 <- cross_validate_model(logistic_6, full_df)
results7 <- cross_validate_model(logistic_7, full_df)
results8 <- cross_validate_model(logistic_8, full_df)
results9 <- cross_validate_model(logistic_9, full_df)
results10 <- cross_validate_model(logistic_10, full_df)

# Print the results for each as a table
library(knitr)
library(DT)

# Create a summary table using the correct metrics from the cross-validation results
results_df <- data.frame(
  Model = c("logistic_1", "logistic_2", "logistic_3", "logistic_4",
            "logistic_5", "logistic_6", "logistic_7", "logistic_8", "logistic_9", "logistic_10"),
  Accuracy = round(c(results1$Accuracy, results2$Accuracy, results3$Accuracy, 
                     results4$Accuracy, results5$Accuracy, results6$Accuracy, results7$Accuracy, results8$Accuracy, results9$Accuracy, results10$Accuracy), 3),
  Kappa = round(c(results1$Kappa, results2$Kappa, results3$Kappa, 
                  results4$Kappa, results5$Kappa, results6$Kappa, results7$Kappa, results8$Kappa, results9$Kappa, results10$Kappa), 3),
  AccuracySD = round(c(results1$AccuracySD, results2$AccuracySD, results3$AccuracySD, 
                       results4$AccuracySD, results5$AccuracySD, results6$AccuracySD, results7$AccuracySD, results8$AccuracySD, results9$AccuracySD, results10$AccuracySD), 3)
)

# Display the summary table
datatable(results_df, caption = 'Logistic Regression models Cross-validation Results', options = list(pageLength = 10))


# Integrated Model Development and Performance Summary:

# Initial Steps:
# - logistic_1 (baseline with 'age'): Accuracy at 0.743; a starting point with room for improvement.
# - logistic_2 (addition of 'hday'): Slight increase in Accuracy to 0.744 and Kappa to 0.069, indicating marginal improvement.

# Incorporating Health Status:
# - logistic_3 (addition of 'scoma'): Significant leap in Accuracy to 0.781 and Kappa to 0.295, showing the importance of health status indicators.
# - logistic_4 (introduction of 'adlsc'): Maintains Accuracy at 0.782 and Kappa at 0.301, consolidating gains.

# Clinical and Demographic Expansion:
# - logistic_6-8: Further fine-tuning with clinical and demographic variables. Accuracy stabilizes around 0.775, and Kappa sees modest gains, highlighting the complex nature of these factors.

# Refined and Optimal Models:
# - logistic_9 (inclusion of 'dzgroup'): Peaks in Accuracy (0.783) and Kappa (0.35), underscoring the impact of disease grouping.
# - logistic_10 (testing 'edu' and 'dementia'): Sustains peak Accuracy and Kappa (0.351), confirming the optimized combination of variables.

# Final Takeaways:
# - The evolution from logistic_1 to logistic_10 illustrates a careful balance of adding predictive variables while monitoring overfitting (as shown by stable AccuracySD).
# - The best models, logistic_9 and logistic_10, demonstrate the effectiveness of a comprehensive approach, blending demographic, clinical, and disease-specific predictors.
# - This step-wise development process resulted in models that not only predict accurately but also provide meaningful insights into the factors affecting patient outcomes


##Selecting logistic_10 as our best model based on cross-validation results
# We want to deep dive understand the performance of the model using a confusion matrix
# and ROC curve.

# Calculate the Confusion Matrix
# Load the required libraries
library(caret)

# First, make predictions on the dataset
predictions <- predict(logistic_10, newdata = final_df, type = "response")
predicted_class <- ifelse(predictions > 0.5, 1, 0)  # Assuming 0.5 as threshold

# Generate the confusion matrix
conf_matrix0.5 <- confusionMatrix(as.factor(predicted_class), as.factor(final_df$hospdead))
print(conf_matrix0.5)

# Confusion Matrix (0.5) Interpretation:

# Model Performance:
# - Accuracy: 0.792. Model correctly predicts survival and non-survival in 79.2% of cases.
# - Kappa: 0.3813. Moderate agreement beyond chance between prediction and actual outcomes.

# Class-wise Performance:
# - Sensitivity: 92.34%. High ability to correctly identify non-survivors (True Negative Rate).
# - Specificity: 41.22%. Lower ability to correctly identify survivors (True Positive Rate).
# - Positive Predictive Value: 81.95%. If model predicts non-survival, 81.95% chance it's correct.
# - Negative Predictive Value: 65.06%. If model predicts survival, 65.06% chance it's correct.

# Statistical Tests:
# - P-Value [Acc > NIR]: < 2.2e-16. Model significantly better than No Information Rate.
# - Mcnemar's Test: < 2.2e-16. Significant difference between type I and type II errors.

# Overall Inferences:
# - Model is more effective at identifying non-survivors than survivors.
# - Imbalance in sensitivity and specificity suggests potential bias towards predicting non-survival.
# - High prevalence (74.29%) might influence the predictive values.
# - Balanced Accuracy (66.78%) indicates room for improvement in managing class imbalance.

# Conclusion:
# - While the model is quite accurate, there's a need for improved balance in sensitivity and specificity.
# - Consideration for class imbalance and further feature refinement could enhance model performance.

# ROC Curve and AUC
# Load the necessary library
# Load the necessary library
library(pROC)

# Calculate the ROC curve
roc_response <- roc(response = final_df$hospdead, predictor = as.numeric(predictions), percent = TRUE)

# Specify the thresholds you want to display on the plot
thresholds_to_print <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
# Plotting the ROC curve with thresholds
plot(roc_response, 
     main = "ROC Curve Analysis", 
     xlab = "False Positive Rate (1 - Specificity)", 
     ylab = "True Positive Rate (Sensitivity)", 
     col = "#1c61b6", 
     lwd = 2,
     print.thres = thresholds_to_print, 
     print.thres.pattern = "%.2f", # Adding percent sign to thresholds
     print.thres.cex = 0.6,
     print.auc = TRUE,
     print.auc.pattern = "AUC: %.3f", # Customize AUC print format
     print.auc.col = "blue",
     print.auc.cex = 1.2,
     grid = TRUE,
     identity = TRUE, # Turn off the identity line
     max.auc.polygon = TRUE, # Do not fill the area up to the maximum AUC possible
     auc.polygon = TRUE,
     auc.polygon.col = "lightblue"
)

# ROC Curve and AUC Interpretation for Hospdead Variable:

# - The AUC of 82.075% suggests the model is effective in distinguishing between patients who died in the hospital (hospdead = 1) and those who did not (hospdead = 0).
# - The True Positive Rate (TPR) or sensitivity indicates the model's accuracy in identifying actual in-hospital deaths.
# - The False Positive Rate (FPR) or 1 - specificity represents how often the model incorrectly predicts death among those who survived.

# Impact of Class Distribution:
# - The higher number of survivors (hospdead = 0) may influence the model's predictive performance, particularly its specificity.
# - Careful threshold selection is crucial to balance TPR and FPR given the class imbalance.

# Threshold Strategy for Clinical Utility:
# - In clinical settings where the cost of missing an actual death (False Negative) is high, a lower threshold may be warranted to ensure high sensitivity.
# - Conversely, to avoid overburdening the healthcare system with false alarms, a threshold that ensures an acceptable level of specificity without sacrificing too much sensitivity might be preferred.

# Conclusion:
# - The model's strong AUC performance, considering the distribution of the `hospdead` variable, indicates its potential as a supportive tool for prognostic assessment in a hospital setting.
# - The plotted thresholds on the ROC curve provide insights into how different cut-off points will affect the model's prediction of in-hospital mortality.


