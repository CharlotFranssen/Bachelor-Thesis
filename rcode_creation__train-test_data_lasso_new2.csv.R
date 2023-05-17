

# Load necessary libraries
library(dplyr)
library(readr)
library(survival)
library(glmnet)
library(tidyr)
library(caret)
library(Hmisc)
library(MASS)
library(randomForestSRC)
# library(pec)
library(ibs)


# setwd("C:/tilburg university/2022-2023/Thesis/")

# Load dataset
dataset <- read_csv("df_survival_Rcode.csv")

# Define columns to keep
keep_cols <- c("w4_quality")

# Remove columns with more than 20% NA values
na_perc <- sapply(dataset, function(x) mean(is.na(x))*100)
cols_to_remove <- names(na_perc[na_perc > 20 & !(names(na_perc) %in% keep_cols)])

lowdim_dataset <- dataset[, !(names(dataset) %in% cols_to_remove)]


# delete these columns since the variance is 0 (same value everywhere = no predictive power)
columns_to_exclude <- c("irb_consent", "qflag", "q24_fam_cousins_passive", "q24_fam_aunt_niece_passive", "q24_fam_uncle_nephew_passive", "q24_fam_grandmother_passive", "q24_fam_grandfather_passive", "pp2_igdr2")
lowdim_dataset <- lowdim_dataset[, setdiff(names(lowdim_dataset), columns_to_exclude)]

# cat_columns file --------------------------------------------------------


# create an empty list to store the categorical columns
not_cat_columns <- list("caseid_new", "weight1", "weight2", "ppage", "pphouseholdsize","hhinc","ppt01","ppt1317","ppt18ov","ppt25","ppt612","children_in_hh","ppppcmdate_yrmo","pppadate_yrmo","pphhcomp11_member2_age","pphhcomp11_member3_age","pphhcomp11_member4_age","pphhcomp11_member5_age","pphhcomp11_member6_age","pphhcomp11_member7_age","pphhcomp11_member8_age","pphhcomp11_member9_age","weight4","weight3","weight5","weight6","weight7","weight_couples_coresident","HCMST_main_interview_yrmo","duration","q9","q16","q21a","q21b","q21c","summary_q24_total","distancemoved_10mi","q32_internet","how_long_ago_first_met","how_long_ago_first_romantic","how_long_ago_first_cohab","how_long_relationship","age_difference","respondent_yrsed","partner_yrsed","partner_mom_yrsed","respondent_mom_yrsed","pp2_pphhsize","pp2_respondent_yrsed","pp2_ppt01","pp2_ppt1317","pp2_ppt18ov","pp2_ppt25","pp2_ppt612","pp2_ppcmdate_yrmo","w2_HCMST_interview_fin_yrmo","w2_duration","w2_days_elapsed","pp3_pphhsize","w3_xyear","w3_xmonth","w4_xyear","w4_xmonth","w4_xwave1_month","w4_xwave1_month","pp4_pphhsize","pp4_ppt01" , "pp4_ppt25" ,"pp4_ppt612" , "pp4_ppt1317" , "pp4_ppt18ov","pp4_ppcmdate_yrmo","ppa2009_services_yrmo","w5x_year","w5x_month","w5x_civmonth","w5x_civyear","pp5_ppage","pp5_pphhsize","pp5_ppcmdate_yrmo","pp5_ppt01" , "pp5_ppt25" ,"pp5_ppt612" , "pp5_ppt1317","pp5_ppt18ov","ppcmarit_2009_yrmo","ppcmarit_2007_yrmo" )
cat_columns <- list()

modified_lowdim_dataset <- lowdim_dataset

# loop through each column name in the dataset
for (namecol in colnames(lowdim_dataset)) {
  # skip the column if it is named "Year" or "Status"
  if (namecol %in% c("Year", "Status")) {
    next
  }
  # check if the column name ends with "cat"
  if (grepl("cat$", namecol)) {
    # add the matching column name to the list if it end with "cat"
    cat_columns <- append(cat_columns, namecol)
    modified_lowdim_dataset[[namecol]] <- as.factor(modified_lowdim_dataset[[namecol]])
    next
  }
  if (!namecol  %in% not_cat_columns){ # if column name not in not_cat_columns, add it to cat_columns
    cat_columns <- append(cat_columns, namecol)
    modified_lowdim_dataset[[namecol]] <- as.factor(modified_lowdim_dataset[[namecol]])
  }
}


# only include columns from wave 1
modified_lowdim_dataset2 <- modified_lowdim_dataset
modified_lowdim_dataset2 <- modified_lowdim_dataset2[, c(1:which(names(modified_lowdim_dataset2) == "coresident"), 
                                                         (ncol(modified_lowdim_dataset2)-1):ncol(modified_lowdim_dataset2))]


# ----------------------------------------------------------

# STEP 1: Dummy coding


#check for cat vars in dataframe: 
cat_vars <- sapply(modified_lowdim_dataset2, is.factor)
# Exclude Status and Year from the list of categorical columns
cat_cols <- names(cat_vars)[cat_vars & !names(cat_vars) %in% c("Status", "Year")]


# % cat cols ------------------------

# Count the number of categorical columns
num_cat_cols <- sum(cat_vars & !names(cat_vars) %in% c("Status", "Year"))

# Count the total number of columns
num_total_cols <- ncol(modified_lowdim_dataset2)

# Calculate the percentage of categorical columns
perc_cat_cols <- num_cat_cols / num_total_cols * 100

# Print the percentage of categorical columns
cat(sprintf("%.2f%% of columns are categorical", perc_cat_cols))

#------------------------------------------



# Check for columns with multiple levels (more than 5) and combine infrequent levels
for (colname in names(modified_lowdim_dataset2)) {
  if (is.factor(modified_lowdim_dataset2[[colname]]) && nlevels(modified_lowdim_dataset2[[colname]]) > 5) {
    freq_levels <- names(sort(table(modified_lowdim_dataset2[[colname]]), decreasing = TRUE)[1:9])
    modified_lowdim_dataset2[[colname]] = as.character(modified_lowdim_dataset2[[colname]])
    modified_lowdim_dataset2[[colname]][!(modified_lowdim_dataset2[[colname]] %in% freq_levels)] <- "Other"
    modified_lowdim_dataset2[[colname]] = as.factor(modified_lowdim_dataset2[[colname]])
    print(colname)
  }
}

# Create dummy variables for categorical columns using dummyVars()
dummy_transform <- dummyVars(~ ., data = modified_lowdim_dataset2[, cat_cols],fullRank = TRUE)

# Use predict() function to create dummy variables 
dummy_cols <- predict(dummy_transform, newdata = modified_lowdim_dataset2[, cat_cols])
#ncol(dummy_cols) #384 cols
#________________________________________________________________________
# STEP 2: preprocess numeric values excluding year and status:

numeric_only <- modified_lowdim_dataset2[, -which(names(modified_lowdim_dataset2) %in% c(cat_cols,"Year.1", "Status.1","caseid_new","Year","Status"))]
#View(numeric_only)#36 cols

#_______________________________________________
# STEP 2.1: impute missing values for only numeric cols (NOT THE DUMMY VARIABLES)

numeric_only <- as.data.frame(numeric_only)

for(c in 1:(ncol(numeric_only))) {
  median = median(numeric_only[,c], na.rm=TRUE)
  numeric_only[which(is.na(numeric_only[c])),c] = median
}
#___________________________________________________________
#transformation of the numeric cols

abnormal_columns = c("pphouseholdsize","ppt01","duration","ppt612","children_in_hh","q16","how_long_ago_first_met",
                     "how_long_ago_first_romantic","how_long_ago_first_cohab","how_long_relationship","age_difference","ppcmarit_2009_yrmo")


# transformation for each abnormal variable:
numeric_only$pphouseholdsize = log(numeric_only$pphouseholdsize) #0
numeric_only$ppt01 = 1/(numeric_only$ppt01+0.001)^2 #-2
numeric_only$duration = 1/(numeric_only$duration)#-1
numeric_only$ppt612 = 1/(numeric_only$ppt612+0.001)#-1
numeric_only$children_in_hh = 1/sqrt(numeric_only$children_in_hh+0.001)#-0.5/-1
numeric_only$q16 = log(numeric_only$q16+0.001) #0
numeric_only$how_long_ago_first_met = log(numeric_only$how_long_ago_first_met+0.001) #0
numeric_only$how_long_ago_first_romantic = log(numeric_only$how_long_ago_first_romantic+0.001) #0
numeric_only$how_long_ago_first_cohab = log(numeric_only$how_long_ago_first_cohab+0.001) #0
numeric_only$how_long_relationship = log(numeric_only$how_long_relationship+0.001) #0
numeric_only$age_difference = log(numeric_only$age_difference+0.001) #0


#View(numeric_only)

#___________________________________________________________

# Pre-process the numeric variables (center and scale)
preproc <- preProcess(numeric_only, method=c("center", "scale"))
encoded_data_processed <- predict(preproc, numeric_only)



#------------------------------------------
#STEP 3: Combine the processed numeric variables with the dummy variables and Year and Status columns

encoded_data_final <- cbind(numeric_only, dummy_cols,modified_lowdim_dataset2[, c("Status", "Year")])
#ncol(encoded_data_final)#422 cols
#_______________________________________________
# STEP 3.1: impute missing values for dummy variables as well

encoded_data_final <- as.data.frame(encoded_data_final)

for(c in 1:(ncol(encoded_data_final))) {
  median = median(encoded_data_final[,c], na.rm=TRUE)
  encoded_data_final[which(is.na(encoded_data_final[c])),c] = median
}
#___________________________________________________________

# Check the number of non-majority values with status 1
# Keep data point only if this is higher than or equal to treshold 15
keep = vector()
for(i in 1:(ncol(encoded_data_final)-3)) {
  # Check the number of non-majority values with status 1
  keep[i] = sum(rev(sort(table(encoded_data_final[which(encoded_data_final$Status == 1),i])))[-1]) >= 15
}
keep = which(keep)

#------------------------------------------

# STEP 4: split in train and test set:


set.seed(1)
encoded_data_final$Class = as.factor(paste(encoded_data_final$Status, encoded_data_final$Year, sep = "!"))
train_id = createDataPartition(encoded_data_final$Class, p = 0.70, list = FALSE)
train.x = as.matrix(encoded_data_final[train_id, keep])
test.x = encoded_data_final[-train_id, keep]

train.y = Surv(encoded_data_final[train_id,]$Year, encoded_data_final[train_id,]$Status)
test.y = Surv(encoded_data_final[-train_id,]$Year, encoded_data_final[-train_id,]$Status)


train_data <- cbind(train.x, train.y)

#ncol(train_data) #303

test_data <- cbind(test.x, encoded_data_final[-train_id,]$Year,encoded_data_final[-train_id,]$Status)
colnames(test_data)[ncol(test_data)-1] <- "time"
colnames(test_data)[ncol(test_data)] <- "status"


# -----------------------------------------------------------------------------------
# STEP 5: run lasso model with cross-validation :

cox.cv = cv.glmnet(train.x, train.y, alpha = 1, family = "cox")


# -----------------------------------------------------------------------------------
# STEP 6: use lambda min from cross-validation to run the COX PH

cox = glmnet(train.x, train.y, alpha = 1, family = "cox", lambda = cox.cv$lambda.min)


#-------------------------------
# STEP 7: get the predictor variables from lasso

coef = (coef(cox))

# Identify the indices of the selected features
selected_indices <- which(coef != 0)[-1]

# Subset the predictors matrix to include only the selected features

selected_predictors_test <- test.x[, selected_indices, drop = FALSE]
selected_predictors_train <- as.data.frame(train.x[, selected_indices, drop = FALSE])

selected_predictors_train_new <- as.data.frame(selected_predictors_train)
selected_predictors_test_new <- as.data.frame(selected_predictors_test)

selected_columns <- colnames(selected_predictors_train_new)

#View(selected_predictors_train_new)

#____________________________________


# Fit Cox regression model with selected predictors. .
cox_model_train <- coxph(train.y ~ ., data = selected_predictors_train_new)


# Print summary of the Cox regression model
summary(cox_model_train)
#__________________________________________________________

# Check PH assumption using cox.zph() function
cox_model_zph <- cox.zph(cox_model_train)
cox_model_zph


#_____________________________________
# get correct data in dataframe

selected_columns_rsf <- c(selected_columns, "time", "status")

# make sure data includes same predictor variables PLUS time and status 
train_data_lasso <- train_data[, c(selected_columns_rsf)]
test_data_lasso <- test_data[, c(selected_columns_rsf)]

# delete columns because of correlation of 1.
test_data_lasso <- subset(test_data_lasso, select = -c(s1.2, q34.4, q34.3))
train_data_lasso <- subset(train_data_lasso, select = -c(s1.2, q34.4, q34.3))

# # 
# ncol(train_data_lasso) # 33
# ncol(test_data_lasso) # 33

# #
# write.csv(train_data_lasso, file = "train_data_lasso_new2.csv", row.names = FALSE)
# write.csv(test_data_lasso, file = "test_data_lasso_new2.csv", row.names = FALSE)



