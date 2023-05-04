# all code used

install.packages("installr") # install the installr package if you haven't already
library(installr)
updateR() # follow the prompts to update to the latest R version
# reduce high dimension file ---------------------------------------------------

# Load necessary libraries
library(dplyr)
library(readr)
library(survival)
library(glmnet)
library(tidyr)
library(caret)
library(Hmisc)
library(dplyr)

setwd("C:/tilburg university/2022-2023/Thesis/")
# Load dataset
dataset <- read_csv("C:/tilburg university/2022-2023/Thesis/df_survival_RCode.csv")
#View(dataset)
# Define columns to keep
keep_cols <- c("w4_quality")

# Remove columns with more than 20% NA values
na_perc <- sapply(dataset, function(x) mean(is.na(x))*100)
cols_to_remove <- names(na_perc[na_perc > 20 & !(names(na_perc) %in% keep_cols)])
lowdim_dataset <- dataset %>% select(-one_of(cols_to_remove))
#View(lowdim_dataset)
# # Check variance of each column
# for (col1 in colnames(modified_lowdim_dataset2)) {
#   if (var(modified_lowdim_dataset2[[col1]]) == 0) {
#     print(paste(col1, "has variance of 0"))
#   }
# }

# # delete these columns since the variance is 0 (same value everywhere = no predictive power)
columns_to_exclude <- c("irb_consent", "qflag", "q24_fam_cousins_passive", "q24_fam_aunt_niece_passive", "q24_fam_uncle_nephew_passive", "q24_fam_grandmother_passive", "q24_fam_grandfather_passive", "pp2_igdr2")
lowdim_dataset_2 <- lowdim_dataset[, setdiff(names(lowdim_dataset), columns_to_exclude)]
#"q24_fam_cousins_passive" %in% names(lowdim_dataset_2)

#View(lowdim_dataset_2)

# Write low-dimensional dataset to CSV file
#write.csv(lowdim_dataset_2, file = "lowdim_dataset_2.csv", row.names = FALSE)


# cat_columns file --------------------------------------------------------

lowdim_dataset <- read.csv("lowdim_dataset_2.csv")
#View(lowdim_dataset)


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
    # add the matching column name to the list
    cat_columns <- append(cat_columns, namecol)
    modified_lowdim_dataset[[namecol]] <- as.factor(modified_lowdim_dataset[[namecol]])
    next
  }
  if (!namecol  %in% not_cat_columns){
    cat_columns <- append(cat_columns, namecol)
    modified_lowdim_dataset[[namecol]] <- as.factor(modified_lowdim_dataset[[namecol]])
  }
}


# LASSO+COC cleaned file --------------------------------------------------

modified_lowdim_dataset2 <- modified_lowdim_dataset
View(modified_lowdim_dataset2)

# delete column which are altered version of (year, status) tuple
modified_lowdim_dataset2 <- modified_lowdim_dataset2[, -which(names(modified_lowdim_dataset2) %in% c("w2345_combo_breakup","w2_broke_up","w2w3_combo_breakup"))]
#View(modified_lowdim_dataset2)

# ----------------------------------------------------------

# STEP 1: Dummy coding


#check for cat vars in dataframe: 
cat_vars <- sapply(modified_lowdim_dataset2, is.factor)
# Exclude Status and Year from the list of categorical columns
cat_cols <- names(cat_vars)[cat_vars & !names(cat_vars) %in% c("Status", "Year")]


# Check for columns with multiple levels and combine infrequent levels
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
dummy_transform <- dummyVars(~ ., data = modified_lowdim_dataset2[, cat_cols],fulllRank = TRUE)

# Use predict() function to create dummy variables on modified dataset
dummy_cols <- predict(dummy_transform, newdata = modified_lowdim_dataset2[, cat_cols])
#View(dummy_cols) #865 cols
#________________________________________________________________________
# STEP 2: preprocess numeric values excluding year and status:

numeric_only <- modified_lowdim_dataset2[, -which(names(modified_lowdim_dataset2) %in% c(cat_cols,"Year.1", "Status.1","caseid_new","Year","Status"))]
#View(numeric_only)

#_______________________________________________
# STEP 2.1: impute missing values for numeric only cols (NOT THE DUMMY VARIABLES)
for(c in 1:(ncol(numeric_only))) {
  median = median(numeric_only[,c], na.rm=TRUE)
  numeric_only[which(is.na(numeric_only[c])),c] = median
}
#___________________________________________________________
#transformation

abnormal_columns = c("pphouseholdsize","ppt01","duration","ppt612","children_in_hh","q16","how_long_ago_first_met",
                     "how_long_ago_first_romantic","how_long_ago_first_cohab","how_long_relationship","age_difference","pp2_ppt1317","pp2_ppt18ov",
                     "pp4_ppt612","pp4_ppt1317","pp4_ppt18ov","pp5_ppt01","pp5_ppt25","pp5_ppt612","pp5_ppt1317","pp5_ppt18ov","ppcmarit_2009_yrmo")

library(MASS)


#boxcox(numeric_only$ppcmarit_2009_yrmo ~ 1)


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
numeric_only$pp2_ppt1317 = 1/(numeric_only$pp2_ppt1317+0.001) #-1
numeric_only$pp2_ppt18ov = log(numeric_only$pp2_ppt18ov) #0
numeric_only$pp4_ppt612 = 1/(numeric_only$pp4_ppt612+0.001) #-1
numeric_only$pp4_ppt1317 = 1/(numeric_only$pp4_ppt1317+0.001)#-1
numeric_only$pp4_ppt18ov = log(numeric_only$pp4_ppt18ov)#0
numeric_only$pp5_ppt01 = 1/(numeric_only$pp5_ppt01+0.001)^2 #-2
numeric_only$pp5_ppt25 = 1/(numeric_only$pp5_ppt25+0.001)^2 #-2
numeric_only$pp5_ppt612 = 1/(numeric_only$pp5_ppt612+0.001) #-1
numeric_only$pp5_ppt1317 = 1/(numeric_only$pp5_ppt1317+0.001) #-1
numeric_only$pp5_ppt18ov = log(numeric_only$pp5_ppt18ov)#0




#___________________________________________________________
#View(numeric_only)
# Pre-process the numeric variables (center and scale)
preproc <- preProcess(numeric_only, method=c("center", "scale"))
encoded_data_processed <- predict(preproc, numeric_only)

#--------------------
#Distribution check and transformation for numeric variables only
#___________________________________
# histrogram:
# pdf("histograms_num2.pdf", width = 50, height = 50)
# 
# par(mfrow = c(11, 11)) # set up the plot layout with 6 rows and 6 columns
# for (i in 1:74) { # loop through the columns 
#   hist(numeric_only[, i], main = colnames(numeric_only)[i], xlab = "Value")
# }
# 
# dev.off()
#___________________________________



#------------------------------------------
#STEP 3: Combine the processed numeric variables with the categorical variables and Year and Status columns

encoded_data_final <- cbind(numeric_only, dummy_cols,modified_lowdim_dataset2[, c("Status", "Year")])
#View(encoded_data_final)
#_______________________________________________
# STEP 3.1: impute missing values for dummy variables as well
for(c in 1:(ncol(encoded_data_final))) {
  median = median(encoded_data_final[,c], na.rm=TRUE)
  encoded_data_final[which(is.na(encoded_data_final[c])),c] = median
}
#___________________________________________________________


# ---------------------------------------------------------------------------
# Check the number of non-majority values with status 1
# Keep data point only if this is higher than or equal to some threshold, for instance 20
keep = vector()
for(i in 1:(ncol(encoded_data_final)-3)) {
  # Check the number of non-majority values with status 1
  keep[i] = sum(rev(sort(table(encoded_data_final[which(encoded_data_final$Status == 1),i])))[-1]) >= 20
}
keep = which(keep)

#------------------------------------------

# STEP 4: split in train and test set:
View(encoded_data_final)

#write.csv(encoded_data_final, file = "encoded_data_final.csv", row.names = FALSE)

set.seed(1)
encoded_data_final$Class = as.factor(paste(encoded_data_final$Status, encoded_data_final$Year, sep = "!"))
train_id = createDataPartition(encoded_data_final$Class, p = 0.70, list = FALSE)
train.x = as.matrix(encoded_data_final[train_id, keep])
test.x = encoded_data_final[-train_id, keep]

train.y = Surv(encoded_data_final[train_id,]$Year, encoded_data_final[train_id,]$Status)
test.y = Surv(encoded_data_final[-train_id,]$Year, encoded_data_final[-train_id,]$Status)

#ncol(train.x)

train_data <- cbind(train.x, train.y)

#View(train.y)
test_data <- cbind(test.x, encoded_data_final[-train_id,]$Year,encoded_data_final[-train_id,]$Status)
colnames(test_data)[ncol(test_data)-1] <- "time"
colnames(test_data)[ncol(test_data)] <- "status"

#View(test_data)
# write.csv(train_data, file = "train_data.csv", row.names = FALSE)
# write.csv(test_data, file = "test_data.csv", row.names = FALSE)

# -----------------------------------------------------------------------------------
# STEP 5: run lasso model with cross-validation (MATRIX IS NEEDED FOR GLMNET):

cox.cv = cv.glmnet(train.x, train.y, alpha = 1, family = "cox")


# -----------------------------------------------------------------------------------
# STEP 6: use lambda min from cross-validation to run the COX PH

cox = glmnet(train.x, train.y, alpha = 1, family = "cox", lambda = cox.cv$lambda.1se)


#-------------------------------
# STEP 7: get the predictor variables from lasso

coef = (coef(cox))

# Identify the indices of the selected features
selected_indices <- which(coef != 0)[-1]

# Subset the predictors matrix to include only the selected features

selected_predictors_test <- test.x[, selected_indices, drop = FALSE]
selected_predictors_train <- as.data.frame(train.x[, selected_indices, drop = FALSE])

# delete column which look like (status,year)
cols_to_remove <- grep("_xpartnered\\s*|_xqualified\\s*|x_qualified\\s*|_complete\\s*|f1complete\\s*|status\\s*|ppmarit\\s*", 
                       colnames(selected_predictors_train))
selected_predictors_train_new <- subset(selected_predictors_train, select = -cols_to_remove)
selected_predictors_test_new <- subset(selected_predictors_test, select = -cols_to_remove)

selected_predictors_train_new <- as.data.frame(selected_predictors_train_new)
selected_predictors_test_new <- as.data.frame(selected_predictors_test_new)

selected_columns <- colnames(selected_predictors_train_new)

ncol(selected_predictors_train_new)
ncol(selected_predictors_test_new)
#__________________________________________
# check highly correlated
# Compute the correlation matrix of the selected predictors
# cor_matrix <- cor(selected_predictors)
# 
# # Find the indices of the highly correlated variables
# highly_correlated <- findCorrelation(cor_matrix, cutoff = 0.8)
# 
# # Get the names of the highly correlated variables
# names(selected_predictors)[highly_correlated]
#_______________________________________________
#STEP 8: run cox model

#check cor() __________________________________
#cor <- cor(selected_predictors_train_df)
# print(cor)

# selected_predictors_train_df <- selected_predictors_train_df[, !colnames(selected_predictors_train_df) %in% "weight3"]
# print(ncol(selected_predictors_train_df))
#____________________________________


# Fit Cox regression model with selected predictors. to do: used train instead of test.
cox_model_train <- coxph(train.y ~ ., data = selected_predictors_train_new)
cox_model_test <- coxph(test.y ~ ., data = selected_predictors_test_new)


# Print summary of the Cox regression model
summary(cox_model_train)
summary(cox_model_test)
#__________________________________________________________

# Check PH assumption using cox.zph() function
cox_model_zph <- cox.zph(cox_model_train)
cox_model_zph

table(encoded_data_final$Status, encoded_data_final$relationship_quality.5)
table(encoded_data_final$Status, encoded_data_final$w4_xmarry.1)
table(encoded_data_final$Status, encoded_data_final$w4_xmarry.2)

class(encoded_data_final$w4_xmarry.2)
View(encoded_data_final)

# RSF Model ---------------------------------------------------------------


library(randomForestSRC)

# get correct data in dataframe
View(train_data)
selected_columns_rsf <- c(selected_columns, "time", "status")

train_data_lasso <- as.data.frame(train_data[, c(selected_columns_rsf)])
test_data_lasso <- as.data.frame(test_data[, c(selected_columns_rsf)])
test_data_subset <- subset(test_data_lasso, select = -c(time, status))
View(train_data_lasso)

nrow(test_data_subset)
nrow(test_data_lasso)

# Fit the RSF model with train data were time and status are included ?! to do

rsf_model <- rfsrc(Surv(time, status) ~ ., data = train_data_lasso, ntree = 100, nodesize = 5)

summary(rsf_model)


# option 1 __________________________________________________
library(pec)
mat_rsf = predict(rsf_model, test_data_lasso)$survival

dis_time = rsf_model$time.interest

#calculate the C index
#med_index = median(1:length(dis_time))
med_index = 2
surv_obj = Surv(test_data$time, test_data$status)

#C index for RSF
metrics_rsf[i] = Cindex(surv_obj, predicted = mat_rsf[, med_index])


t_star = median(rsf_model$time.interest)

#Brier Score for RSF
metrics_rsf[i] = Brier(surv_obj, pre_sp = mat_rsf[, med_index], t_star)
library(ibs)

# Install the devtools package
install.packages("devtools")

# Install the development version of the ibs package from GitHub
devtools::install_github("drizopoulos/ibs")
#IBS for RSF
metrics_rsf[i] = ibs::IBS(surv_obj, sp_matrix = mat_rsf, dis_time)


# Calculate the Brier score
brier_rsf <- pec(Surv(test_data_lasso$time, test_data_lasso$status), mat_rsf, dis_time)

# option 2________________________________________________________________
library(Hmisc)
library(pec)

# Make predictions on the test set

test_pred <- predict(rsf_model, newdata = test_data_subset, type = "risk")
print(test_pred)

# Calculate the c-index
cindex <- rcorr.cens(test_pred, test_data_lasso$time, test_data_lasso$status)$Cindex

#of

cindex2 <- concordance(Surv(time, status) ~predict(rsf_model), test_data_lasso)

# Print the c-index
cat("C-index:", cindex)


# compute the Brier score for the RSF model predictions
brier_score <- pec::brier_score(test_pred, test_data_lasso$time, test_data_lasso$status)

# print the Brier score
print(brier_score)



