

# Load libraries
library(SurvMetrics)
library(caret)
library(randomForestSRC)
library(survival)
library(pec)


setwd("C:/tilburg university/2022-2023/Thesis/")

# Load data
train_data_lasso = read.csv("train_data_lasso_new2.csv")
test_data_lasso = read.csv("test_data_lasso_new2.csv")
surv_test <- read.csv("surv_new_test_1.csv")


View(test_data_lasso)
ncol(train_data_lasso)

# -------------------------------------------------------------------------




# Run models on train data sets with selected predictors from LASSO
mod.rsf = rfsrc(Surv(time,status)~., data = train_data_lasso, ntree = 600, importance = TRUE)
mod.cox = coxph(Surv(time, status) ~ ., data = train_data_lasso, x = TRUE)



# check ph assumption
cox_model_zph <- cox.zph(mod.cox)
cox_model_zph


# Make predictions for the test data
mat.rsf = predict(mod.rsf, test_data_lasso)$survival
dis.time = mod.rsf$time.interest
mat.cox = predictSurvProb(mod.cox, test_data_lasso, dis.time)
mat.surv <- t(surv_test)



# Define index of median time point
med_index = 4

# Define observed values in the test set
y.obs = Surv(test_data_lasso$time, test_data_lasso$status)

dim(test_data_lasso)

# Calculate C-indices
Cindex(y.obs, predicted = mat.cox[, 4])
# 0.79132 
Cindex(y.obs, predicted = mat.rsf[, 4])
# # 0.795166   
Cindex(y.obs, predicted = mat.surv[, 4])
# 0.780 

# Calculate Brier scores for each time point
brier.cox = vector()
for(i in 1:length(dis.time)) {
  brier.cox[i] = Brier(y.obs, mat.cox[,i], dis.time[i])
}
names(brier.cox) = dis.time
brier.cox
# 2010     2011     2013     2014     2015     2017 
# 0.026470 0.075600 0.095783 0.108550 0.113664 3.386893 

brier.rsf = vector()
for(i in 1:length(dis.time)) {
  brier.rsf[i] = Brier(y.obs, mat.rsf[,i], dis.time[i])
}
names(brier.rsf) = dis.time
brier.rsf
# 2010     2011     2013     2014     2015     2017 
# 0.026421 0.075265 0.093951 0.105472 0.111112 3.149421

brier.surv = vector()
for(i in 1:length(dis.time)) {
  brier.surv[i] = Brier(y.obs, mat.surv[,i], dis.time[i])
}
names(brier.surv) = dis.time
brier.surv
# 2010      2011      2013      2014      2015      2017 
# 0.006728 0.075296 0.105734 0.117053 0.125589 1.499611 

# Calculate integrated Brier scores (IBS)
IBS(y.obs, mat.cox, dis.time)
# 0.087 
IBS(y.obs, mat.rsf, dis.time)
# 0.086 
IBS(y.obs, mat.surv, dis.time)
# 0.090  

#___________________________________________________________________________

# variable importance for RSF model

var_importance <- mod.rsf$importance

# Sort the variable importance in descending order
sorted_importance <- sort(var_importance, decreasing = TRUE)

# Get the top 5 most important predictors
top_5_predictors <- names(sorted_importance)[1:5]

# Create a dataframe with the top 5 predictors and their corresponding importance values
top_5_df <- data.frame(Predictor = top_5_predictors, Importance = sorted_importance[1:5])


write.csv(top_5_df, file = "top_5_predictors.csv", row.names = FALSE)

