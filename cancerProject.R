# Imports
library(caTools)
library(randomForest)
library(ggplot2)
library(e1071)
# ------------------------------------------------------------------------------
# Data Preprocessing
# ------------------------------------------------------------------------------

# Importing the dataset
dataset = read.csv('cancer_reg.csv', stringsAsFactors = FALSE)

# Taking care of missing data
#colSums(is.na(dataset))
dataset$pctemployed16_over = ifelse(is.na(dataset$pctemployed16_over),
                                    mean(dataset$pctemployed16_over, na.rm = TRUE),
                                    dataset$pctemployed16_over)
#colSums(is.na(dataset))

# Removing unneeded columns
dataset = subset(dataset, select = c(target_deathrate,
                                     medincome,
                                     povertypercent,
                                     studypercap,
                                     medianage,
                                     percentmarried,
                                     pcths25_over,
                                     pctbachdeg25_over,
                                     pctunemployed16_over,
                                     pctprivatecoverage,
                                     pctpubliccoverage,
                                     pctwhite,
                                     pctblack,
                                     pctasian))

# ------------------------------------------------------------------------------
# Multiple Linear Regression
# ------------------------------------------------------------------------------
# Splitting the dataset into the Training set and Test set
set.seed(123)
split = sample.split(dataset$target_deathrate, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Selecting the optimal team of variables using backward elimination
mlr_regressor_opt = lm(formula = target_deathrate ~ .,
                       data = training_set)
summary(mlr_regressor_opt)
# Remove medianage
training_set = subset(training_set, select = -c(medianage))
test_set = subset(test_set, select = -c(medianage))
mlr_regressor_opt = lm(formula = target_deathrate ~ .,
                       data = training_set)
summary(mlr_regressor_opt)
# Remove pctasian
training_set = subset(training_set, select = -c(pctasian))
test_set = subset(test_set, select = -c(pctasian))
mlr_regressor_opt = lm(formula = target_deathrate ~ .,
                       data = training_set)
summary(mlr_regressor_opt)
# Remove pctprivatecoverage
training_set = subset(training_set, select = -c(pctprivatecoverage))
test_set = subset(test_set, select = -c(pctprivatecoverage))
mlr_regressor_opt = lm(formula = target_deathrate ~ .,
                       data = training_set)
summary(mlr_regressor_opt)
# Remove studypercap
training_set = subset(training_set, select = -c(studypercap))
test_set = subset(test_set, select = -c(studypercap))
mlr_regressor_opt = lm(formula = target_deathrate ~ .,
                       data = training_set)
summary(mlr_regressor_opt)
# Remove pctpubliccoverage
training_set = subset(training_set, select = -c(pctpubliccoverage))
test_set = subset(test_set, select = -c(pctpubliccoverage))
mlr_regressor_opt = lm(formula = target_deathrate ~ .,
                       data = training_set)
summary(mlr_regressor_opt)
# Remove medincome
training_set = subset(training_set, select = -c(medincome))
test_set = subset(test_set, select = -c(medincome))
mlr_regressor_opt = lm(formula = target_deathrate ~ .,
                       data = training_set)
summary(mlr_regressor_opt)
# Remove temp data items
rm(mlr_regressor_opt)
rm(test_set)
rm(training_set)

# Filter dataset based on results of backwards elimination
dataset = subset(dataset, select = c(target_deathrate,
                                     povertypercent,
                                     percentmarried,
                                     pcths25_over,
                                     pctbachdeg25_over,
                                     pctemployed16_over,
                                     pctunemployed16_over,
                                     pctwhite,
                                     pctblack))

# Evaluating Model Performance
mlr_r2_sum = 0
mlr_r2_adj_sum = 0
num_of_ind_vars = 8

for (x in 1:10) {
  split = sample.split(dataset$target_deathrate, SplitRatio = 0.8)
  training_set = subset(dataset, split == TRUE)
  test_set = subset(dataset, split == FALSE)
  
  mlr_regressor = lm(formula = target_deathrate ~ .,
                     data = training_set)
  mlr_y_pred = predict(mlr_regressor, newdata = test_set)
  
  mlr_ssr = sum((test_set$target_deathrate - mlr_y_pred) ^ 2)
  mlr_sst = sum((test_set$target_deathrate - mean(test_set$target_deathrate)) ^ 2)
  
  mlr_r2 = 1 - (mlr_ssr/mlr_sst)
  mlr_r2_sum = mlr_r2_sum + mlr_r2
  mlr_r2_adj = 1 - (1 - mlr_r2) * (length(test_set$target_deathrate) - 1) / (length(test_set$target_deathrate) - num_of_ind_vars - 1)
  mlr_r2_adj_sum = mlr_r2_adj_sum + mlr_r2_adj
}
mlr_r2_avg = mlr_r2_sum/10
mlr_r2_adj_avg = mlr_r2_adj_sum/10
print(paste("Average R2: ", mlr_r2_avg))
print(paste("Averge R2 Adj: ", mlr_r2_adj_avg))


# ------------------------------------------------------------------------------
# Random Forest Regression
# ------------------------------------------------------------------------------
rf_sum = 0
set.seed(123)

# Function for calling random forest regression
random_forest <- function() {
  # Splitting the dataset into the Training set and Test set
  split = sample.split(dataset$target_deathrate, SplitRatio = 0.8)
  training_set = subset(dataset, split == TRUE)
  test_set = subset(dataset, split == FALSE)
  
  rf_regressor = randomForest(formula = target_deathrate ~ .,
                              data = training_set,
                              ntree = 500)
  rf_y_pred = predict(rf_regressor, newdata = test_set)
  
  # Evaluating the Model Performance
  rf_ssr = sum((test_set$target_deathrate - rf_y_pred)^2)
  rf_sst = sum((test_set$target_deathrate - mean(test_set$target_deathrate))^2)
  rf_r2 = 1 - rf_ssr/rf_sst
  print(paste("R2: ", rf_r2))
  rf_r2_adjusted = 1 - (1 - rf_r2) * (length(test_set$target_deathrate) - 1) / (length(test_set$target_deathrate) - ncol(test_set) - 1)
  print(paste("Adjusted R2: ", rf_r2_adjusted))
  return(rf_r2_adjusted)
}

# Average R2
for (x in 1:10) {
  rf_sum = rf_sum + random_forest()
}
rf_avg = rf_sum / 10
print(paste("Average R2: ", rf_avg))
# average r2: 0.32

# Visualizing the Random Forest Regression results

# ------------------------------------------------------------------------------
# SVR
# ------------------------------------------------------------------------------
svr_sum = 0
set.seed(123)

# Function for calling random forest regression
svr_model <- function() {
  # Splitting the dataset into the Training set and Test set
  split = sample.split(dataset$target_deathrate, SplitRatio = 0.8)
  training_set = subset(dataset, split == TRUE)
  test_set = subset(dataset, split == FALSE)
  
  svr_regressor =  svm(formula = target_deathrate ~ .,
                       data = training_set,
                       type = 'eps-regression',
                       kernel = 'radial')
  svr_y_pred = predict(svr_regressor, newdata = test_set)
  
  # Evaluating the Model Performance
  svr_ssr = sum((test_set$target_deathrate - svr_y_pred)^2)
  svr_sst = sum((test_set$target_deathrate - mean(test_set$target_deathrate))^2)
  svr_r2 = 1 - svr_ssr/svr_sst
  print(paste("R2: ", svr_r2))
  svr_r2_adjusted = 1 - (1 - svr_r2) * (length(test_set$target_deathrate) - 1) / (length(test_set$target_deathrate) - ncol(test_set) - 1)
  print(paste("Adjusted R2: ", svr_r2_adjusted))
  return(svr_r2_adjusted)
}

# Average R2
for (x in 1:10) {
  svr_sum = svr_sum + svr_model()
}
svr_avg = svr_sum / 10
print(paste("Average R2: ", svr_avg))


#Average R2: 0.2890264




