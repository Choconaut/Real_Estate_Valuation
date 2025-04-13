# Data Preprocessing

# Importing the dataset
dataset = read.csv('Stores.csv')
# Remove Store ID Column
dataset = dataset[, 2:5]

# Splitting the dataset into the Training set and Test set
library(caTools)
split = sample.split(dataset$Store_Sales, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
training_scaled_cols = scale(training_set[, 1:4])
training_set[, 1:4] = training_scaled_cols
test_set[, 1:4] = scale(test_set[, 1:4], center=attr(training_scaled_cols, 'scaled:center'),
                        scale=attr(training_scaled_cols, 'scaled:scale'))

# ------------------------------------------------------------------------------
# Multiple Linear Regression
# ------------------------------------------------------------------------------

# Splitting the dataset into the Training set and Test set
library(caTools)
split = sample.split(dataset$Store_Sales, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

#Building the optimal model using Backward Elimination
mlr_regressor_opt = lm(formula = Store_Sales ~ .,
                       data = training_set)
summary(mlr_regressor_opt)
mlr_regressor_opt = lm(formula = Store_Sales ~ Store_Area + Items_Available,
                       data = training_set)
summary(mlr_regressor_opt)
mlr_regressor_opt = lm(formula = Store_Sales ~ Items_Available,
                       data = training_set)
summary(mlr_regressor_opt)
# Optimal Team of Variables: Items_Available

# Evaluating Model Performance
mlr_sum = 0
num_of_ind_vars = 1

for (x in 1:10) {
  split = sample.split(dataset$Store_Sales, SplitRatio = 0.8)
  training_set = subset(dataset, split == TRUE)
  test_set = subset(dataset, split == FALSE)
  
  mlr_regressor = lm(formula = Store_Sales ~ Items_Available,
                     data = training_set)
  mlr_y_pred = predict(mlr_regressor, newdata = test_set)
  mlr_ssr = sum((test_set$Store_Sales - mlr_y_pred) ^ 2)
  mlr_sst = sum((test_set$Store_Sales - mean(test_set$Store_Sales)) ^ 2)
  mlr_r2 = 1 - (mlr_ssr/mlr_sst)
  mlr_r2_adjusted = 1 - (1 - mlr_r2) * (length(test_set$Store_Sales) - 1) / (length(test_set$Store_Sales) - num_of_ind_vars - 1)
  mlr_sum = mlr_sum + mlr_r2
}
mlr_avg = mlr_sum/10
print(paste("Average R2: ", mlr_avg))

# ------------------------------------------------------------------------------
# Random Forest Regression
# ------------------------------------------------------------------------------
library(randomForest)
rf_regressor = randomForest(formula = Y ~ .,
                            data = training_set,
                            ntree = 500)
rf_y_pred = predict(rf_regressor, newdata = test_set)

# Evaluating the Model Performance
rf_sum = 0

rf_ssr = sum((test_set$Y - rf_y_pred)^2)
rf_sst = sum((test_set$Y - mean(test_set$Y))^2)
rf_r2 = 1 - rf_ssr/rf_sst
print(rf_r2)
rf_r2_adjusted = 1 - (1 - rf_r2) * (length(test_set$Y) - 1) / (length(test_set$Y) - ncol(test_set) - 1)
print(rf_r2_adjusted)
rf_sum = rf_r2_adjusted + rf_sum

# 2nd Run
split = sample.split(dataset$Y, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

training_scaled_cols = scale(training_set[, 1:6])
training_set[, 1:6] = training_scaled_cols
test_set[, 1:6] = scale(test_set[, 1:6], center=attr(training_scaled_cols, 'scaled:center'),
                        scale=attr(training_scaled_cols, 'scaled:scale'))

rf_regressor = randomForest(formula = Y ~ .,
                            data = training_set,
                            ntree = 500)
rf_y_pred = predict(rf_regressor, newdata = test_set)

rf_ssr = sum((test_set$Y - rf_y_pred)^2)
rf_sst = sum((test_set$Y - mean(test_set$Y))^2)
rf_r2 = 1 - rf_ssr/rf_sst
print(rf_r2)
rf_r2_adjusted = 1 - (1 - rf_r2) * (length(test_set$Y) - 1) / (length(test_set$Y) - ncol(test_set) - 1)
print(rf_r2_adjusted)
rf_sum = rf_r2_adjusted + rf_sum

# 3rd Run
split = sample.split(dataset$Y, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

training_scaled_cols = scale(training_set[, 1:6])
training_set[, 1:6] = training_scaled_cols
test_set[, 1:6] = scale(test_set[, 1:6], center=attr(training_scaled_cols, 'scaled:center'),
                        scale=attr(training_scaled_cols, 'scaled:scale'))

rf_regressor = randomForest(formula = Y ~ .,
                            data = training_set,
                            ntree = 500)
rf_y_pred = predict(rf_regressor, newdata = test_set)

rf_ssr = sum((test_set$Y - rf_y_pred)^2)
rf_sst = sum((test_set$Y - mean(test_set$Y))^2)
rf_r2 = 1 - rf_ssr/rf_sst
print(rf_r2)
rf_r2_adjusted = 1 - (1 - rf_r2) * (length(test_set$Y) - 1) / (length(test_set$Y) - ncol(test_set) - 1)
print(rf_r2_adjusted)
rf_sum = rf_r2_adjusted + rf_sum

# 4th Run
split = sample.split(dataset$Y, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

training_scaled_cols = scale(training_set[, 1:6])
training_set[, 1:6] = training_scaled_cols
test_set[, 1:6] = scale(test_set[, 1:6], center=attr(training_scaled_cols, 'scaled:center'),
                        scale=attr(training_scaled_cols, 'scaled:scale'))

rf_regressor = randomForest(formula = Y ~ .,
                            data = training_set,
                            ntree = 500)
rf_y_pred = predict(rf_regressor, newdata = test_set)

rf_ssr = sum((test_set$Y - rf_y_pred)^2)
rf_sst = sum((test_set$Y - mean(test_set$Y))^2)
rf_r2 = 1 - rf_ssr/rf_sst
print(rf_r2)
rf_r2_adjusted = 1 - (1 - rf_r2) * (length(test_set$Y) - 1) / (length(test_set$Y) - ncol(test_set) - 1)
print(rf_r2_adjusted)
rf_sum = rf_r2_adjusted + rf_sum

# 5th Run
split = sample.split(dataset$Y, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

training_scaled_cols = scale(training_set[, 1:6])
training_set[, 1:6] = training_scaled_cols
test_set[, 1:6] = scale(test_set[, 1:6], center=attr(training_scaled_cols, 'scaled:center'),
                        scale=attr(training_scaled_cols, 'scaled:scale'))

rf_regressor = randomForest(formula = Y ~ .,
                            data = training_set,
                            ntree = 500)
rf_y_pred = predict(rf_regressor, newdata = test_set)

rf_ssr = sum((test_set$Y - rf_y_pred)^2)
rf_sst = sum((test_set$Y - mean(test_set$Y))^2)
rf_r2 = 1 - rf_ssr/rf_sst
print(rf_r2)
rf_r2_adjusted = 1 - (1 - rf_r2) * (length(test_set$Y) - 1) / (length(test_set$Y) - ncol(test_set) - 1)
print(rf_r2_adjusted)
rf_sum = rf_r2_adjusted + rf_sum

# 6th Run
split = sample.split(dataset$Y, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

training_scaled_cols = scale(training_set[, 1:6])
training_set[, 1:6] = training_scaled_cols
test_set[, 1:6] = scale(test_set[, 1:6], center=attr(training_scaled_cols, 'scaled:center'),
                        scale=attr(training_scaled_cols, 'scaled:scale'))

rf_regressor = randomForest(formula = Y ~ .,
                            data = training_set,
                            ntree = 500)
rf_y_pred = predict(rf_regressor, newdata = test_set)

rf_ssr = sum((test_set$Y - rf_y_pred)^2)
rf_sst = sum((test_set$Y - mean(test_set$Y))^2)
rf_r2 = 1 - rf_ssr/rf_sst
print(rf_r2)
rf_r2_adjusted = 1 - (1 - rf_r2) * (length(test_set$Y) - 1) / (length(test_set$Y) - ncol(test_set) - 1)
print(rf_r2_adjusted)
rf_sum = rf_r2_adjusted + rf_sum

# 7th Run
split = sample.split(dataset$Y, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

training_scaled_cols = scale(training_set[, 1:6])
training_set[, 1:6] = training_scaled_cols
test_set[, 1:6] = scale(test_set[, 1:6], center=attr(training_scaled_cols, 'scaled:center'),
                        scale=attr(training_scaled_cols, 'scaled:scale'))

rf_regressor = randomForest(formula = Y ~ .,
                            data = training_set,
                            ntree = 500)
rf_y_pred = predict(rf_regressor, newdata = test_set)

rf_ssr = sum((test_set$Y - rf_y_pred)^2)
rf_sst = sum((test_set$Y - mean(test_set$Y))^2)
rf_r2 = 1 - rf_ssr/rf_sst
print(rf_r2)
rf_r2_adjusted = 1 - (1 - rf_r2) * (length(test_set$Y) - 1) / (length(test_set$Y) - ncol(test_set) - 1)
print(rf_r2_adjusted)
rf_sum = rf_r2_adjusted + rf_sum

# 8th Run
split = sample.split(dataset$Y, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

training_scaled_cols = scale(training_set[, 1:6])
training_set[, 1:6] = training_scaled_cols
test_set[, 1:6] = scale(test_set[, 1:6], center=attr(training_scaled_cols, 'scaled:center'),
                        scale=attr(training_scaled_cols, 'scaled:scale'))

rf_regressor = randomForest(formula = Y ~ .,
                            data = training_set,
                            ntree = 500)
rf_y_pred = predict(rf_regressor, newdata = test_set)

rf_ssr = sum((test_set$Y - rf_y_pred)^2)
rf_sst = sum((test_set$Y - mean(test_set$Y))^2)
rf_r2 = 1 - rf_ssr/rf_sst
print(rf_r2)
rf_r2_adjusted = 1 - (1 - rf_r2) * (length(test_set$Y) - 1) / (length(test_set$Y) - ncol(test_set) - 1)
print(rf_r2_adjusted)
rf_sum = rf_r2_adjusted + rf_sum

# 9th Run
split = sample.split(dataset$Y, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

training_scaled_cols = scale(training_set[, 1:6])
training_set[, 1:6] = training_scaled_cols
test_set[, 1:6] = scale(test_set[, 1:6], center=attr(training_scaled_cols, 'scaled:center'),
                        scale=attr(training_scaled_cols, 'scaled:scale'))

rf_regressor = randomForest(formula = Y ~ .,
                            data = training_set,
                            ntree = 500)
rf_y_pred = predict(rf_regressor, newdata = test_set)

rf_ssr = sum((test_set$Y - rf_y_pred)^2)
rf_sst = sum((test_set$Y - mean(test_set$Y))^2)
rf_r2 = 1 - rf_ssr/rf_sst
print(rf_r2)
rf_r2_adjusted = 1 - (1 - rf_r2) * (length(test_set$Y) - 1) / (length(test_set$Y) - ncol(test_set) - 1)
print(rf_r2_adjusted)
rf_sum = rf_r2_adjusted + rf_sum

# 10th Run
split = sample.split(dataset$Y, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

training_scaled_cols = scale(training_set[, 1:6])
training_set[, 1:6] = training_scaled_cols
test_set[, 1:6] = scale(test_set[, 1:6], center=attr(training_scaled_cols, 'scaled:center'),
                        scale=attr(training_scaled_cols, 'scaled:scale'))

rf_regressor = randomForest(formula = Y ~ .,
                            data = training_set,
                            ntree = 500)
rf_y_pred = predict(rf_regressor, newdata = test_set)

rf_ssr = sum((test_set$Y - rf_y_pred)^2)
rf_sst = sum((test_set$Y - mean(test_set$Y))^2)
rf_r2 = 1 - rf_ssr/rf_sst
print(rf_r2)
rf_r2_adjusted = 1 - (1 - rf_r2) * (length(test_set$Y) - 1) / (length(test_set$Y) - ncol(test_set) - 1)
print(rf_r2_adjusted)
rf_sum = rf_r2_adjusted + rf_sum

# Average R2
rf_avg = rf_sum / 10
print(paste("Average R2: ", rf_avg))
