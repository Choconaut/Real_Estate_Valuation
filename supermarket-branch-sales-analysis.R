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
rf_sum = 0

# Function for calling random forest regression
random_forest <- function() {
  # Splitting the dataset into the Training set and Test set
  split = sample.split(dataset$Store_Sales, SplitRatio = 0.8)
  training_set = subset(dataset, split == TRUE)
  test_set = subset(dataset, split == FALSE)
  
  # Feature Scaling
  training_scaled_cols = scale(training_set[, 1:4])
  training_set[, 1:4] = training_scaled_cols
  test_set[, 1:4] = scale(test_set[, 1:4], center=attr(training_scaled_cols, 'scaled:center'),
                          scale=attr(training_scaled_cols, 'scaled:scale'))
  
  library(randomForest)
  rf_regressor = randomForest(formula = Store_Sales ~ .,
                              data = training_set,
                              ntree = 500)
  rf_y_pred = predict(rf_regressor, newdata = test_set)
  
  # Evaluating the Model Performance
  rf_ssr = sum((test_set$Store_Sales - rf_y_pred)^2)
  rf_sst = sum((test_set$Store_Sales - mean(test_set$Store_Sales))^2)
  rf_r2 = 1 - rf_ssr/rf_sst
  print(paste("R2: ", rf_r2))
  rf_r2_adjusted = 1 - (1 - rf_r2) * (length(test_set$Store_Sales) - 1) / (length(test_set$Store_Sales) - ncol(test_set) - 1)
  print(paste("Adjusted R2: ", rf_r2_adjusted))
  return(rf_r2_adjusted)
}

# Average R2
for (x in 1:10) {
  rf_sum = rf_sum + random_forest()
}
rf_avg = rf_sum / 10
print(paste("Average R2: ", rf_avg))

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
rf_sum = 0

# Function for calling random forest regression
random_forest <- function() {
  # Splitting the dataset into the Training set and Test set
  split = sample.split(dataset$Store_Sales, SplitRatio = 0.8)
  training_set = subset(dataset, split == TRUE)
  test_set = subset(dataset, split == FALSE)
  
  # Feature Scaling
  training_scaled_cols = scale(training_set[, 1:4])
  training_set[, 1:4] = training_scaled_cols
  test_set[, 1:4] = scale(test_set[, 1:4], center=attr(training_scaled_cols, 'scaled:center'),
                          scale=attr(training_scaled_cols, 'scaled:scale'))
  
  library(randomForest)
  rf_regressor = randomForest(formula = Store_Sales ~ .,
                              data = training_set,
                              ntree = 500)
  rf_y_pred = predict(rf_regressor, newdata = test_set)
  
  # Evaluating the Model Performance
  rf_ssr = sum((test_set$Store_Sales - rf_y_pred)^2)
  rf_sst = sum((test_set$Store_Sales - mean(test_set$Store_Sales))^2)
  rf_r2 = 1 - rf_ssr/rf_sst
  print(paste("R2: ", rf_r2))
  rf_r2_adjusted = 1 - (1 - rf_r2) * (length(test_set$Store_Sales) - 1) / (length(test_set$Store_Sales) - ncol(test_set) - 1)
  print(paste("Adjusted R2: ", rf_r2_adjusted))
  return(rf_r2_adjusted)
}

# Average R2
for (x in 1:10) {
  rf_sum = rf_sum + random_forest()
}
rf_avg = rf_sum / 10
print(paste("Average R2: ", rf_avg))

# ------------------------------------------------------------------------------
# Polynomial Regression
# ------------------------------------------------------------------------------



# Creating polynomial features (e.g., degree 2)
training_set$Store_Area2 = training_set$Store_Area^2
training_set$Items_Available2 = training_set$Items_Available^2
training_set$Daily_Customer_Count2 = training_set$Daily_Customer_Count^2

# Fitting Polynomial Regression model
regressor = lm(formula = Store_Sales ~ Store_Area + Store_Area2 +
                  Items_Available + Items_Available2 +
                  Daily_Customer_Count + Daily_Customer_Count2,
                data = training_set)

# Summary to evaluate p-values
summary(regressor)

#Started this, unfinished because the p values are terrible

# ------------------------------------------------------------------------------
# SVR
# ------------------------------------------------------------------------------

# Feature Scaling
training_scaled_cols = scale(training_set[, 1:4])
training_set[, 1:4] = training_scaled_cols
test_set[, 1:4] = scale(test_set[, 1:4], 
                         center = attr(training_scaled_cols, 'scaled:center'),
                         scale = attr(training_scaled_cols, 'scaled:scale'))
# Fitting SVR to the training set
library(e1071)
regressor = svm(formula = Store_Sales ~ .,
                 data = training_set,
                 type = 'eps-regression',
                 kernel = 'radial')

# Predicting on the test set
svr_y_pred_scaled = predict(regressor, newdata = test_set[, 1:3])

# Inverse scaling of predictions
train_sales_mean = attr(training_scaled_cols, "scaled:center")[4]
train_sales_sd = attr(training_scaled_cols, "scaled:scale")[4]
svr_y_pred = svr_y_pred_scaled

# Calculating R² and Adjusted R²
ssr = sum((test_set$Store_Sales - svr_y_pred)^2)
sst = sum((test_set$Store_Sales - mean(test_set$Store_Sales))^2)
r2 = 1 - (ssr / sst)
adjusted_r2 = 1 - (1 - r2) * (nrow(test_set) - 1) / (nrow(test_set) - 3 - 1)

cat("R²: ", round(r2, 4), "\n")
cat("Adjusted R²: ", round(adjusted_r2, 4), "\n")




# Comparing Actual vs Predicted
comparison = data.frame(
  Actual = test_set$Store_Sales,
  Predicted = svr_y_pred
)

ggplot(comparison, aes(x = Actual, y = Predicted)) +
  geom_point(color = 'darkblue') +
  geom_abline(intercept = 0, slope = 1, color = 'red', linetype = "dashed") +
  ggtitle('Actual vs Predicted Store Sales') +
  xlab('Actual Sales') +
  ylab('Predicted Sales')


