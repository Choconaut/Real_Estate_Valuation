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
                                     pctemployed16_over,
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
library(caTools)
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

# Function for calling random forest regression
random_forest <- function() {
  # Splitting the dataset into the Training set and Test set
  split = sample.split(dataset$avganncount, SplitRatio = 0.8)
  training_set = subset(dataset, split == TRUE)
  test_set = subset(dataset, split == FALSE)
  
  # Feature Scaling
  training_scaled_cols = scale(training_set[, 1:31])
  training_set[, 1:31] = training_scaled_cols
  test_set[, 1:31] = scale(test_set[, 1:31], center=attr(training_scaled_cols, 'scaled:center'),
                           scale=attr(training_scaled_cols, 'scaled:scale'))
  
  library(randomForest)
  rf_regressor = randomForest(formula = avganncount ~ .,
                              data = training_set,
                              ntree = 500)
  rf_y_pred = predict(rf_regressor, newdata = test_set)
  
  # Evaluating the Model Performance
  rf_ssr = sum((test_set$avganncount - rf_y_pred)^2)
  rf_sst = sum((test_set$avganncount - mean(test_set$avganncount))^2)
  rf_r2 = 1 - rf_ssr/rf_sst
  print(paste("R2: ", rf_r2))
  rf_r2_adjusted = 1 - (1 - rf_r2) * (length(test_set$avganncount) - 1) / (length(test_set$avganncount) - ncol(test_set) - 1)
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
# SVR
# ------------------------------------------------------------------------------

# Fitting SVR to the training set
library(e1071)
regressor = svm(formula = avganncount ~ .,
                data = training_set,
                type = 'eps-regression',
                kernel = 'radial')

# Predicting on the test set
svr_y_pred_scaled = predict(regressor, newdata = test_set[, 1:31])

# Inverse scaling of predictions
target_center = attr(training_scaled_cols, "scaled:center")[1]
target_scale = attr(training_scaled_cols, "scaled:scale")[1]
svr_y_pred = svr_y_pred_scaled * target_scale + target_center
y_actual_scaled <- test_set$avganncount
y_actual <- y_actual_scaled * target_scale + target_center

# Calculating R² and Adjusted R²
ssr = sum((test_set$avganncount - svr_y_pred)^2)
sst = sum((test_set$avganncount - mean(test_set$avganncount))^2)
r2 = 1 - (ssr / sst)
adjusted_r2 = 1 - (1 - r2) * (nrow(test_set) - 1) / (nrow(test_set) - 3 - 1)

cat("R²: ", round(r2, 4), "\n")
cat("Adjusted R²: ", round(adjusted_r2, 4), "\n")

summary(test_set$avganncount)
summary(svr_y_pred_scaled)

# # Comparing Actual vs Predicted
# comparison = data.frame(
#   Actual = test_set$avganncount,
#   Predicted = svr_y_pred
# )
# 
# ggplot(comparison, aes(x = Actual, y = Predicted)) +
#   geom_point(color = 'darkblue') +
#   geom_abline(intercept = 0, slope = 1, color = 'red', linetype = "dashed") +
#   ggtitle('Actual vs Predicted Store Sales') +
#   xlab('Actual Sales') +
#   ylab('Predicted Sales')


# ------------------------------------------------------------------------------
# Polynomial Regression
# ------------------------------------------------------------------------------



# Creating polynomial features (e.g., degree 2)
training_set$avgdeathsperyear2 = training_set$avgdeathsperyear^2
training_set$target_deathrate2 = training_set$target_deathrate^2
training_set$incidencerate2 = training_set$incidencerate^2
training_set$medincome2 = training_set$medincome^2
training_set$popest20152 = training_set$popest2015^2
training_set$povertypercent2 = training_set$povertypercent^2
training_set$studypercap2 = training_set$studypercap^2
training_set$medianage2 = training_set$medianage^2
training_set$medianagemale2 = training_set$medianagemale^2
training_set$medianagefemale2 = training_set$medianagefemale^2
training_set$percentmarried2 = training_set$percentmarried^2
training_set$pctnohs18_24_2 = training_set$pctnohs18_24^2
training_set$pcths18_24_2 = training_set$pcths18_24^2
training_set$pctsomecol18_24_2 = training_set$pctsomecol18_24^2
training_set$pctbachdeg18_24_2 = training_set$pctbachdeg18_24^2
training_set$pcths25_over2 = training_set$pcths25_over^2
training_set$pctbachdeg25_over2 = training_set$pctbachdeg25_over^2
training_set$pctemployed16_over2 = training_set$pctemployed16_over^2
training_set$pctunemployed16_over2 = training_set$pctunemployed16_over^2
training_set$pctprivatecoverage2 = training_set$pctprivatecoverage^2
training_set$pctprivatecoveragealone2 = training_set$pctprivatecoveragealone^2
training_set$pctempprivcoverage2 = training_set$pctempprivcoverage^2
training_set$pctpubliccoverage2 = training_set$pctpubliccoverage^2
training_set$pctpubliccoveragealone2 = training_set$pctpubliccoveragealone^2
training_set$pctwhite2 = training_set$pctwhite^2
training_set$pctblack2 = training_set$pctblack^2
training_set$pctasian2 = training_set$pctasian^2
training_set$pctotherrace2 = training_set$pctotherrace^2
training_set$pctmarriedhouseholds2 = training_set$pctmarriedhouseholds^2
training_set$bithrate2 = training_set$birthrate^2




# Fitting Polynomial Regression model
regressor = lm(formula = avganncount ~ avgdeathsperyear + avgdeathsperyear2 +
                 target_deathrate + target_deathrate2 +
                 incidencerate + incidencerate2 +
                 medincome + medincome2 + 
                 popest2015 + popest20152 +
                 povertypercent + povertypercent2 + 
                 studypercap + studypercap2 + 
                 medianage + medianage2 + 
                 medianagemale + medianagemale2 + 
                 medianagefemale + medianagefemale2 +
                 percentmarried + percentmarried2 +
                 pctnohs18_24 + pctnohs18_24_2 +
                 pcths18_24 + pcths18_24_2 + 
                 pctsomecol18_24 + pctsomecol18_24_2+
                 pctbachdeg18_24 + pctbachdeg18_24_2 + 
                 pctbachdeg25_over + pctbachdeg25_over2 + 
                 pctemployed16_over + pctemployed16_over2 +
                 pctunemployed16_over + pctunemployed16_over2 + 
                 pctprivatecoverage + pctprivatecoverage2 + 
                 pctprivatecoveragealone + pctprivatecoveragealone2 +
                 pctempprivcoverage + pctempprivcoverage2 + 
                 pctpubliccoverage + pctpubliccoverage2+
                 pctpubliccoveragealone + pctpubliccoveragealone2 +
                 pctwhite + pctwhite2 + 
                 pctblack + pctblack2 + 
                 pctasian + pctasian2 + 
                 pctotherrace + pctotherrace2 +
                 pctmarriedhouseholds + pctmarriedhouseholds2 + 
                 birthrate + bithrate2,
                data = training_set)

# Summary to evaluate p-values
summary(regressor)

#Started this, unfinished because the p values are mixed, some qualify for
#p-value, others do not, unsure if you can remove characteristics



