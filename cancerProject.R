# Data Preprocessing

# Importing the dataset
dataset = read.csv('cancer_reg.csv', stringsAsFactors = FALSE)

#colSums(is.na(dataset))

# Taking care of missing data
dataset$pctsomecol18_24 = ifelse(is.na(dataset$pctsomecol18_24),
                     0,
                     dataset$pctsomecol18_24)

dataset$pctemployed16_over = ifelse(is.na(dataset$pctemployed16_over),
                                 0,
                                 dataset$pctemployed16_over)
dataset$pctprivatecoveragealone = ifelse(is.na(dataset$pctprivatecoveragealone),
                                 0,
                                 dataset$pctprivatecoveragealone)

#colSums(is.na(dataset))

dataset = dataset[, !(names(dataset) %in% c("binnedinc", "geography"))]

# Splitting the dataset into the Training set and Test set
library(caTools)
split = sample.split(dataset$avganncount, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
training_scaled_cols = scale(training_set[, 1:31])
training_set[, 1:31] = training_scaled_cols
test_set[, 1:31] = scale(test_set[, 1:31], center=attr(training_scaled_cols, 'scaled:center'),
                        scale=attr(training_scaled_cols, 'scaled:scale'))


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
training_set$pcths18_24_2 = training_set$pcths18_24^2



# Fitting Polynomial Regression model
regressor = lm(formula = Store_Sales ~ Store_Area + Store_Area2 +
                 Items_Available + Items_Available2 +
                 Daily_Customer_Count + Daily_Customer_Count2,
               data = training_set)

# Summary to evaluate p-values
summary(regressor)

#Started this, unfinished because the p values are terrible



