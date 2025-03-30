# Data Preprocessing

# Importing the dataset
dataset = read.csv('Real_Estate_Data.csv')

# Splitting the dataset into the Training set and Test set
library(caTools)
set.seed(123)
split = sample.split(dataset$Y, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
training_scaled_cols = scale(training_set[, 1:6])
training_set[, 1:6] = training_scaled_cols
test_set[, 1:6] = scale(test_set[, 1:6], center=attr(training_scaled_cols, 'scaled:center'),
                        scale=attr(training_scaled_cols, 'scaled:scale'))