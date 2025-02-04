# Install necessary packages (only if they are not already installed)
packages <- c("randomForest", "caret", "dplyr", "ggplot2", "pROC", "tidyverse")

# Install missing packages
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load libraries
library(randomForest)
library(caret)
library(dplyr)
library(ggplot2)
library(pROC)
library(tidyverse)
# Load data
data <- read.csv("C:/Users/norah/Desktop/Wholesale customers data.csv")
# Data Preprocessing
data$Region <- as.factor(data$Region)  # Convert Region to factor
data <- data %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))  # Handle missing values

# Split into training and test sets
set.seed(123)
trainIndex <- createDataPartition(data$Region, p = 0.7, list = FALSE)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]
# Train Random Forest Model
rf_model <- randomForest(Region ~ Channel + Fresh + Milk + Grocery + Frozen + Detergents_Paper + Delicassen,
                         data = trainData, ntree = 500, importance = TRUE)

# Print the model summary
print(rf_model)
# Make predictions on the test set
predictions <- predict(rf_model, testData)



# Confusion Matrix
conf_matrix <- confusionMatrix(predictions, testData$Region)
print(conf_matrix)

# Feature importance
importance(rf_model)
varImpPlot(rf_model)

# Calculate accuracy
accuracy <- sum(predictions == testData$Region) / nrow(testData)
print(paste("Accuracy: ", accuracy))

# Perform cross-validation
control <- trainControl(method = "cv", number = 5)  # 5-fold CV
rf_cv_model <- train(Region ~ ., data = trainData, method = "rf", trControl = control, tuneLength = 5)

# Hyperparameter tuning
# Define the tuning grid
tuneGrid <- expand.grid(mtry = c(2, 4, 6, 8, 10))

# Train the random forest model with the tuning grid
rf_tuned <- train(Region ~ ., data = trainData, method = "rf", trControl = control, tuneGrid = tuneGrid)

# Print the results of the tuned model
print(rf_tuned)
# More Performance Metrics (Beyond Accuracy)

# Extract precision from confusion matrix
precision <- conf_matrix$byClass["Pos Pred Value"]
# Extract recall (sensitivity) from confusion matrix
recall <- conf_matrix$byClass["Sensitivity"]
# Compute F1 score
f1_score <- 2 * (precision * recall) / (precision + recall)

# Print the results
print(paste("Precision:", precision))
print(paste("Recall:", recall))
print(paste("F1 Score:", f1_score))

# ROC Curve & AUC Score (For Binary Classification or One-vs-All Multiclass)
# If Region has more than 2 categories, convert it into one-vs-all to plot an ROC curve.
roc_curve <- multiclass.roc(testData$Region, as.numeric(predictions))

# Print the AUC score
print(auc(roc_curve))

# Extract confusion matrix table
cm <- conf_matrix$table  

# Calculate Precision, Recall, and F1-Score:
# Precision:
precision <- diag(cm) / colSums(cm)
print(precision)

# Recall:
recall <- diag(cm) / rowSums(cm)
print(recall)

# F1-Score:
f1_score <- 2 * (precision * recall) / (precision + recall)
print(f1_score)