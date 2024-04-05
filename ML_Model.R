# Load necessary libraries
library(readr) # for read_csv
library(dplyr) # for data manipulation
library(caret) # for train/test split and model evaluation
library(xgboost) # for the xgboost model
library(pROC) # for AUC
library(Matrix) # for sparse matrix
library('this.path')

airline_data <- read_csv(file.path(this.dir(),"Airline_Dataset.csv"))

# Display the first ten rows of the data
print(head(airline_data, 10))

# Convert categorical variables to dummy variables
airline_data_dummies <- airline_data %>%
  mutate(satisfaction = as.factor(satisfaction)) %>%
  mutate(`Customer Type` = as.factor(`Customer Type`)) %>%
  mutate(`Type of Travel` = as.factor(`Type of Travel`)) %>%
  mutate(Class = as.factor(Class)) %>%
  model.matrix(~.-1, data = .) %>%
  as.data.frame()

# Define the target variable y
y <- airline_data_dummies$satisfactionsatisfied
# Define the predictor variables X
X <- airline_data_dummies %>%
  select(-satisfactionsatisfied, -satisfactiondissatisfied)

# Split the data into training and testing sets
set.seed(0) # for reproducibility
indexes <- createDataPartition(y, p = 0.75, list = FALSE)
X_train <- X[indexes, ]
X_test <- X[-indexes, ]
y_train <- y[indexes]
y_test <- y[-indexes]

# Define the XGBoost model
xgb_grid <- expand.grid(nrounds = 100,
                        eta = c(0.1, 0.2, 0.3),
                        max_depth = c(4, 6),
                        gamma = 1,
                        colsample_bytree = 0.7,
                        min_child_weight = c(3, 5),
                        subsample = 0.7)

# Train the model with cross-validation
xgb_train <- xgb.DMatrix(data = as.matrix(X_train), label = y_train)
# Ensure y_train is a factor if it's not already
y_train <- factor(y_train, levels = c(0, 1), labels = c("satisfactiondissatisfied", "satisfactionsatisfied"))

# Update trainControl with classProbs = TRUE for ROC metric
xgb_control <- trainControl(method = "cv",
                            number = 5, # Number of folds in cross-validation
                            summaryFunction = twoClassSummary, # Use ROC and other two-class summary stats
                            classProbs = TRUE, # IMPORTANT: This is needed for ROC metric
                            verboseIter = TRUE)

# Now, train the model with the updated settings
xgb_model <- train(x = as.matrix(X_train), 
                   y = y_train,
                   method = "xgbTree",
                   trControl = xgb_control,
                   tuneGrid = xgb_grid,
                   metric = "ROC")

# Check the model's details
print(xgb_model)

# Make predictions
xgb_test <- xgb.DMatrix(data = as.matrix(X_test))
y_pred <- predict(xgb_model, newdata = as.matrix(X_test))

y_pred_numeric <- ifelse(y_pred == "satisfactionsatisfied", 1, 0)

# Evaluate the model
confusionMatrix <- confusionMatrix(as.factor(y_pred_numeric), as.factor(y_test))
print(confusionMatrix)

# Extracting the xgb.Booster object from a caret model
xgb_booster <- xgb_model$finalModel

# Getting feature importance from the xgb.Booster object
importance_matrix <- xgb.importance(feature_names = colnames(X_train), model = xgb_booster)

# Plotting the feature importance
xgb.plot.importance(importance_matrix)

 # calculate the f1 score, accuracy, precision, recall and roc_auc
f1_score <- 2 * confusionMatrix$byClass[7] * confusionMatrix$byClass[8] / (confusionMatrix$byClass[7] + confusionMatrix$byClass[8])
accuracy <- confusionMatrix$overall["Accuracy"]
precision <- confusionMatrix$byClass["Pos Pred Value"]
recall <- confusionMatrix$byClass["Sensitivity"]
roc_auc <- xgb_model$results$ROC
print(paste("F1 Score:", f1_score))
print(paste("Accuracy:", accuracy))
print(paste("Precision:", precision))
print(paste("Recall:", recall))
print(paste("ROC AUC:", roc_auc))

# Save the trained model
saveRDS(xgb_model, file = "xgb_model.rds")
# Save the feature importance matrix
saveRDS(importance_matrix, file = "importance_matrix.rds")


# Step 1: Calculate the averages for each column in X_test
column_averages <- colMeans(X_test)

# Step 2: Round averages to the nearest integer
rounded_averages <- round(column_averages)
# Step 3: Convert the rounded vector to a matrix format expected by xgboost
rounded_averages_matrix <- matrix(rounded_averages, nrow = 1)

# Assuming xgb_model is your trained XGBoost model, predict using the rounded_averages_matrix
# Here's how you might do this, adjusting as necessary for your specific model and its input expectations
prediction <- predict(xgb_model, newdata = X_test[prediction$satisfactiondissatisfied > 0.5, ][1, ], type = "prob")
prediction
# Print the first raw X_test matrix where the prediction satisfactiondissatisfied>0.5

print(X_test[prediction$satisfactiondissatisfied > 0.5, ][1, ])
