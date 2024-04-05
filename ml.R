library(dplyr)
library(caret)
library(nnet) # For multinom (multinomial logistic regression)
library('this.path')

airline_data <- read_csv(file.path(this.dir(),"Airline_Dataset.csv"))

# Select a subset of features for simplicity
data_subset <- airline_data %>%
  select(satisfaction, Age, `Type of Travel`, Class, `Flight Distance`, `Inflight wifi service`, `Departure Delay in Minutes`, `Arrival Delay in Minutes`)

# Convert categorical variables to factors
data_subset$satisfaction <- as.factor(data_subset$satisfaction)
data_subset$`Type of Travel` <- as.factor(data_subset$`Type of Travel`)
data_subset$Class <- as.factor(data_subset$Class)

# Split the data into training and testing sets
set.seed(123) # for reproducibility
indexes <- createDataPartition(data_subset$satisfaction, p = 0.8, list = FALSE)
train_data <- data_subset[indexes, ]
test_data <- data_subset[-indexes, ]

# Preprocess the data: One-hot encoding for categorical variables
# Caret's dummyVars function can help in encoding categorical variables
dummies <- dummyVars(" ~ .", data = train_data)
train_data_processed <- predict(dummies, newdata = train_data)
test_data_processed <- predict(dummies, newdata = test_data)

# Prepare the target and features
y_train <- train_data$satisfaction
X_train <- train_data_processed[, -which(names(train_data_processed) == "satisfaction")]

y_test <- test_data$satisfaction
X_test <- test_data_processed[, -which(names(test_data_processed) == "satisfaction")]

# Train the logistic regression model
model <- glm(satisfaction ~ ., data = train_data, family = binomial())

# Make predictions
predictions <- predict(model, newdata = X_test, type = "response")
predicted_class <- ifelse(predictions > 0.5, "satisfied", "dissatisfied")

# Evaluate the model
conf_mat <- table(Predicted = predicted_class, Actual = y_test)
accuracy <- sum(diag(conf_mat)) / sum(conf_mat)

print(conf_mat)
cat("Model Accuracy:", accuracy, "\n")
