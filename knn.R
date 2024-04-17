library(tidyverse)
library(RCurl)

#importing data from repo
url1 = getURL("https://raw.githubusercontent.com/shoun-lo/asrm-432-fp/main/train.txt")
train_data = read_delim(url1, delim = "\t",
                        escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)

url2 = getURL("https://raw.githubusercontent.com/shoun-lo/asrm-432-fp/main/predicts.txt")
test_data = read_delim(url2, delim = "\t",
                       escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)

url3 = getURL("https://raw.githubusercontent.com/shoun-lo/asrm-432-fp/main/targets.txt")
target_data = read_delim(url3, delim = "\t",
                         escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)

set.seed(7)

# Load necessary libraries
library(class)
library(caret)
library(ggplot2)

# Normalize the data since KNN uses distance metrics.
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# Apply normalization to the training and testing datasets
train_data_normalized <- as.data.frame(lapply(train_data[, -86], normalize))
test_data_normalized <- as.data.frame(lapply(test_data, normalize))

# Split the training data into features and target variable
train_features <- train_data_normalized
train_target <- train_data[, 86]

# Set up cross-validation for k optimization
control <- trainControl(method = "cv", number = 10)

# Define the range of k values to test
grid <- expand.grid(.k = 1:10)  # Testing k from 1 to 10

# Convert the target variable to a factor to allow train() to run below
train_target <- as.factor(train_target[[1]])

# Train the model with different values of k and select the best k
knn_train <- train(x = train_features, y = train_target,
                   method = "knn", tuneGrid = grid,
                   trControl = control)

# Print the best k value
cat("Best k:", knn_train$bestTune[1,1], "\n")

# Fit the KNN model with the best k to the training data
best_k <- knn_train$bestTune[1,1]
knn_model <- knn(train = train_features, test = test_data_normalized,
                 cl = train_target, k = best_k)

# Evaluate the model by comparing its predictions to target_data
conf_matrix <- table(predicted = knn_model, actual = target_data$X1)
print(conf_matrix)

# Calculate accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
cat("Accuracy:", accuracy, "\n")

# Plot accuracy versus different K values
accuracy_plot <- ggplot(knn_train$results,
                        aes(x = knn_train$results$k, y = knn_train$results$Accuracy)) +
  geom_line() +
  geom_point(shape = 21, fill = "blue") +
  labs(title = "KNN Model Accuracy for Different K Values",
       x = "Number of Neighbors (K)",
       y = "Cross-Validated Accuracy") +
  theme_minimal()

print(accuracy_plot)