library(tidyverse)
library(RCurl)
library(rpart)
library(randomForest)


#importing data from repo
url1 = getURL("https://raw.githubusercontent.com/shoun-lo/asrm-432-fp/main/train.txt")
train_data = read_delim(url1, delim = "\t", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)

url2 = getURL("https://raw.githubusercontent.com/shoun-lo/asrm-432-fp/main/predicts.txt")
test_data = read_delim(url2, delim = "\t", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)

url3 = getURL("https://raw.githubusercontent.com/shoun-lo/asrm-432-fp/main/targets.txt")
target_data = read_delim(url3, delim = "\t", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)

#set seed to be 7
set.seed(7)

#first hundred observations.
train_selected = train_data[1:100, ]

#apply bagging method (replace train_selected with train_data)
bagging = randomForest(as.factor(train_selected$X86) ~., data = train_selected, 
                       mtry = 85, importance = TRUE)

#plot out-of-bag error to find the best ntrees
plot(bagging, col = "darkorange")
#ntree = 340 looks decent

#tune mtry
tuned = tuneRF(x = train_data[, -86], y = as.factor(train_data$X86), ntreeTry = 340, mtryStart = 42, stepFactor = 1.5)

#optimal mtry value


#choose trees 330-350
bagging = randomForest(as.factor(train_selected$X86) ~., data = train_selected, 
                       ntree = 340, mtry = 85, importance = TRUE)

#bagging predictions
bagging_pred = predict(bagging, newdata = test_data)
plot(bagging_pred)

#confusion matrix and accuracy (training data) (go over with prof or ta)
conf_tab = bagging$confusion[, 1:2]
sum(diag(conf_tab)) / sum(conf_tab)
#or
conf_tab = table(Predicted = bagging_pred, Actual = train_data$X86[1:4000])
sum(diag(conf_tab)) / sum(conf_tab)

#confusion matrix and accuracy (target data)
conf_tab2 = table(Predicted = bagging_pred, Actual = target_data$X1)
sum(diag(conf_tab2)) / sum(conf_tab2)

#importance of vars. (Mean Decreasing Accuracy)
importance = importance(bagging)
importance[, 3:4] = abs(importance[, 3:4])
importance = sort(importance[,3], decreasing = TRUE)
head(importance, 5)
# X33, X6, X41, X29, X24

#random forest

#apply random forest onto training data
random_forest = randomForest(as.factor(train_selected$X86) ~., 
                             data = train_selected, importance = TRUE)
random_forest

#random forest predictions on testing data
random_forest_pred = predict(random_forest, newdata = test_data)

mean((random_forest_pred - randon))