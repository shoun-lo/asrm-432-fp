library(tidyverse)
library(RCurl)
library(rpart)
library(randomForest)
library(caret)

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

##Bagging
#apply bagging method (replace train_selected with train_data)
bagging = randomForest(as.factor(train_selected$X86) ~., data = train_selected, 
                       mtry = 85, importance = TRUE)

#plot out-of-bag error to find the best ntrees
plot(bagging, col = "darkorange")
#ntree = 340 looks decent
abline(v = 340)

#bagging with the best ntree
bagging2 = randomForest(as.factor(train_selected$X86) ~., data = train_selected, 
                        ntree = 340, mtry = 85, importance = TRUE)

bagging_pred = predict(bagging2, newdata = test_data)

#confusion matrix and accuracy (training data)
conf_tab = bagging2$confusion[, 1:2]
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

##Random Forest (mtry < 85)

#tune mtry
tuned = tuneRF(x = train_selected[, -86], y = as.factor(train_selected$X86), 
               ntreeTry = 340, mtryStart = 42, stepFactor = 1.5, trace = FALSE)

tuned_mtry = as_tibble(tuned) |>
  arrange(OOBError) |>
  select(mtry) |>
  head(1) |>
  pull(mtry)

#cross-valid. mtry
cv_results = rfcv(trainx = train_selected[, -86], trainy = as.factor(train_selected$X86), 
               mtryStart = 2, mtryEnd = 85, stepFactor = 1.5, scale = "log")
cv_mtry = cv_results$n.var[which.min(cv_results$error.cv)]
  

#choose ntrees = 340 with tuned mtry
rf_tuned = randomForest(as.factor(train_selected$X86) ~., data = train_selected, 
                       ntree = 340, mtry = tuned_mtry, importance = TRUE)

#choose ntrees =3 40 with cv mtry
rf_cv = randomForest(as.factor(train_selected$X86) ~., data = train_selected, 
                       ntree = 340, mtry = cv_mtry, importance = TRUE)

#random forest predictions (w/ tuned)
rf_tuned_pred = predict(bagging_tuned, newdata = test_data)

#confusion matrix and accuracy (training data) (go over with prof or ta)
conf_tab_tuned = rf_tuned$confusion[, 1:2]
sum(diag(conf_tab_tuned)) / sum(conf_tab_tuned)
#or
conf_tab_tuned = table(Predicted = bagging_tuned_pred, Actual = train_data$X86[1:4000])
sum(diag(conf_tab_tuned)) / sum(conf_tab_tuned)

#confusion matrix and accuracy (target data)
conf_tab_tuned2 = table(Predicted = bagging_tuned_pred, Actual = target_data$X1)
sum(diag(conf_tab_tuned2)) / sum(conf_tab_tuned2)

#random forest predictions (w/ cv)
rf_cv_pred = predict(rf_cv, newdata = test_data)

#confusion matrix and accuracy (training data) (go over with prof or ta)
conf_tab_cv = rf_cv$confusion[, 1:2]
sum(diag(conf_tab_cv)) / sum(conf_tab_cv)
#or
conf_tab_cv = table(Predicted = rf_cv_pred, Actual = train_data$X86[1:4000])
sum(diag(conf_tab_cv)) / sum(conf_tab_cv)

#confusion matrix and accuracy (target data)
conf_tab2_cv = table(Predicted = rf_cv_pred, Actual = target_data$X1)
sum(diag(conf_tab2_cv)) / sum(conf_tab2_cv)
