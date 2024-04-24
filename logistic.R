library(tidyverse)
library(RCurl)

#importing data from repo
url1 = getURL("https://raw.githubusercontent.com/shoun-lo/asrm-432-fp/main/train.txt")
train_data = read_delim(url1, delim = "\t", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)

url2 = getURL("https://raw.githubusercontent.com/shoun-lo/asrm-432-fp/main/predicts.txt")
test_data = read_delim(url2, delim = "\t", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)

url3 = getURL("https://raw.githubusercontent.com/shoun-lo/asrm-432-fp/main/targets.txt")
target_data = read_delim(url3, delim = "\t", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)

#Here we look at the contents and structure of the data.
head(train_data)
str(train_data)

#Making a local training dataset
training_data = train_data

#Set seed to 7
set.seed(7)

#Looking through the data, we find that the variables don't have their names assigned to the columns yet so we get the names off 
#of the website dictionary and set the column names accordingly.
colnames(training_data) = c("MOSTYPE", "MAANTHUI", "MGEMOMV", "MGEMLEEF", "MOSHOOFD", "MGODRK", "MGODPR", "MGODOV", "MGODGE", "MRELGE", "MRELSA", "MRELOV", "MFALLEEN", "MFGEKIND", "MFWEKIND", "MOPLHOOG", "MOPLMIDD", "MOPLLAAG", "MBERHOOG", "MBERZELF", "MBERBOER", "MBERMIDD", "MBERARBG", "MBERARBO", "MSKA", "MSKB1", "MSKB2", "MSKC", "MSKD", "MHHUUR", "MHKOOP", "MAUT1", "MAUT2", "MAUT0", "MZFONDS", "MZPART", "MINKM30", "MINK3045", "MINK4575", "MINK7512", "MINK123M", "MINKGEM", "MKOOPKLA", "PWAPART", "PWABEDR", "PWALAND", "PPERSAUT", "PBESAUT", "PMOTSCO", "PVRAAUT", "PAANHANG", "PTRACTOR", "PWERKT", "PBROM", "PLEVEN", "PPERSONG", "PGEZONG", "PWAOREG", "PBRAND", "PZEILPL", "PPLEZIER", "PFIETS", "PINBOED", "PBYSTAND", "AWAPART", "AWABEDR", "AWALAND", "APERSAUT", "ABESAUT", "AMOTSCO", "AVRAAUT", "AAANHANG", "ATRACTOR", "AWERKT", "ABROM", "ALEVEN", "APERSONG", "AGEZONG", "AWAOREG", "ABRAND", "AZEILPL", "APLEZIER", "AFIETS", "AINBOED", "ABYSTAND", "CARAVAN")
colnames(training_data)

is.factor(training_data$CARAVAN)
training_data$CARAVAN = as.factor(training_data$CARAVAN)


#Going to try running a glm with all of the variables to see what happens. Storing this model in "logistic_model".
logistic_model_caravan = glm(CARAVAN ~ ., data = training_data, family = "binomial")

#Running a summary on "logistic_model".
summary(logistic_model)

#Getting the correlation matrix
cor_full = cor.test(training_data)

#Using this "full" model we proceed to see what the model would predict based off the training dataset. 
glm_predict_caravan = predict(logistic_model_caravan, training_data, type = 'response')
head(glm_predict_caravan)

#Classifying values that the predict function got above 0.5 as being TRUE and anything less to be FALSE.
predicted_caravan = ifelse(glm_predict_caravan >= 0.5, 1, 0)
head(predicted_caravan)

#Determining the accuracy of the model
accuracy_full = mean(predicted_caravan==training_data$CARAVAN)
print(accuracy_full)




