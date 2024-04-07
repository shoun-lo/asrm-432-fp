library(tidyverse)
library(RCurl)

#importing data from repo
url1 = getURL("https://raw.githubusercontent.com/shoun-lo/asrm-432-fp/main/train.txt")
train_data = read_delim(url1, delim = "\t", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)

url2 = getURL("https://raw.githubusercontent.com/shoun-lo/asrm-432-fp/main/predicts.txt")
predict_data = read_delim(url2, delim = "\t", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)

url3 = getURL("https://raw.githubusercontent.com/shoun-lo/asrm-432-fp/main/targets.txt")
target_data = read_delim(url3, delim = "\t", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)