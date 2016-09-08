# Load.R
# Load original data from local data files.
# Data are from the official website of byte cup 2016
#   http://biendata.com/competition/bytecup2016/
#
# @2016.09.08

cat("start loading data ...\n")
# user_info
user_info <- read.table(file = "data/user_info.txt", header = F, sep = "\t")
colnames(user_info) <- c("uid", "label", "wordID", "charID")

# question_info
question_info <- read.table(file = "data/question_info.txt", header = F, sep = "\t")
colnames(question_info) <- c("qid", "label", "wordID", "charID", "upvotes", "totalNumber", "topQuality")

# training data
invited_info_train <- read.table(file = "data/invited_info_train_new.txt", header = F, sep = "\t")
colnames(invited_info_train) <- c("qid", "uid", "flag")

# data for predicting
validate_nolabel <- read.table(file = "data/validate_nolabel.txt", header = F, sep = ",")
colnames(validate_nolabel) <- c("qid", "uid")

### To be added after Nov. 11
# invited_info_validate
# invited_info_test

cat("load data completed ...\n")