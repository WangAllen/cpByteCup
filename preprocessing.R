# preprocessing.R
#   preprocess data
#
# 2016.09.08
library(dplyr)
library(stringr)

# labels of users
labels_user <- user_info %>%
  select(label) %>%
  lapply(., str_split, "/") %>%
  lapply(unlist) %>%
  as.data.frame() %>%
  unique()

# labels of questions
labels_question <- question_info %>%
  select(label) %>%
  lapply(., str_split, "/") %>%
  lapply(unlist) %>%
  as.data.frame() %>%
  unique()

# get user list by labels
# e.g. 1, uid1, uid2, ..., uidm
#      2, uidx, uidy, ..., uidw
#      ...
# user answered questions: label, wordID set, charID set

# extract features(f_label, f_wordID, f_charID) with fExtractFeatures
# on invited_info_train
features_inv <- apply(invited_info_train, 1, fExtractFeatures)
save(features_inv, file = "data/features_inv.rdata")
df_features <- data.frame(features_inv)

featureNum <- length(features_inv[[1]])
len_df_features <- length(df_features)
myDataset_train <- invited_info_train
for(i in 1:featureNum) {
  seq <- seq(i, len_df_features, featureNum)
  df <- data.frame(as.numeric(t(df_features[seq])))
  myDataset_train <- cbind(myDataset_train, df)
}
colnames(myDataset_train) <- c("qid", "uid", "label", "f_label", "f_label_ans", "f_label_non_ans", "f_total_asked", "f_total_ans", 
                               "f_total_same_asked", "f_total_same_ans", "f_total_avg_up", "f_total_avg_no", 
                               "f_total_avg_top", "f_same_ans_avg_up", "f_same_ans_avg_no", "f_same_ans_avg_top",
                               "f_total_non_avg_up", "f_total_non_avg_no", "f_total_non_avg_top", 
                               "f_same_non_ans_avg_up", "f_same_non_ans_avg_no", "f_same_non_ans_avg_top", 
                               "f_wordID_QInU", "f_wordID_total_ans", "f_wordID_same_ans", "f_wordID_total_diff", "f_wordID_same_diff", 
                               "f_charID_QInU", "f_charID_total_ans", "f_charID_same_ans", "f_charID_total_diff", "f_charID_same_diff")
save(myDataset_train, file = "data/myDataset_train.rdata")

# on validate_nolabel
features_val <- apply(validate_nolabel, 1, fExtractFeatures)
save(features_val, file = "data/features_val.rdata")
df_features <- data.frame(features_val)

featureNum <- length(features_inv[[1]])
len_df_features <- length(df_features)
df_flag <- data.frame(flag = 1:nrow(validate_nolabel) * 0)
myDataset_predict <- cbind(validate_nolabel, df_flag)
for(i in 1:featureNum) {
  seq <- seq(i, len_df_features, featureNum)
  df <- data.frame(as.numeric(t(df_features[seq])))
  myDataset_predict <- cbind(myDataset_predict, df)
}
colnames(myDataset_predict) <- c("qid", "uid", "label", "f_label", "f_label_ans", "f_label_non_ans", "f_total_asked", "f_total_ans", 
                               "f_total_same_asked", "f_total_same_ans", "f_total_avg_up", "f_total_avg_no", 
                               "f_total_avg_top", "f_same_ans_avg_up", "f_same_ans_avg_no", "f_same_ans_avg_top",
                               "f_total_non_avg_up", "f_total_non_avg_no", "f_total_non_avg_top", 
                               "f_same_non_ans_avg_up", "f_same_non_ans_avg_no", "f_same_non_ans_avg_top", 
                               "f_wordID_QInU", "f_wordID_total_ans", "f_wordID_same_ans", "f_wordID_total_diff", "f_wordID_same_diff", 
                               "f_charID_QInU", "f_charID_total_ans", "f_charID_same_ans", "f_charID_total_diff", "f_charID_same_diff")
save(myDataset_predict, file = "data/myDataset_predict.rdata")

# remove temp variables

#