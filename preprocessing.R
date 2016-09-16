# preprocessing.R
#   preprocess data
#
# 2016.09.08 added
# 2016.09.13 modified to replace the Nas to 0 in myDataset_train(predict)

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

# extract features with f_ExtractFeatures_(label, vote, wordID, charID)

# 1. on invitited_info_train: myDataset_train
features_label <- apply(invited_info_train, 1, fExtractFeatures_label)
save(features_label, file = "data/features_label_train.rdata")
features_vote <- apply(invited_info_train, 1, fExtractFeatures_vote)
save(features_vote, file = "data/features_vote_train.rdata")
features_wordID <- apply(invited_info_train, 1, fExtractFeatures_wordID)
save(features_wordID, file = "data/features_wordID_train.rdata")
features_charID <- apply(invited_info_train, 1, fExtractFeatures_charID)
save(features_charID, file = "data/features_charID_train.rdata")

myDataset_train <- invited_info_train
myDataset_train <- featureCbind(features_label, myDataset_train)
myDataset_train <- featureCbind(features_vote, myDataset_train)
myDataset_train <- featureCbind(features_wordID, myDataset_train)
myDataset_train <- featureCbind(features_charID, myDataset_train)
myDataset_train[is.na(myDataset_train)] <- 0

colnames(myDataset_train) <- c("qid", "uid", "label", "f_label", "f_label_ans", "f_label_non_ans", "f_total_asked", "f_total_ans", 
                               "f_total_same_asked", "f_total_same_ans", "f_total_avg_up", "f_total_avg_no", 
                               "f_total_avg_top", "f_same_ans_avg_up", "f_same_ans_avg_no", "f_same_ans_avg_top",
                               "f_total_non_avg_up", "f_total_non_avg_no", "f_total_non_avg_top", 
                               "f_same_non_ans_avg_up", "f_same_non_ans_avg_no", "f_same_non_ans_avg_top", 
                               "f_wordID_QInU", "f_wordID_total_ans", "f_wordID_same_ans", "f_wordID_total_diff", "f_wordID_same_diff", 
                               "f_charID_QInU", "f_charID_total_ans", "f_charID_same_ans", "f_charID_total_diff", "f_charID_same_diff")
save(myDataset_train, file = "data/myDataset_train.rdata")

# 2. on validate: myDataset_predict
features_label <- apply(validate_nolabel, 1, fExtractFeatures_label)
save(features_label, file = "data/features_label_predict.rdata")
features_vote <- apply(validate_nolabel, 1, fExtractFeatures_vote)
save(features_vote, file = "data/features_vote_predict.rdata")
features_wordID <- apply(validate_nolabel, 1, fExtractFeatures_wordID)
save(features_wordID, file = "data/features_wordID_predict.rdata")
features_charID <- apply(validate_nolabel, 1, fExtractFeatures_charID)
save(features_charID, file = "data/features_charID_predict.rdata")

df_flag <- data.frame(flag = 1:nrow(validate_nolabel) * 0)
myDataset_predict <- cbind(validate_nolabel, df_flag)
myDataset_predict <- featureCbind(features_label, myDataset_predict)
myDataset_predict <- featureCbind(features_vote, myDataset_predict)
myDataset_predict <- featureCbind(features_wordID, myDataset_predict)
myDataset_predict <- featureCbind(features_charID, myDataset_predict)
myDataset_predict[is.na(myDataset_predict)] <- 0

colnames(myDataset_predict) <- c("qid", "uid", "label", "f_label", "f_label_ans", "f_label_non_ans", "f_total_asked", "f_total_ans", 
                               "f_total_same_asked", "f_total_same_ans", "f_total_avg_up", "f_total_avg_no", 
                               "f_total_avg_top", "f_same_ans_avg_up", "f_same_ans_avg_no", "f_same_ans_avg_top",
                               "f_total_non_avg_up", "f_total_non_avg_no", "f_total_non_avg_top", 
                               "f_same_non_ans_avg_up", "f_same_non_ans_avg_no", "f_same_non_ans_avg_top", 
                               "f_wordID_QInU", "f_wordID_total_ans", "f_wordID_same_ans", "f_wordID_total_diff", "f_wordID_same_diff", 
                               "f_charID_QInU", "f_charID_total_ans", "f_charID_same_ans", "f_charID_total_diff", "f_charID_same_diff")
save(myDataset_predict, file = "data/myDataset_predict.rdata")
