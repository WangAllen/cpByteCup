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

# seqences to get f_label, f_wordID, f_charID
seq1 <- seq(1, length(df_features), 3) 
seq2 <- seq(2, length(df_features), 3) 
seq3 <- seq(3, length(df_features), 3) 
df_f_label <- data.frame(f_label = as.numeric(t(df_features[seq1]))) # f_label
df_f_wordID <- data.frame(f_wordID = as.numeric(t(df_features[seq2]))) # f_wordID
df_f_charID <- data.frame(f_charID = as.numeric(t(df_features[seq3]))) # f_charID

# form new data frame
myDataset_train <- cbind(invited_info_train, df_f_label, df_f_wordID, df_f_charID)
save(myDataset_train, file = "data/myDataset_train.rdata")

# on validate_nolabel
features_val <- apply(validate_nolabel, 1, fExtractFeatures)
save(features_val, file = "data/features_val.rdata")
df_features <- data.frame(features_val)

seq1 <- seq(1, length(df_features), 3)
seq2 <- seq(2, length(df_features), 3)
seq3 <- seq(3, length(df_features), 3)
df_f_label <- data.frame(f_label = as.numeric(t(df_features[seq1])))
df_f_wordID <- data.frame(f_wordID = as.numeric(t(df_features[seq2])))
df_f_charID <- data.frame(f_charID = as.numeric(t(df_features[seq3])))

myDataset_predict <- cbind(invited_info_train, df_f_label, df_f_wordID, df_f_charID)
save(myDataset_predict, file = "data/myDataset_predict.rdata")

# remove temp variables
rm(seq1, seq2, seq3, df_f_label, df_f_wordID, df_f_charID)
#