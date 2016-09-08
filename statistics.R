# statistics of the data
# 1. User
#    user numbers, user labels, wordIDs and charIDs for specified users, 
#    answered questions of a user, labels of answered questions by user
#
# 2. Question
#    question numbers, question labels, wordIDs and charIDs for specified questions
#
# 2016.09.08
################################################################################

###
# frequencies of questions and user in invited_info_train
tq_invited <- as.data.frame(table(invited_info_train$qid))
tu_invited <- as.data.frame(table(invited_info_train$uid))

# frequencies of questions and usre in validate_nolabel
tq_validate <- as.data.frame(table(validate_nolabel$qid))
tu_validate <- as.data.frame(table(validate_nolabel$uid))

### Check if the questions of invited_info_train and validate_nolabel are all in question_info
res_tq_invited <- data.frame(InOrNot = tq_invited$Var1 %in% question_info$qid)
res_tu_invited <- data.frame(InOrNot = tu_invited$Var1 %in% user_info$uid)

### Check if the users of invited_info_train and validate_nolabel are all in user_info
res_tq_validate <- data.frame(InOrNot = tq_validate$Var1 %in% question_info$qid)
res_tu_validate <- data.frame(InOrNot = tu_validate$Var1 %in% user_info$uid)

cat('all questions in "invited_info_train" are in "question_info": ', all(res_tq_invited$InOrNot), ',\n')
cat('all users in "invited_info_train" are in "user_info": ', all(res_tu_invited$InOrNot), ',\n')
cat('all questions in "validate_nolabel" are in "question_info": ', all(res_tq_invited$InOrNot), ',\n')
cat('all users in "validate_nolabel" are in "user_info":', all(res_tu_validate$InOrNot), '.\n\n')

# remove temp variables
rm(tq_invited, tu_invited, tq_validate, tu_validate, res_tq_invited, res_tu_invited, res_tq_validate, res_tu_validate)

## 
# User numbers
cat("user numbers: ", nrow(user_info), "\n")
cat("question numbers: ", nrow(question_info), "\n")
# user labels
cat("user labels: ", NA, "\n")
cat("question labels:", unique(question_info$label),"\n")

# plot the label frequencies of users
barplot(table(user_info$label))

# plot the label frequencies of questions
barplot(table(question_info$label))

# testa <- apply(question_info, 1, checkQuestion)
# all(apply(question_info, 1, checkQuestion))
# we find not all questions "totalNumber" >= "topQuality" >= "upvotes"