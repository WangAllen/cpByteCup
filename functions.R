# functions.R
#   functions
#
# 2016.09.08

### checkQuestion
checkQuestion <- function(x) {
  a <- as.numeric(x['totalNumber'])
  b <- as.numeric(x['topQuality'])
  c <- as.numeric(x['upvotes'])
  if( (a >= b) && (b >= c)) {
    return(T)
  } else {
    return(F)
  }
}

# extract features (generate new cols) for invited_info_train and validate_nolabel
#  by label, wordID and charID
# used dataset: question_info, user_info, invited_info_train
### added @2016.09.05
### modified @2016.09.08
fExtractFeatures <- function(x) {
  questionID <- x[1]
  userID <- x[2]

  qlabel <- filter(question_info, qid == questionID)$label
  ulabel <- filter(user_info, uid == userID)$label
  
  # user asked questions
  asked_qid <- invited_info_train %>%
    filter(uid == as.character(userID))
  
  asked_q <- filter(question_info, qid %in% unique(asked_qid$qid))
  asked_q_qlabel <- filter(asked_q, label == qlabel)
  labels_asked <- asked_q$label
  
  # user answered questions
  answered_qid <- invited_info_train %>%
    filter(uid == as.character(userID), flag == 1) # Is as.character() necessary?
  
  answered_q <- filter(question_info, qid %in% unique(answered_qid$qid))
  answered_q_qlabel <- filter(answered_q, label == qlabel)
  labels_answered <- answered_q$label
  
  # user not answered questions
  nonAnswered_qid <- invited_info_train %>%
    filter(uid == as.character(userID), flag == 0)
  
  nonAnswered_q <- filter(question_info, qid %in% unique(nonAnswered_qid$qid))
  nonAnswered_q_qlabel <- filter(nonAnswered_q, label == qlabel)
  labels_nonAnswered <- nonAnswered_q$label
  
  ################################# labels #####################################
  # feature 1. f_label
  u_label <- unlist(str_split(ulabel, "/"))
  if(qlabel %in% u_label) {
    f_label <- 1
  } else {
    f_label <- 0
  }
  # feature 2: f_label_ans
  # if qlabel is in answered labels, ratio of qlabel in answered labels
  if (qlabel %in% labels_answered) {
    f_label_ans <- nrow(answered_q_qlabel) / nrow(answered_q)
  } else {
    f_label_ans <- 0
  }
  # feature 3: f_label_non_ans
  # if qlabel is in nonAnswered labels
  if(qlabel %in% labels_nonAnswered) {
    f_label_non_ans <- nrow(nonAnswered_q_qlabel) / nrow(nonAnswered_q)
  } else {
    f_label_non_ans <- 0
  }
  # feature 4: f_total_asked
  # total number of user asked questions
  f_total_asked <- nrow(asked_qid)
  
  # feature 5: f_total_ans
  # total number of answered questions
  f_total_ans <- nrow(answered_qid)
  
  # feature 6: f_total_same_asked
  # total number of asked questions that have same label
  f_total_same_asked <- nrow(asked_q_qlabel)
  
  # feature 7: f_total_same_ans
  # total number of answered questions that have the same label
  f_total_same_ans <- nrow(answered_q_qlabel)
  
  # feature 8: f_total_avg_up
  # average of total answered questions: upvotes
  f_total_avg_up <- mean(answered_q$upvotes)
  
  # feature 9: f_total_avg_no
  # average of total answered questions: totalNumber
  f_total_avg_no <- mean(answered_q$totalNumber)
  
  # feature 10: f_total_avg_top
  # average of total answered questions: topQuality
  f_total_avg_top <- mean(answered_q$topQuality)
  
  # feature 11: f_same_ans_avg_up
  # average of same label answered questions: upvotes
  f_same_ans_avg_up <- mean(answered_q_qlabel$upvotes)
  
  # feature 12: f_same_ans_avg_no
  # average of same label answered questions: totalNumber
  f_same_ans_avg_no <- mean(answered_q_qlabel$totalNumber)
  
  # feature 13: f_same_ans_avg_top
  # average of same label answered questions: topQuality
  f_same_ans_avg_top <- mean(answered_q_qlabel$topQuality)
  
  # feature 14: f_total_non_avg_up
  # average of total nonanswered questions: upvotes
  f_total_non_avg_up <- mean(nonAnswered_q$upvotes)
  
  # feature 15: f_total_non_avg_no
  # average of total nonanswered questions: totalNumber
  f_total_non_avg_no <- mean(nonAnswered_q$totalNumber)
  
  # feature 16: f_total_non_avg_top
  # average of total nonanswered questions: topQuality
  f_total_non_avg_top <- mean(nonAnswered_q$topQuality)
  
  # feature 17: f_same_non_ans_avg_up
  # average of same label answered questions: upvotes
  f_same_non_ans_avg_up <- mean(nonAnswered_q_qlabel$upvotes)
  
  # feature 18: f_same_non_ans_avg_no
  # average of same label answered questions: totalNumber
  f_same_non_ans_avg_no <- mean(nonAnswered_q_qlabel$totalNumber)
  
  # feature 19: f_same_non_ans_avg_top
  # average of same label answered questions: topQuality
  f_same_non_ans_avg_top <- mean(nonAnswered_q_qlabel$topQuality)
  
  ################################# wordID #####################################
  # wordID set of current question
  wordIDs_q <- filter(question_info, qid == questionID) %>%
    select(wordID) %>%
    lapply(., str_split, "/") %>%
    lapply(., unlist) %>%
    as.data.frame() %>%
    unique()
  
  # wordID set of userID
  wordIDs_u <- filter(user_info, uid == userID) %>%
    select(wordID) %>%
    lapply(., str_split, "/") %>%
    lapply(., unlist) %>%
    as.data.frame() %>%
    unique()
  
  # wordID set of user answered questions
  wordIDs_answered_total <- answered_q %>%
    select(wordID) %>%
    lapply(., str_split, "/") %>%
    lapply(., unlist) %>%
    as.data.frame() %>%
    unique()
  
  # wordID set of user answered questions with same label
  wordIDs_answered_same <- answered_q_qlabel %>%
    select(wordID) %>%
    lapply(., str_split, "/") %>%
    lapply(., unlist) %>%
    as.data.frame() %>%
    unique()

  # wordID set of user nonanswered questions
  wordIDs_nonanswered_total <- nonAnswered_q %>%
    select(wordID) %>%
    lapply(., str_split, "/") %>%
    lapply(., unlist) %>%
    as.data.frame() %>%
    unique()
  
  # wordID set of user answered questions with same label
  wordIDs_nonanswered_same <- nonAnswered_q_qlabel %>%
    select(wordID) %>%
    lapply(., str_split, "/") %>%
    lapply(., unlist) %>%
    as.data.frame() %>%
    unique()
  
  # feature 20: f_wordID_QInU
  # wordIDs_q and wordIDs_u
  f_wordID_QInU <- length(intersect(wordIDs_q$wordID, wordIDs_u$wordID)) / length(union(wordIDs_q$wordID, wordIDs_u$wordID))
  
  # feature 21: f_wordID_total_ans
  # wordIDs_q and wordIDs_answered_total
  f_wordID_total_ans <- length(intersect(wordIDs_q$wordID, wordIDs_answered_total$wordID)) / length(union(wordIDs_q$wordID, wordIDs_answered_total$wordID))
  
  # feature 22: f_wordID_same_ans
  # wordIDs_q and wordIDs_answered_same
  f_wordID_same_ans <- length(intersect(wordIDs_q$wordID, wordIDs_answered_same$wordID)) / length(union(wordIDs_q$wordID, wordIDs_answered_same$wordID))
  
  # feature 23: f_wordID_total_diff
  # wordIDs_q and setdiff(wordIDs_nonanswered_total, wordIDs_answered_total)
  wordIDs_diff <- setdiff(wordIDs_nonanswered_total$wordID, wordIDs_answered_total$wordID)
  f_wordID_total_diff <- length(intersect(wordIDs_q$wordID, wordIDs_diff)) / length(union(wordIDs_q$wordID, wordIDs_diff))
  
  # feature 24: f_wordID_same_diff
  # wordIDs_q and setdiff(wordIDs_nonanswered_same, wordIDs_answered_same)
  wordIDs_diff <- setdiff(wordIDs_nonanswered_same$wordID, wordIDs_answered_same$wordID)
  f_wordID_same_diff <- length(intersect(wordIDs_q$wordID, wordIDs_diff)) / length(union(wordIDs_q$wordID, wordIDs_diff))

  ################################# charID #####################################
  # charID set of current question
  charIDs_q <- filter(question_info, qid == questionID) %>%
    select(charID) %>%
    lapply(., str_split, "/") %>%
    lapply(., unlist) %>%
    as.data.frame() %>%
    unique()
  
  # charID set of userID
  charIDs_u <- filter(user_info, uid == userID) %>%
    select(charID) %>%
    lapply(., str_split, "/") %>%
    lapply(., unlist) %>%
    as.data.frame() %>%
    unique()
  
  # charID set of user answered questions
  charIDs_answered_total <- answered_q %>%
    select(charID) %>%
    lapply(., str_split, "/") %>%
    lapply(., unlist) %>%
    as.data.frame() %>%
    unique()
  
  # charID set of user answered questions with same label
  charIDs_answered_same <- answered_q_qlabel %>%
    select(charID) %>%
    lapply(., str_split, "/") %>%
    lapply(., unlist) %>%
    as.data.frame() %>%
    unique()
  
  # charID set of user nonanswered questions
  charIDs_nonanswered_total <- nonAnswered_q %>%
    select(charID) %>%
    lapply(., str_split, "/") %>%
    lapply(., unlist) %>%
    as.data.frame() %>%
    unique()
  
  # charID set of user answered questions with same label
  charIDs_nonanswered_same <- nonAnswered_q_qlabel %>%
    select(charID) %>%
    lapply(., str_split, "/") %>%
    lapply(., unlist) %>%
    as.data.frame() %>%
    unique()
  
  # feature 25: f_charID_QInU
  # charIDs_q and charIDs_u
  f_charID_QInU <- length(intersect(charIDs_q$charID, charIDs_u$charID)) / length(union(charIDs_q$charID, charIDs_u$charID))
  
  # feature 26: f_charID_total_ans
  # charIDs_q and charIDs_answered_total
  f_charID_total_ans <- length(intersect(charIDs_q$charID, charIDs_answered_total$charID)) / length(union(charIDs_q$charID, charIDs_answered_total$charID))
  
  # feature 27: f_charID_same_ans
  # charIDs_q and charIDs_answered_same
  f_charID_same_ans <- length(intersect(charIDs_q$charID, charIDs_answered_same$charID)) / length(union(charIDs_q$charID, charIDs_answered_same$charID))
  
  # feature 28: f_charID_total_diff
  # charIDs_q and setdiff(charIDs_nonanswered_total, charIDs_answered_total)
  charIDs_diff <- setdiff(charIDs_nonanswered_total$charID, charIDs_answered_total$charID)
  f_charID_total_diff <- length(intersect(charIDs_q$charID, charIDs_diff)) / length(union(charIDs_q$charID, charIDs_diff))
  
  # feature 29: f_charID_same_diff
  # charIDs_q and setdiff(charIDs_nonanswered_same, charIDs_answered_same)
  charIDs_diff <- setdiff(charIDs_nonanswered_sam$charIDe, charIDs_answered_same$charID)
  f_charID_same_diff <- length(intersect(charIDs_q$charID, charIDs_diff)) / length(union(charIDs_q$charID, charIDs_diff))
  
  ################################# data frame #####################################
  df <- data.frame(f_label, f_label_ans, f_label_non_ans, f_total_asked, f_total_ans, 
                   f_total_same_asked, f_total_same_ans, f_total_avg_up, f_total_avg_no, 
                   f_total_avg_top, f_same_ans_avg_up, f_same_ans_avg_no, f_same_ans_avg_top,
                   f_total_non_avg_up, f_total_non_avg_no, f_total_non_avg_top, 
                   f_same_non_ans_avg_up, f_same_non_ans_avg_no, f_same_non_ans_avg_top, 
                   f_wordID_QInU, f_wordID_total_ans, f_wordID_same_ans, f_wordID_total_diff, f_wordID_same_diff, 
                   f_charID_QInU, f_charID_total_ans, f_charID_same_ans, f_charID_total_diff, f_charID_same_diff)
  return(df)
}

### added @2016.09.09
# Due to the large computatioin, "fExtractFeatures" is split into 4 small functions, named as:
# fExtractFeatures_label, fExtractFeatures_vote, fExtractFeatures_wordID, and fExtractFeatures_charID.

# Extract features of label
fExtractFeatures_label <- function(x) {
  questionID <- x[1]
  userID <- x[2]
  
  qlabel <- filter(question_info, qid == questionID)$label
  ulabel <- filter(user_info, uid == userID)$label
  
  # user asked questions
  asked_qid <- invited_info_train %>%
    filter(uid == as.character(userID))
  
  asked_q <- filter(question_info, qid %in% unique(asked_qid$qid))
  asked_q_qlabel <- filter(asked_q, label == qlabel)
  labels_asked <- asked_q$label
  
  # user answered questions
  answered_qid <- invited_info_train %>%
    filter(uid == as.character(userID), flag == 1) # Is as.character() necessary?
  
  answered_q <- filter(question_info, qid %in% unique(answered_qid$qid))
  answered_q_qlabel <- filter(answered_q, label == qlabel)
  labels_answered <- answered_q$label
  
  # user not answered questions
  nonAnswered_qid <- invited_info_train %>%
    filter(uid == as.character(userID), flag == 0)
  
  nonAnswered_q <- filter(question_info, qid %in% unique(nonAnswered_qid$qid))
  nonAnswered_q_qlabel <- filter(nonAnswered_q, label == qlabel)
  labels_nonAnswered <- nonAnswered_q$label
  
  ################################# labels #####################################
  # feature 1. f_label
  u_label <- unlist(str_split(ulabel, "/"))
  if(qlabel %in% u_label){
    f_label <- 1
  } else {
    f_label <- 0
  }
  
  # feature 2: f_label_ans
  # if qlabel is in answered labels, ratio of qlabel in answered labels
  if (qlabel %in% labels_answered) {
    f_label_ans <- nrow(answered_q_qlabel) / nrow(answered_q)
  } else {
    f_label_ans <- 0
  }
  
  # feature 3: f_label_non_ans
  # if qlabel is in nonAnswered labels
  if(qlabel %in% labels_nonAnswered) {
    f_label_non_ans <- nrow(nonAnswered_q_qlabel) / nrow(nonAnswered_q)
  } else {
    f_label_non_ans <- 0
  }
  
  ################################# data frame #####################################
  df <- data.frame(f_label, f_label_ans, f_label_non_ans)
  return(df)
}

# Extract features of votes: upvotes, topQuality and totalNumber of questions in question_info
fExtractFeatures_vote <- function(x) {
  questionID <- x[1]
  userID <- x[2]
  
  qlabel <- filter(question_info, qid == questionID)$label
  ulabel <- filter(user_info, uid == userID)$label
  
  # user asked questions
  asked_qid <- invited_info_train %>%
    filter(uid == as.character(userID))
  
  asked_q <- filter(question_info, qid %in% unique(asked_qid$qid))
  asked_q_qlabel <- filter(asked_q, label == qlabel)
  labels_asked <- asked_q$label
  
  # user answered questions
  answered_qid <- invited_info_train %>%
    filter(uid == as.character(userID), flag == 1) # Is as.character() necessary?
  
  answered_q <- filter(question_info, qid %in% unique(answered_qid$qid))
  answered_q_qlabel <- filter(answered_q, label == qlabel)
  labels_answered <- answered_q$label
  
  # user not answered questions
  nonAnswered_qid <- invited_info_train %>%
    filter(uid == as.character(userID), flag == 0)
  
  nonAnswered_q <- filter(question_info, qid %in% unique(nonAnswered_qid$qid))
  nonAnswered_q_qlabel <- filter(nonAnswered_q, label == qlabel)
  labels_nonAnswered <- nonAnswered_q$label
  
  ################################# vote   #####################################
  # feature 4: f_total_asked
  # total number of user asked questions
  f_total_asked <- nrow(asked_qid)
  
  # feature 5: f_total_ans
  # total number of answered questions
  f_total_ans <- nrow(answered_qid)
  
  # feature 6: f_total_same_asked
  # total number of asked questions that have same label
  f_total_same_asked <- nrow(asked_q_qlabel)
  
  # feature 7: f_total_same_ans
  # total number of answered questions that have the same label
  f_total_same_ans <- nrow(answered_q_qlabel)
  
  # feature 8: f_total_avg_up
  # average of total answered questions: upvotes
  f_total_avg_up <- mean(answered_q$upvotes)
  
  # feature 9: f_total_avg_no
  # average of total answered questions: totalNumber
  f_total_avg_no <- mean(answered_q$totalNumber)
  
  # feature 10: f_total_avg_top
  # average of total answered questions: topQuality
  f_total_avg_top <- mean(answered_q$topQuality)
  
  # feature 11: f_same_ans_avg_up
  # average of same label answered questions: upvotes
  f_same_ans_avg_up <- mean(answered_q_qlabel$upvotes)
  
  # feature 12: f_same_ans_avg_no
  # average of same label answered questions: totalNumber
  f_same_ans_avg_no <- mean(answered_q_qlabel$totalNumber)
  
  # feature 13: f_same_ans_avg_top
  # average of same label answered questions: topQuality
  f_same_ans_avg_top <- mean(answered_q_qlabel$topQuality)
  
  # feature 14: f_total_non_avg_up
  # average of total nonanswered questions: upvotes
  f_total_non_avg_up <- mean(nonAnswered_q$upvotes)
  
  # feature 15: f_total_non_avg_no
  # average of total nonanswered questions: totalNumber
  f_total_non_avg_no <- mean(nonAnswered_q$totalNumber)
  
  # feature 16: f_total_non_avg_top
  # average of total nonanswered questions: topQuality
  f_total_non_avg_top <- mean(nonAnswered_q$topQuality)
  
  # feature 17: f_same_non_ans_avg_up
  # average of same label answered questions: upvotes
  f_same_non_ans_avg_up <- mean(nonAnswered_q_qlabel$upvotes)
  
  # feature 18: f_same_non_ans_avg_no
  # average of same label answered questions: totalNumber
  f_same_non_ans_avg_no <- mean(nonAnswered_q_qlabel$totalNumber)
  
  # feature 19: f_same_non_ans_avg_top
  # average of same label answered questions: topQuality
  f_same_non_ans_avg_top <- mean(nonAnswered_q_qlabel$topQuality)
  
  ################################# data frame #####################################
  df <- data.frame(f_total_asked, f_total_ans, 
                   f_total_same_asked, f_total_same_ans, f_total_avg_up, f_total_avg_no, 
                   f_total_avg_top, f_same_ans_avg_up, f_same_ans_avg_no, f_same_ans_avg_top,
                   f_total_non_avg_up, f_total_non_avg_no, f_total_non_avg_top, 
                   f_same_non_ans_avg_up, f_same_non_ans_avg_no, f_same_non_ans_avg_top)
  return(df)
}

# Extract features of wordID
fExtractFeatures_wordID <- function(x) {
  questionID <- x[1]
  userID <- x[2]
  
  qlabel <- filter(question_info, qid == questionID)$label
  ulabel <- filter(user_info, uid == userID)$label
  
  # user asked questions
  asked_qid <- invited_info_train %>%
    filter(uid == as.character(userID))
  
  asked_q <- filter(question_info, qid %in% unique(asked_qid$qid))
  asked_q_qlabel <- filter(asked_q, label == qlabel)
  labels_asked <- asked_q$label
  
  # user answered questions
  answered_qid <- invited_info_train %>%
    filter(uid == as.character(userID), flag == 1) # Is as.character() necessary?
  
  answered_q <- filter(question_info, qid %in% unique(answered_qid$qid))
  answered_q_qlabel <- filter(answered_q, label == qlabel)
  labels_answered <- answered_q$label
  
  # user not answered questions
  nonAnswered_qid <- invited_info_train %>%
    filter(uid == as.character(userID), flag == 0)
  
  nonAnswered_q <- filter(question_info, qid %in% unique(nonAnswered_qid$qid))
  nonAnswered_q_qlabel <- filter(nonAnswered_q, label == qlabel)
  labels_nonAnswered <- nonAnswered_q$label
  
  ################################# wordID #####################################
  # wordID set of current question
  wordIDs_q <- filter(question_info, qid == questionID) %>%
    select(wordID) %>%
    lapply(., str_split, "/") %>%
    lapply(., unlist) %>%
    as.data.frame() %>%
    unique()
  
  # wordID set of userID
  wordIDs_u <- filter(user_info, uid == userID) %>%
    select(wordID) %>%
    lapply(., str_split, "/") %>%
    lapply(., unlist) %>%
    as.data.frame() %>%
    unique()
  
  # wordID set of user answered questions
  wordIDs_answered_total <- answered_q %>%
    select(wordID) %>%
    lapply(., str_split, "/") %>%
    lapply(., unlist) %>%
    as.data.frame() %>%
    unique()
  
  # wordID set of user answered questions with same label
  wordIDs_answered_same <- answered_q_qlabel %>%
    select(wordID) %>%
    lapply(., str_split, "/") %>%
    lapply(., unlist) %>%
    as.data.frame() %>%
    unique()
  
  # wordID set of user nonanswered questions
  wordIDs_nonanswered_total <- nonAnswered_q %>%
    select(wordID) %>%
    lapply(., str_split, "/") %>%
    lapply(., unlist) %>%
    as.data.frame() %>%
    unique()
  
  # wordID set of user answered questions with same label
  wordIDs_nonanswered_same <- nonAnswered_q_qlabel %>%
    select(wordID) %>%
    lapply(., str_split, "/") %>%
    lapply(., unlist) %>%
    as.data.frame() %>%
    unique()
  
  # feature 20: f_wordID_QInU
  # wordIDs_q and wordIDs_u
  f_wordID_QInU <- length(intersect(wordIDs_q$wordID, wordIDs_u$wordID)) / length(union(wordIDs_q$wordID, wordIDs_u$wordID))
  
  # feature 21: f_wordID_total_ans
  # wordIDs_q and wordIDs_answered_total
  f_wordID_total_ans <- length(intersect(wordIDs_q$wordID, wordIDs_answered_total$wordID)) / length(union(wordIDs_q$wordID, wordIDs_answered_total$wordID))
  
  # feature 22: f_wordID_same_ans
  # wordIDs_q and wordIDs_answered_same
  f_wordID_same_ans <- length(intersect(wordIDs_q$wordID, wordIDs_answered_same$wordID)) / length(union(wordIDs_q$wordID, wordIDs_answered_same$wordID))
  
  # feature 23: f_wordID_total_diff
  # wordIDs_q and setdiff(wordIDs_nonanswered_total, wordIDs_answered_total)
  wordIDs_diff <- setdiff(wordIDs_nonanswered_total$wordID, wordIDs_answered_total$wordID)
  f_wordID_total_diff <- length(intersect(wordIDs_q$wordID, wordIDs_diff)) / length(union(wordIDs_q$wordID, wordIDs_diff))
  
  # feature 24: f_wordID_same_diff
  # wordIDs_q and setdiff(wordIDs_nonanswered_same, wordIDs_answered_same)
  wordIDs_diff <- setdiff(wordIDs_nonanswered_same$wordID, wordIDs_answered_same$wordID)
  f_wordID_same_diff <- length(intersect(wordIDs_q$wordID, wordIDs_diff)) / length(union(wordIDs_q$wordID, wordIDs_diff))
  
  ################################# data frame #####################################
  df <- data.frame(f_wordID_QInU, f_wordID_total_ans, f_wordID_same_ans, f_wordID_total_diff, f_wordID_same_diff)
  return(df)
}

# Extract features of charID
fExtractFeatures_charID <- function(x) {
  questionID <- x[1]
  userID <- x[2]
  
  qlabel <- filter(question_info, qid == questionID)$label
  ulabel <- filter(user_info, uid == userID)$label
  
  # user asked questions
  asked_qid <- invited_info_train %>%
    filter(uid == as.character(userID))
  
  asked_q <- filter(question_info, qid %in% unique(asked_qid$qid))
  asked_q_qlabel <- filter(asked_q, label == qlabel)
  labels_asked <- asked_q$label
  
  # user answered questions
  answered_qid <- invited_info_train %>%
    filter(uid == as.character(userID), flag == 1) # Is as.character() necessary?
  
  answered_q <- filter(question_info, qid %in% unique(answered_qid$qid))
  answered_q_qlabel <- filter(answered_q, label == qlabel)
  labels_answered <- answered_q$label
  
  # user not answered questions
  nonAnswered_qid <- invited_info_train %>%
    filter(uid == as.character(userID), flag == 0)
  
  nonAnswered_q <- filter(question_info, qid %in% unique(nonAnswered_qid$qid))
  nonAnswered_q_qlabel <- filter(nonAnswered_q, label == qlabel)
  labels_nonAnswered <- nonAnswered_q$label
  
  ################################# charID #####################################
  # charID set of current question
  charIDs_q <- filter(question_info, qid == questionID) %>%
    select(charID) %>%
    lapply(., str_split, "/") %>%
    lapply(., unlist) %>%
    as.data.frame() %>%
    unique()
  
  # charID set of userID
  charIDs_u <- filter(user_info, uid == userID) %>%
    select(charID) %>%
    lapply(., str_split, "/") %>%
    lapply(., unlist) %>%
    as.data.frame() %>%
    unique()
  
  # charID set of user answered questions
  charIDs_answered_total <- answered_q %>%
    select(charID) %>%
    lapply(., str_split, "/") %>%
    lapply(., unlist) %>%
    as.data.frame() %>%
    unique()
  
  # charID set of user answered questions with same label
  charIDs_answered_same <- answered_q_qlabel %>%
    select(charID) %>%
    lapply(., str_split, "/") %>%
    lapply(., unlist) %>%
    as.data.frame() %>%
    unique()
  
  # charID set of user nonanswered questions
  charIDs_nonanswered_total <- nonAnswered_q %>%
    select(charID) %>%
    lapply(., str_split, "/") %>%
    lapply(., unlist) %>%
    as.data.frame() %>%
    unique()
  
  # charID set of user answered questions with same label
  charIDs_nonanswered_same <- nonAnswered_q_qlabel %>%
    select(charID) %>%
    lapply(., str_split, "/") %>%
    lapply(., unlist) %>%
    as.data.frame() %>%
    unique()
  
  # feature 25: f_charID_QInU
  # charIDs_q and charIDs_u
  f_charID_QInU <- length(intersect(charIDs_q$charID, charIDs_u$charID)) / length(union(charIDs_q$charID, charIDs_u$charID))
  
  # feature 26: f_charID_total_ans
  # charIDs_q and charIDs_answered_total
  f_charID_total_ans <- length(intersect(charIDs_q$charID, charIDs_answered_total$charID)) / length(union(charIDs_q$charID, charIDs_answered_total$charID))
  
  # feature 27: f_charID_same_ans
  # charIDs_q and charIDs_answered_same
  f_charID_same_ans <- length(intersect(charIDs_q$charID, charIDs_answered_same$charID)) / length(union(charIDs_q$charID, charIDs_answered_same$charID))
  
  # feature 28: f_charID_total_diff
  # charIDs_q and setdiff(charIDs_nonanswered_total, charIDs_answered_total)
  charIDs_diff <- setdiff(charIDs_nonanswered_total$charID, charIDs_answered_total$charID)
  f_charID_total_diff <- length(intersect(charIDs_q$charID, charIDs_diff)) / length(union(charIDs_q$charID, charIDs_diff))
  
  # feature 29: f_charID_same_diff
  # charIDs_q and setdiff(charIDs_nonanswered_same, charIDs_answered_same)
  charIDs_diff <- setdiff(charIDs_nonanswered_same$charID, charIDs_answered_same$charID)
  f_charID_same_diff <- length(intersect(charIDs_q$charID, charIDs_diff)) / length(union(charIDs_q$charID, charIDs_diff))
  
  ################################# data frame #####################################
  df <- data.frame(f_charID_QInU, f_charID_total_ans, f_charID_same_ans, f_charID_total_diff, f_charID_same_diff)
  return(df)
}

featureCbind <- function(the_feature, the_dataset) {
  df_features <- data.frame(the_feature)
  featureNum <- length(the_feature[[1]])
  len_df_features <- length(df_features)
  for(i in 1:featureNum) {
    sq <- seq(i, len_df_features, featureNum)
    df <- data.frame(as.numeric(t(df_features[sq])))
    the_dataset <- cbind(the_dataset, df)
  }
  return(the_dataset)
}