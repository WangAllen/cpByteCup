# functions.R
#   functions
#
# 2016.09.08

# extract features (generate new cols) for invited_info_train and validate_nolabel
#  by label, wordID and charID
# used dataset: question_info, user_info, invited_info_train
### added @2016.09.05
fExtractFeatures <- function(x) {
  questionID <- x[1]
  userID <- x[2]
  
  # labels
  qlabel <- filter(question_info, qid == questionID)$label
  ulabel <- filter(user_info, uid == userID)$label
  u_label <- unlist(str_split(ulabel, "/"))
  if(qlabel %in% u_label)
    f_label <- 1
  else
    f_label <- 0
  
  # user answered questions
  answered_q <- invited_info_train %>%
    filter(uid == as.character(userID), flag == 1) %>% # Is as.character() necessary?
    select(qid) %>%
    unique()
  
  # wordID set of userID answered questions with the same label to current one
  wordIDs_answered <- filter(question_info, qid %in% answered_q$qid, label == as.character(qlabel)) %>%
    select(wordID) %>%
    lapply(., str_split, "/") %>%
    lapply(., unlist) %>%
    as.data.frame() %>%
    unique()
  
  # current question wordIDs
  wordIDs_q <- filter(question_info, qid == questionID) %>%
    select(wordID) %>%
    lapply(., str_split, "/") %>%
    lapply(., unlist) %>%
    as.data.frame() %>%
    unique()
  
  # feature f_wordID
  f_wordID <- length(intersect(wordIDs_q$wordID, wordIDs_answered$wordID)) / length(union(wordIDs_q$wordID, wordIDs_answered$wordID))
  
  # charID set of userID answered questions with the same label to current one
  charIDs_answered <- filter(question_info, qid %in% answered_q$qid, label == as.character(qlabel)) %>%
    select(charID) %>%
    lapply(., str_split, "/") %>%
    lapply(., unlist) %>%
    as.data.frame() %>%
    unique()
  
  # current question charIDs
  charIDs_q <- filter(question_info, qid == questionID) %>%
    select(charID) %>%
    lapply(., str_split, "/") %>%
    lapply(., unlist) %>%
    as.data.frame() %>%
    unique()
  
  # feature f_charID
  f_charID <- length(intersect(charIDs_q$charID, charIDs_answered$charID)) / length(union(charIDs_q$charID, charIDs_answered$charID))
  
  df <- data.frame(f_label, f_wordID, f_charID)
  return(df)
}