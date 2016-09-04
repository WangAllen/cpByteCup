# Byte Cup 2016
# http://biendata.com/competition/bytecup2016/
#
# Created at Sep. 1, 2016
###

### Introduction
## Tasks:
#     参赛队伍利用给定的头条问答数据（包括专家标签、问题数据以及问题分发数据，详见数据描述部分），进
# 行针对问题的专家挖掘。
#     给定若干问题，参赛者需要预测哪些专家更有可能回答这些问题。具体的，针对每个问题和一位候选专家，
# 参赛者需要根据计算该专家回答问题的概率。实际运营中，系统会优先向回答概率高的候选专家发送这个问题的
# 回答邀请，直到收到的回答数量达到指定阈值。评估方面，给定一个问题，我们会按照预测概率把候选专家排序
# ，并分别评估排序结果的NDCG@5，NDCG@10最后评分公式为 NDCG@5 * 0.5 + NDCG@10 * 0.5
###

### Step 1. Load data
library(readr)
library(dplyr)
library(ggplot2)

user_info <- read.table(file = "data/user_info.txt", header = F, sep = "\t")
colnames(user_info) <- c("uid", "label", "wordID", "charID")

question_info <- read.table(file = "data/question_info.txt", header = F, sep = "\t")
colnames(question_info) <- c("qid", "label", "wordID", "charID", "topQuality", "totalNumber", "upvotes")

invited_info_train <- read.table(file = "data/invited_info_train_new.txt", header = F, sep = "\t")
colnames(invited_info_train) <- c("qid", "uid", "flag")

validate_nolabel <- read.table(file = "data/validate_nolabel.txt", header = F, sep = ",")
colnames(validate_nolabel) <- c("qid", "uid")

### To be added after Nov. 11
# invited_info_validate
# invited_info_test

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

all(res_tq_invited$InOrNot)
all(res_tu_invited$InOrNot)

### Check if the users of invited_info_train and validate_nolabel are all in user_info
res_tq_validate <- data.frame(InOrNot = tq_validate$Var1 %in% question_info$qid)
res_tu_validate <- data.frame(InOrNot = tu_validate$Var1 %in% user_info$uid)

all(res_tq_validate$InOrNot)
all(res_tu_validate$InOrNot)

## Feature 1: label
lb <- data.frame(labelFlag = 1:nrow(invited_info_train) * 0, 
                 wordFlag = 1:nrow(invited_info_train) * 0,
                 charFlag = 1:nrow(invited_info_train) * 0)

library(stringr)
lbfun <- function(x) {
  q <- filter(question_info, qid == x[1])
  u <- filter(user_info, uid == x[2])
  
  q_label <- q$label
  u_label <- unlist(str_split(u$label, "/"))
  if(q_label %in% u_label)
    lb$labelFlag <- 1
}

library(dplyr)
testa <- head(validate_nolabel) %>%
  lapply(lbfun)

hval <- head(validate_nolabel)
testa <- apply(hval, 1, lbfun)

apply(validate_nolabel, 1, lbfun)


# collect all the user's wordIDs
labels <- user_info %>%
  select(label) %>%
  lapply(., str_split, "/") %>%
  lapply(unlist) %>%
  as.data.frame() %>%
  unique()

gWord_user <- function(lab) {
  res <- user_info %>%
    filter(label == as.character(lab)) %>%
    select(wordID) %>%
    lapply(., str_split, "/") %>%
    lapply(unlist) %>%
    unique()
  return(res)
}

wordIDSet_user <- lapply(labels$label, gWord_user)

# get all the wordIDs of the user through collect the questions she answered.
fWordIDs_user <- function(userID) {
  answered_q <- invited_info_train %>%
    filter(uid == userID, flag == 1) %>%
    select(qid) %>%
    unique()
  
  wordIDs <- filter(question_info, qid %in% answered_q$qid) %>%
    select(wordID) %>%
    lapply(., str_split, "/") %>%
    lapply(., unlist) %>%
    unique() %>%
    unlist()
  
  return(wordIDs)
}

## test fWordIDs_user
fWordIDs_user("e6a2ecac7f90d426103de95ba7f6d2b0")