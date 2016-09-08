# Byte Cup 2016
# http://biendata.com/competition/bytecup2016/
#
# Created at Sep. 1, 2016
###

### Step 1. Load libraries
library(readr)
library(dplyr)
library(ggplot2)
library(stringr)
# machine learning libraries
library(caret)
library(e1071)
library(kernlab)


# load data
source("load.R")

# statistics of data
source("statistics.R")

# functions defined
source("functions.R")

# preprocess data
source("preprocessing.R")

# models and predict results
source("models/linearModel.R")
source("models/logisticRegression.R")
source("models/svm.R")