# logisticRegression.R
#   logistic regression model
#
# 2016.09.08

# glm: logistic regression
model_glm <- glm(flag ~ f_label + f_wordID + f_charID, data = myDataset_train)
predicted_glm <- predict(model_glm, newData = myDataset_predict)
df_glm <- as.data.frame(predicted_glm)
names(df_glm) <- "label"
result_glm <- cbind(validate_nolabel, df_glm)
write.table(result_glm, file = "result/result_glm.csv", fileEncoding = "utf-8", 
            sep = ",", row.names = F, col.names = T, quote = F)

