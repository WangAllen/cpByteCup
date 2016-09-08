# linearModel.R
#   linear model
#
# 2016.09.08

# lm: linear model
model_lm <- lm(flag ~ ., data = myDataset_train)
predicted_lm <- predict(model_lm, newData = myDataset_predict)
df_lm <- as.data.frame(predicted_lm)
names(df_lm) <- "label"
result_lm <- cbind(validate_nolabel, df_lm)
write.table(result_lm, file = "result/result_lm.csv", fileEncoding = "utf-8", 
            sep = ",", row.names = F, col.names = T, quote = F)
