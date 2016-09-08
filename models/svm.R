# svm.R
#   svm
#
# 2016.09.08

# svm: support vector machine
model_svm <- svm(flag ~ ., data = myDataset_train, method = "C-classification", 
                 kernel = "radial", cost = 10, gamma = 0.1)
predicted_svm <- predict(model_svm, newData = myDataset_predict)
df_svm <- as.data.frame(predicted_svm)
names(df_svm) <- "label"
result_svm <- cbind(validate_nolabel, df_svm)
write.table(result_svm, file = "result/result_svm.csv", fileEncoding = "utf-8", 
            sep = ",", row.names = F, col.names = T, quote = F)