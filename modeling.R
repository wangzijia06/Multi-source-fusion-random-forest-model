library(caret)
library(randomForest)

# --- 递归特征消除 (RFE) ---
set.seed(123)
control <- rfeControl(functions = rfFuncs, method = "cv", number = 10)

# 执行 RFE
# x: 特征矩阵, y: 水深标签
rfe_result <- rfe(x = final_train_df[, -ncol(final_train_df)], 
                  y = final_train_df$depth,
                  sizes = c(10, 15, 20, 25), 
                  rfeControl = control)

# 获取最佳特征子集
selected_features <- predictors(rfe_result)
print(paste("Selected Features:", paste(selected_features, collapse = ", ")))

# --- 训练最终随机森林模型 ---
# 使用优选后的特征进行训练
rf_final <- randomForest(
  x = final_train_df[, selected_features],
  y = final_train_df$depth,
  ntree = 500,
  importance = TRUE,
  na.action = na.omit
)

# 查看变量重要性
varImpPlot(rf_final)