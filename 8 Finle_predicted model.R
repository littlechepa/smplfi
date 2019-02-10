
#train the model
set.seed(42)
glmnet_tune_impf_model <- train(y ~ .,under[,c(boruta_imp_features,"y")], method = "glmnet",
                                tuneGrid = myGrid, trControl = myControl)
# Plot results
plot(model)

glmnet_tune_impf_predict_class <- predict(object = glmnet_tune_impf_model,test[,c(boruta_imp_features,"y")])

glmnet_tune_impf_predict_prob <- predict(object = glmnet_tune_impf_model,test[,c(boruta_imp_features,"y")], type = "prob")

#plot auc curve
colAUC(glmnet_tune_impf_predict_prob$yes, test$y, plotROC = T)

#Confusion Matrix
confusionMatrix(glmnet_tune_impf_predict_class, test$y, positive = "yes")
