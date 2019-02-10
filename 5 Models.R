#################################################BAG MODEL

# Train a bagged model
bag_model <- bagging(formula = y ~ ., 
                     data = under[,c(boruta_imp_features,"y")],
                     coob = TRUE )

# Generate predictions on the test set
bag_predict_prob <- predict(bag_model,
                    test[,c(boruta_imp_features,"y")],
                    type = "prob")

# Generate predictions on the test set
bag_predict_class <- predict(bag_model,
                                test[,c(boruta_imp_features,"y")],
                                type = "class")

#Confusion Matrix
confusionMatrix(bag_predict_class, test$y, positive = "yes")

#ROC curve
colAUC(bag_predict_prob[,2], test$y, plotROC = T)

# Compute the AUC
auc_bag <- auc(ifelse(test$y == "yes", 1,0), bag_predictions_prob[,2])          
auc_bag

#################################################GLM MODEL
#Fit the glm model
glm_model <- glm(y ~ ., family = binomial(link = "logit"), data = under[,c(boruta_imp_features,"y")])

#generate predictions on the test set
glm_predict_prob <- predict(glm_model, test[,c(boruta_imp_features,"y")], type = "response")

glm_predict_calss <- factor(ifelse(glm_predict_prob >.55, "yes", "no"))

#plot auc curve
colAUC(glm_predict_prob,  test$y,plotROC = T)

#confustinmatrix
confusionMatrix(glm_predict_calss, test$y, positive = "yes")

#################################################GLMNET MODEL
#Fit glmnet model
# Make a custom trainControl
myControl <- trainControl(
  method = "cv", number = 5,
  summaryFunction = twoClassSummary,
  classProbs = TRUE, 
  verboseIter = TRUE)

# Fit a model
set.seed(42)

glmnet_model <- train(y ~ ., data = under, 
                      method = "glmnet",
                      trControl = myControl)

plot(glmnet_model)

#generate predictions
glmnet_predict_class <- predict(object = glmnet_model,test)

glmnet_predict_prob <- predict(object = glmnet_model,test, type = "prob")

#plot auc curve
colAUC(glmnet_predict_prob$yes, test$y, plotROC = T)

#Confusion Matrix
confusionMatrix(glmnet_predict_class, test$y, positive = "yes")


#################################################GLMNET MODEL WITH TUNE GRID
# Make a custom tuning grid
myGrid <- expand.grid(alpha = 0:1,lambda = seq(0.0001, 0.1, length = 10))

# Fit a model
set.seed(42)
glmnet_tune_model <- train(y ~ .,under, method = "glmnet",
                           tuneGrid = myGrid, trControl = myControl)
# Plot results
plot(model)

glmnet_tune_predict_class <- predict(object = glmnet_tune_model,test)

glmnet_tune_predict_prob <- predict(object = glmnet_tune_model,test, type = "prob")

#plot auc curve
colAUC(glmnet_tune_predict_prob$yes, test$y, plotROC = T)

#Confusion Matrix
confusionMatrix(glmnet_tune_predict_class, test$y, positive = "yes")

#################################################GLMNET WITH TUNE GIRD ON BALANCED DATA

# Fit a model
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

#################################################SVM MODEL
#Fit model
svm_model <- svm(y ~ .,data = under,kernel="linear",gamma=0.1,cost=0.01)

svm_predict_class <- predict(object = svm_model, test)

confusionMatrix(svm_predict_class, test$y, positive = "yes")

#################################################GBM MODEL
gbm_impf_model <- train(y ~ . , 
                   data = under[,c(boruta_imp_features,"y")], 
                   method= "gbm", 
                   metric = "ROC",
                   preProc = c("zv", "center", "scale"),
                   trControl = trainControl(method= "cv",
                                            number=3,
                                            summaryFunction = twoClassSummary,
                                            classProbs = TRUE))

gbm_impf_predict_class <- predict(object = gbm_impf_model, test, type= "raw")   

gbm_impf_predict_prob <- predict(object = gbm_impf_model, test, type = "prob")

#Confusion Matrix
confusionMatrix(gbm_impf_predict_class, test$y, positive = "yes")

colAUC(gbm_impf_predict_prob$yes, test$y, plotROC = TRUE)

#####################################

