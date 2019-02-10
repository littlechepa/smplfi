
#Predictive model building using gbm algorithm
set.seed(100)
#Use train function from caret package to train the model using gbm algorithm
gbm_model <- train(y ~ . , 
                   data = train, 
                   method= "gbm", 
                   metric = "ROC",
                   preProc = c("zv", "center", "scale"),
                   trControl = trainControl(method= "cv",
                                            number=3,
                                            summaryFunction = twoClassSummary,
                                            classProbs = TRUE))

summary(gbm_model)

print(gbm_model)

# Generate predictions on the test set 

gbm_predict_class <- predict(object = gbm_model, test, type= "raw")   

gbm_predict_prob <- predict(object = gbm_model, test, type = "prob")

#Confusion Matrix
confusionMatrix(gbm_predict_class, test$y, positive = "yes")

colAUC(gbm_predict_prob$yes, test$y, plotROC = TRUE)

#Sensitivity or recall or TPR(True positive rate) or hit rate is not so impressive
#This is becasue imbalance in target variabe

#plot the proportions of imbalanced target variable
barplot(prop.table(table(bank_additional$y)),
        col = rainbow(2),
        ylim = c(0, 0.7),
        main = "Class Distribution")

#Check the proportion of classes of target variable
prop.table(table(train$y))

prop.table(table(test$y))

#Target variable consists with 90 percent of NOs and only 10 pecent of YESs

