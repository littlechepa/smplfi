
#Contingency table of target variable
table(train$y)

#Create balanced data using over sampling.

over <- ovun.sample(y~., data = train, method = "over", N = 5880 )$data

table(over$y)

summary(over)

#Create balanced data using under sampling.

under <- ovun.sample(y~., data=train, method = "under", N = 710)$data

table(under$y)


#Create balanced data using over & under sampling.

both <- ovun.sample(y~., data=train, method = "both",
                    p = 0.5,
                    seed = 222,
                    N = 3295)$data
table(both$y)



###############

# Predictive Model (Random Forest)
rftrain <- randomForest(y~., data = train)
rfover <- randomForest(y~., data = over)
rfunder <- randomForest(y~., data=under)
rfboth <-randomForest(y~., data=both)


# Predictive Model Evaluation with test data
confusionMatrix(predict(rftrain, test), test$y, positive = "yes")
confusionMatrix(predict(rfover, test), test$y, positive = "yes")
confusionMatrix(predict(rfunder, test), test$y, positive = "yes")
confusionMatrix(predict(rfboth, test), test$y, positive = "yes")

#under sample is giving highest sensitivity values
