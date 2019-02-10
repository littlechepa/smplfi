

setwd("E:\\1 Data Science\\Competitions\\Simplifai\\bank-additional")

#Read in dataset
bank_additional <- read.csv("bank-additional.csv", sep = ";")

#Exclude zero variance variables from the dataset
bank_additional <- bank_additional[,-c(nearZeroVar(bank_additional), 11)]
levels(bank_additional$job)[levels(bank_additional$job) == "blue-collar"] <- "bluecollar"
levels(bank_additional$job)[levels(bank_additional$job) == "self-employed"] <- "selfemployed"

##########################################################################################

#The following function will take a data.frame which contains the original data and checks for
#categorical data & create dummy variables
create_dummies <- function(df,y){ 
  
  
  df2 <- df %>%
    select(-y) #Subsetting all predictors and saving as od2 object
  
  #predictor_names <- names(od2)[which(names(od2) != y)] #Selecting the Independent variables
  trcatch <- c() #To store the column numbers which has/have categorical data.
                 #Later, by using this vector we delete those categorical variables after creating dummies for them
  
  for(i in 1:length(names(df2))){
    if(is.factor(df2[[i]])){ #Executes if the column's class is factor, otherwise will go for next iteration
      if(length(levels(df2[[i]])) == 2){ #If the column has only two levels then we don't need to create separate columns.
        df2[[i]] <- ifelse(df2[[i]] == levels(df2[[i]])[1], 1, 0) #We can simply put binary values in this column
      } else{
        df2<- cbind(df2,dummy(df2[[i]], sep = "_")) #Here, dummy function from dummies package creates dummy variables
        df2 <- df2[-ncol(df2)] #Here we need n-1 dummy variables. This line of code deletes last column created through dummy function
        trcatch <- append(trcatch, i, after = length(trcatch)) #Catching the column number which has categorical data
      }
    }
  }
  if(is.null(trcatch)){
    print("No categorical variables found. So no transformation of data")
    df2 <- cbind(df2, y = df$y) #So no change in od2
  } else{
    df2 <- df2[-trcatch] #The function has already created dummy variables for categorical variable, so delete these categorical variables]
    df2 <- cbind(df2, y = df$y)
  }
}

#Just checking the function 
transformed_data <- create_dummies(bank_additional, y = "y")
str(transformed_data)

##########################################################################################
#Create data partition of 80:20 train and test data

set.seed(123) #For reproducibility

transformed_data <- transformed_data[sample(1:nrow(transformed_data)),]

split <- round(nrow(transformed_data) * 0.8)

train <- transformed_data[1:split,]

test <- transformed_data[(split + 1) : nrow(transformed_data),]

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#Predictive model building using gbm algorithm
set.seed(100)
#Use train function from caret package to train the model using gbm algorithm
gbm_model <- train(y ~ . , 
                   data = train, 
                   method= "gbm", 
                   metric = "ROC",
                   preProc = c("zv", "center", "scale", "pca"),
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

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


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

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#Train the boruta model
boruta.train <- Boruta(y ~ ., data = train, doTrace = 2)
print(boruta.train)

#Take decision on tentative attributes
final.boruta <- TentativeRoughFix(boruta.train)
print(final.boruta)

#obtain the list of confirmed attributes
getSelectedAttributes(final.boruta, withTentative = FALSE)

#create a data frame of the final result derived from Boruta.
boruta.df <- attStats(final.boruta)
class(boruta.df)

print(boruta.df)

boruta_confirmed_formula2 <- getConfirmedFormula(final.boruta)

boruta_imp_features2 <- getSelectedAttributes(final.boruta)


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#################################################BAG MODEL

# Train a bagged model
bag_model <- bagging(formula = y ~ ., 
                     data = both[,c(boruta_imp_features2,"y")],
                     coob = TRUE )

# Generate predictions on the test set
bag_predict_prob <- predict(bag_model,
                            test[,c(boruta_imp_features2,"y")],
                            type = "prob")

# Generate predictions on the test set
bag_predict_class <- predict(bag_model,
                             test[,c(boruta_imp_features2,"y")],
                             type = "class")

#Confusion Matrix
confusionMatrix(bag_predict_class, test$y, positive = "yes")

#ROC curve
colAUC(bag_predict_prob[,2], test$y, plotROC = T)

# Compute the AUC
auc_bag <- auc(ifelse(test$y == "yes", 1,0), bag_predict_prob[,2])          
auc_bag

#################################################GLM MODEL
#Fit the glm model
glm_model <- glm(y ~ ., family = binomial(link = "logit"), data = both[,c(boruta_imp_features2,"y")])

#generate predictions on the test set
glm_predict_prob <- predict(glm_model, test[,c(boruta_imp_features2,"y")], type = "response")

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

glmnet_model <- train(y ~ ., data = both, 
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
glmnet_tune_model <- train(y ~ .,both, 
                           method = "glmnet",
                           tuneGrid = myGrid,
                           preProc = c("zv", "center", "scale", "pca"),
                           metric = "ROC",
                           trControl = myControl)
# Plot results
plot(glmnet_tune_model)

glmnet_tune_predict_class <- predict(object = glmnet_tune_model,test)

glmnet_tune_predict_prob <- predict(object = glmnet_tune_model,test, type = "prob")

#plot auc curve
colAUC(glmnet_tune_predict_prob$yes, test$y, plotROC = T)

#Confusion Matrix
confusionMatrix(glmnet_tune_predict_class, test$y, positive = "yes")

#################################################GLMNET WITH TUNE GIRD ON BALANCED DATA

# Fit a model
set.seed(42)
glmnet_tune_impf_model <- train(y ~ .,both[,c(boruta_imp_features2,"y")], 
                                method = "glmnet",
                                tuneGrid = myGrid,
                                preProc = c("zv", "center", "scale", "pca"),
                                metric = "ROC",
                                trControl = myControl)
# Plot results
plot(glmnet_tune_impf_model)

glmnet_tune_impf_predict_class <- predict(object = glmnet_tune_impf_model,test[,c(boruta_imp_features2,"y")])

glmnet_tune_impf_predict_prob <- predict(object = glmnet_tune_impf_model,test[,c(boruta_imp_features2,"y")], type = "prob")

#plot auc curve
colAUC(glmnet_tune_impf_predict_prob$yes, test$y, plotROC = T)

#Confusion Matrix
confusionMatrix(glmnet_tune_impf_predict_class, test$y, positive = "yes")

#################################################SVM MODEL
#Fit model
svm_model <- svm(y ~ .,data = both[,c(boruta_imp_features2,"y")],kernel="linear",gamma=0.1,cost=0.01)

svm_predict_class <- predict(object = svm_model, test[,c(boruta_imp_features2,"y")])

confusionMatrix(svm_predict_class, test$y, positive = "yes")

#################################################GBM MODEL
gbm_impf_model <- train(y ~ . , 
                        data = both[,c(boruta_imp_features2,"y")], 
                        method= "gbm", 
                        metric = "ROC",
                        preProc = c("zv", "center", "scale"),
                        trControl = trainControl(method= "cv",
                                                 number=5,
                                                 summaryFunction = twoClassSummary,
                                                 classProbs = TRUE))

gbm_impf_predict_class <- predict(object = gbm_impf_model, test[,c(boruta_imp_features2,"y")], type= "raw")   

gbm_impf_predict_prob <- predict(object = gbm_impf_model, test[,c(boruta_imp_features2,"y")], type = "prob")

#Confusion Matrix
confusionMatrix(gbm_impf_predict_class, test$y, positive = "yes")

colAUC(gbm_impf_predict_prob$yes, test$y, plotROC = TRUE)

#####################################

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


#Ensemble data.frame
ensemble_df <- data.frame(bag = bag_predict_class,
                          glm = glm_predict_calss,
                          glmnet = glmnet_predict_class,
                          glmnet_tune = glmnet_tune_predict_class,
                          glmnet_tune_impf = glmnet_tune_impf_predict_class,
                          svm = svm_predict_class,
                          gbm = gbm_impf_predict_class)
head(ensemble_df)

ensemble_df <- as.data.frame(ifelse(ensemble_df == "yes",1,0))

ensemble_df$vote_count <- rowSums(ensemble_df)

ensemble_df$prediction <- ifelse(ensemble_df$vote_count >= 4, "yes", "no")

confusionMatrix(ensemble_df$prediction, test$y, positive = "yes")

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


# List of predictions
preds_list <- list(bag_predict_prob[,2],
                   glm_predict_prob,
                   glmnet_predict_prob$yes,
                   glmnet_tune_predict_prob$yes,
                   glmnet_tune_impf_predict_prob$yes,
                   gbm_impf_predict_prob$yes
)

# List of actual values (same for all)
m <- length(preds_list)
actuals_list <- rep(list(test$y), m)

# Plot the ROC curves
pred <- prediction(preds_list, actuals_list)
rocs <- performance(pred, "tpr", "fpr")
plot(rocs, col = as.list(1:m), main = "Test Set ROC Curves")
legend(x = "bottomright", 
       legend = c("bagging","glm","glmnet","glmnet_t","glmnet_t_if","gbm"),
       fill = 1:m)



#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#train the model
set.seed(42)
glmnet_tune_impf_model <- train(y ~ .,under[,c(boruta_imp_features2,"y")], method = "glmnet",
                                tuneGrid = myGrid, trControl = myControl)
# Plot results
plot(model)

glmnet_tune_impf_predict_class <- predict(object = glmnet_tune_impf_model,test[,c(boruta_imp_features2,"y")])

glmnet_tune_impf_predict_prob <- predict(object = glmnet_tune_impf_model,test[,c(boruta_imp_features2,"y")], type = "prob")

#plot auc curve
colAUC(glmnet_tune_impf_predict_prob$yes, test$y, plotROC = T)

#Confusion Matrix
confusionMatrix(glmnet_tune_impf_predict_class, test$y, positive = "yes")


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
