
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
