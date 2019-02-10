
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


