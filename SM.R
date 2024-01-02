svm_model <- tune(svm, surv2m~., data = train_data, kernel = "radial", ranges = list(cost = c(0.1,1,10,100,1000), gamma = c(0.5,1,2,3,4)))

# Make predictions on the test set
svm_predictions <- predict(svm_model$best.model, test_data)
table(svm_predictions)

# Evaluate the model's performance
svm_conf_matrix <- confusionMatrix(svm_predictions, test_data$surv2m)
svm_conf_matrix