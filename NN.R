# Select relevant columns
nn_data <- data %>%
  select(aps, surv2m) %>%
  na.omit()

# Split the data into training and testing sets
set.seed(555)
print(1:nrow(nn_data))
indices <- sample(1:nrow(nn_data), 0.8 * nrow(nn_data))
train_data <- nn_data[indices, ]
test_data <- nn_data[-indices, ]

boxplot(train_data$aps, test_data$aps)
boxplot(train_data$surv2m, test_data$surv2m)


# Create and train the neural network
nn <- neuralnet(surv2m ~ aps, data = train_data, hidden = c(5, 3), linear.output = TRUE)
plot(nn)


# Scale
max <- apply(nn_data, 2, max)
min <- apply(nn_data, 2, min)
nn_data_scaled <- as.data.frame(scale(nn_data, center = min, scale = max - min))
train_scaled <- nn_data_scaled %>% slice(indices)
test_scaled <- nn_data_scaled %>% slice(-indices)

# Make predictions on the test set
predictions <- predict(nn, test_data)
plot(test_data$surv2m, predictions, col='blue', pch=16, ylab = "predicted rating NN", xlab = "real surv2m")
abline(0,1)

#pred_scaled <- predict(nn, test_scaled)
#predictions <- pred_scaled * (max(nn_data$surv2m) - min(nn_data)) + min(nn_data$surv2m)
#plot(test_scaled$surv2m, predictions, col='blue', pch=16, ylab = "predicted rating NN", xlab = "real surv2m")
#abline(0,1)

# Evaluate the performance of the neural network (you may need to choose an appropriate evaluation metric)
# For example, if 'surv2m' is continuous, you can use metrics like Mean Squared Error (MSE)
mse <- mean((predictions - test_data$surv2m)^2)

# Print the Mean Squared Error
cat("Mean Squared Error:", mse)



# Redo with caret using k-fold Cross Validation
models <- train(
  x = nn_data %>% select(-surv2m),
  y = nn_data_scaled %>% pull(surv2m),
  method = 'neuralnet', metric = 'RMSE',
  linear.output = TRUE,
  # be careful, does only work on x!
  preProcess = c('center', 'scale'),
  tuneGrid = expand.grid(.layer1=c(1:4), .layer2=c(0,2), .layer3=c(0)),
  trControl = trainControl(
    method = 'repeatedcv',
    number = 5,
    repeats = 10,
    returnResamp = 'final'
  )
)
plot(models)
