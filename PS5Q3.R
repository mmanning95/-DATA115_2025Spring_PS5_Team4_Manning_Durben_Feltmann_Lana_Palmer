
data <- read.csv("defaults.csv")
data$default <- as.factor(data$default) 
model <- glm(default ~ balance, data = data, family = binomial)

coefficients <- coef(model)
cat("Coefficients:\n")
print(coefficients)
cat("\nLog odds equation: log(odds) =", coefficients[1], "+", coefficients[2], "* balance\n")

predicted_probs <- predict(model, type = "response")
predicted_classes <- ifelse(predicted_probs > 0.5, "Yes", "No")
conf_matrix <- table(Actual = data$default, Predicted = predicted_classes)
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix) * 100
cat("\nCorrect classification percentage:", round(accuracy, 2), "%\n")

new_data <- data.frame(balance = 1950)
predicted_prob <- predict(model, newdata = new_data, type = "response")
predicted_class <- ifelse(predicted_prob > 0.5, "Yes", "No")
cat("\nPrediction for balance = 1950:", predicted_class,
    "\nProbability of default:", round(predicted_prob, 4))
