# Install and load the necessary packages
if (!require(MASS)) install.packages("MASS")
if (!require(caret)) install.packages("caret")

library(MASS)
library(caret)

# Load the Boston housing dataset
data("Boston")

# View the first few rows of the dataset
head(Boston)

# Check the structure of the dataset
str(Boston)

# Split the data into training and testing sets
set.seed(123)  # For reproducibility
train_index <- createDataPartition(Boston$medv, p = 0.7, list = FALSE)
train_data <- Boston[train_index, ]
test_data <- Boston[-train_index, ]

# Create a linear regression model to predict medv (median value of owner-occupied homes)
linreg_model <- lm(medv ~ ., data = train_data)

# Summarize the model
summary(linreg_model)

# Make predictions on the test set
predictions <- predict(linreg_model, newdata = test_data)

# Compare predictions to actual values
results <- data.frame(Actual = test_data$medv, Predicted = predictions)

# Calculate the mean squared error (MSE)
mse <- mean((results$Actual - results$Predicted)^2)
print(paste("Mean Squared Error:", mse))

# Optional: Plot actual vs. predicted values
library(ggplot2)
ggplot(results, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Actual vs Predicted Values", x = "Actual MEDV", y = "Predicted MEDV")
