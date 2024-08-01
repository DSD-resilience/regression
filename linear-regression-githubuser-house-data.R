# Load the necessary packages
install.packages("dplyr")
library(dplyr)

# URL of the dataset
url <- "https://raw.githubusercontent.com/jbrownlee/Datasets/master/housing.data"

# Column names based on the description of the dataset
col_names <- c("CRIM", "ZN", "INDUS", "CHAS", "NOX", "RM", "AGE", "DIS", "RAD", "TAX", "PTRATIO", "B", "LSTAT", "MEDV")

# Import the dataset
housing_data <- read.table(url, header = FALSE, col.names = col_names)

# View the first few rows of the dataset
head(housing_data)

# Explore the structure of the dataset
str(housing_data)

# Split the data into training and testing sets
set.seed(123)  # For reproducibility
train_indices <- sample(1:nrow(housing_data), size = 0.7 * nrow(housing_data))
train_data <- housing_data[train_indices, ]
test_data <- housing_data[-train_indices, ]

# Create a linear regression model to predict MEDV based on other features
model <- lm(MEDV ~ ., data = train_data)

# Summarize the model
summary(model)

# Make predictions on the test set
predictions <- predict(model, newdata = test_data)

# Evaluate the model by comparing predictions to actual values
results <- data.frame(Actual = test_data$MEDV, Predicted = predictions)
head(results)

# Calculate the mean squared error (MSE)
mse <- mean((results$Actual - results$Predicted)^2)
print(paste("Mean Squared Error:", mse))

# Plot actual vs predicted values
library(ggplot2)
ggplot(results, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Actual vs Predicted Values", x = "Actual MEDV", y = "Predicted MEDV")
