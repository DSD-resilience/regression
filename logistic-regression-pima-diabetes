# Install and load the necessary packages
if (!require(mlbench)) install.packages("mlbench")
if (!require(caret)) install.packages("caret")

library(mlbench)
library(caret)

# Load the PimaIndiansDiabetes dataset
data("PimaIndiansDiabetes")

# View the first few rows of the dataset
head(PimaIndiansDiabetes)

# Check for missing values
sum(is.na(PimaIndiansDiabetes))

# Split the data into training and testing sets
set.seed(123)  # For reproducibility
train_index <- createDataPartition(PimaIndiansDiabetes$diabetes, p = 0.7, list = FALSE)
train_data <- PimaIndiansDiabetes[train_index, ]
test_data <- PimaIndiansDiabetes[-train_index, ]

# Create a logistic regression model to predict diabetes
logit_model <- glm(diabetes ~ ., data = train_data, family = binomial)

# Summarize the model
summary(logit_model)

# Make predictions on the test set
predictions_prob <- predict(logit_model, newdata = test_data, type = "response")
predictions <- ifelse(predictions_prob > 0.5, "pos", "neg")

# Convert predictions to factor
predictions <- factor(predictions, levels = c("neg", "pos"))

# Evaluate the model by comparing predictions to actual values
confusion_matrix <- confusionMatrix(predictions, test_data$diabetes)
print(confusion_matrix)
