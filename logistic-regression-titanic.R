# Install and load the necessary packages
if (!require(titanic)) install.packages("titanic")
if (!require(dplyr)) install.packages("dplyr")
if (!require(caret)) install.packages("caret")

library(titanic)
library(dplyr)
library(caret)

# Load the Titanic dataset
data("titanic_train")

# View the first few rows of the dataset
head(titanic_train)

# Select relevant columns and remove rows with missing values
titanic_clean <- titanic_train %>%
  select(Survived, Pclass, Sex, Age) %>%
  filter(!is.na(Age))

# Convert categorical variables to factors
titanic_clean <- titanic_clean %>%
  mutate(Survived = as.factor(Survived),
         Pclass = as.factor(Pclass),
         Sex = as.factor(Sex))

# Split the data into training and testing sets
set.seed(123)  # For reproducibility
train_index <- createDataPartition(titanic_clean$Survived, p = 0.7, list = FALSE)
train_data <- titanic_clean[train_index, ]
test_data <- titanic_clean[-train_index, ]

# Create a logistic regression model to predict Survived
logit_model <- glm(Survived ~ Pclass + Sex + Age, data = train_data, family = binomial)

# Summarize the model
summary(logit_model)

# Make predictions on the test set
predictions_prob <- predict(logit_model, newdata = test_data, type = "response")
predictions <- ifelse(predictions_prob > 0.5, 1, 0)

# Convert predictions to factor
predictions <- as.factor(predictions)

# Evaluate the model by comparing predictions to actual values
confusion_matrix <- confusionMatrix(predictions, test_data$Survived)
print(confusion_matrix)

                                   