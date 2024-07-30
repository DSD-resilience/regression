# Logistic Regression using R
# Load the necessary library
library(datasets)

# Load the mtcars dataset
data("mtcars")

# View the first few rows of the dataset
head(mtcars)

# Create a logistic regression model to predict 'am' based on 'mpg'
model <- glm(am ~ mpg, data = mtcars, family = binomial)

# Summarize the model
summary(model)

