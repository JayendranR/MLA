project <- read.csv("BikeDemand.csv")
head(project)
project$Date <- as.Date(project$Date)
project$Month <- format(project$Date, "%B")

project$Seasons <- as.factor(project$Seasons)
project$Holiday <- as.factor(project$Holiday)
project$Functioning.Day <- as.factor(project$Functioning.Day)
project$Month <- as.factor(project$Month)
head(project)

# Split the data into training and testing sets
set.seed(123)
train_index <- sample(1:nrow(project), 0.7 * nrow(project))
train_data <- project[train_index, ]
test_data <- project[-train_index, ]

# Install and load the package
install.packages("randomForest")
library(randomForest)

# Fit a random forest model
rf_model <- randomForest(Rented.Bike.Count ~ . - Date, data = train_data, ntree = 100, mtry = 3)

# Print model summary
print(rf_model)

# Predict on the test set
rf_predictions <- predict(rf_model, test_data)

# Evaluate the model
rf_mse <- mean((rf_predictions - test_data$Rented.Bike.Count)^2)
cat("Random Forest MSE:", rf_mse, "\n")

# Install and load the package
install.packages("ipred")
library(ipred)

# Fit a bagging model
bagging_model <- bagging(Rented.Bike.Count ~ .- Date, data = train_data, nbagg = 50)

# Predict on the test set
bagging_predictions <- predict(bagging_model, test_data)

# Evaluate the model
bagging_mse <- mean((bagging_predictions - test_data$Rented.Bike.Count)^2)
cat("Bagging MSE:", bagging_mse, "\n")

# Install and load the package
install.packages("gbm")
library(gbm)

# Convert the response variable to numeric if necessary
train_data$Demand <- as.numeric(train_data$Rented.Bike.Count)
test_data$Demand <- as.numeric(test_data$Rented.Bike.Count)

# Fit a boosting model
boosting_model <- gbm(Rented.Bike.Count ~ . - Date, data = train_data, 
                      distribution = "gaussian", 
                      n.trees = 100, 
                      interaction.depth = 3, 
                      shrinkage = 0.01, 
                      cv.folds = 5)

# Print model summary
summary(boosting_model)

# Predict on the test set
boosting_predictions <- predict(boosting_model, test_data, n.trees = 100)

# Evaluate the model
boosting_mse <- mean((boosting_predictions - test_data$Rented.Bike.Count)^2)
cat("Boosting MSE:", boosting_mse, "\n")

cat("Random Forest RMSE:", sqrt(rf_mse), "\n")
cat("Bagging RMSE:", sqrt(bagging_mse), "\n")
cat("Boosting RMSE:", sqrt(boosting_mse), "\n")


