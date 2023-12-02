# Load necessary libraries
library(randomForest)
library(ggplot2)

# Load and preprocess data
twitch_data <- read.csv("/Users/chichenchao/Downloads/twitchdata-update.csv")
twitch_data <- na.omit(twitch_data)

# Log-transform the Followers.gained column and handle zeros (adding 1 to avoid log(0))
twitch_data$Followers.gained <- log(twitch_data$Followers.gained + 1)

# Exclude non-numeric attributes
attributes_to_exclude <- c("Channel", "Partnered", "Mature", "Language")
attributes_to_include <- setdiff(names(twitch_data), attributes_to_exclude)
train_data <- twitch_data[train_indices, attributes_to_include]
test_data <- twitch_data[-train_indices, attributes_to_include]

# Random Forest Model Training
set.seed(100)
train_indices <- sample(1:nrow(train_data), 0.8 * nrow(train_data))

# Assuming twitch_data is preprocessed

# Adjust mtry to a standard approach
mtry_value <- round(sqrt(ncol(train_data)))
train_data = na.omit(train_data)
# Train the Random Forest model
rf_model_twitch <- randomForest(Followers.gained ~ ., 
                                data = train_data, 
                                ntree = 100, 
                                mtry = mtry_value,
                                importance = TRUE)
clean_test_data <- test_data[!is.nan(test_data$Followers.gained), ]
yhat.rf <- predict(rf_model_twitch, newdata = clean_test_data)

# Predictions and MSE calculation on the test data (ensure test_data is preprocessed)
#yhat.rf <- predict(rf_model_twitch, newdata = test_data)
#mse_rf_twitch <- mean((exp(yhat.rf) - 1 - exp(test_data$Followers.gained - 1))^2)
mse_rf_twitch <- mean((yhat.rf - clean_test_data$Followers.gained)^2)

# Printing MSE
print(mse_rf_twitch)

# Variable Importance Plot
varImpPlot(rf_model_twitch)

MSE=rep(0,10)
for (i in 1:10) {
  # Set seed for reproducibility
  set.seed(i)
  
  # Sample 80% of the data as training set
  train_indices <- sample(1:nrow(train_data), 0.8 * nrow(train_data))
  test_data <- train_data[-train_indices, ]
  
  # Train the Random Forest model
  followers.rf <- randomForest(Followers.gained ~., 
                               data = train_data, 
                               subset=train_indices, 
                               
                               mtry = (ncol(twitch_data)-1)/3,
                               importance=TRUE)
  
  # Make predictions on the test set
  yhat.rf <- predict(followers.rf, newdata = test_data)
  
  # Extract actual scores of the test set
  test.score <- test_data$Followers.gained
  
  # Compute and store MSE for this iteration
  MSE[i] <- mean((yhat.rf - test.score)^2)
}
