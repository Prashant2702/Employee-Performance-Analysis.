library(dplyr)  # For data manipulation
library(ggplot2)  # For data visualization

library(stats)
library(caret)

data <- read.csv("C:\\Users\\Dell\\OneDrive\\Desktop\\prashant_data.csv", header = TRUE, sep = ",")
df=data.frame(data)
df


#Structure of data
str(df)

# View the first few rows of the dataset
head(df)

# Summary statistics for numeric columns
summary(df)


#to know data type of each row
data_types <- sapply(df, class)
print(data_types)

# Check for null values 
na2 <- sum(is.na(df))
na2
df<-na.omit(df)
na2 <- sum(is.na(df))
na2

df$Years.of.Experience <- as.integer(df$Years.of.Experience)


data_types <- sapply(df, class)
print(data_types)


#encoding of varaibles
df$Gender <- as.integer(factor(df$Gender, levels = c("Male", "Female")))
df$Education.Level<-as.integer(factor(df$Education.Level,levels = c("Bachelor's","Master's","PhD")))

df
data_types <- sapply(df, class)
print(data_types)

numeric_columns <- df[, sapply(df, is.numeric)]

# Calculate the correlation matrix for the numeric columns
correlation_matrix <- cor(numeric_columns)

# Display the correlation matrix
print(correlation_matrix)

# Load the required library for random forest
library(randomForest)

# Assuming df is your dataset
set.seed(123)  # for reproducibility

# Set the proportion for the test set (e.g., 70% training and 30% testing)
test_size <- 0.3

# Create an index for the test set
test_index <- createDataPartition(df$Salary, p = test_size, list = FALSE)

# Split the data into training and testing sets
train_data <- df[-test_index, ]  # Training data
test_data <- df[test_index, ]    # Testing data

# Fit a Random Forest regression model on the training data
rf_model <- randomForest(Salary ~ Age+Gender+Education.Level+Years.of.Experience , data = train_data, ntree = 100)

# Make predictions on the test data
rf_predictions <- predict(rf_model, newdata = test_data)

# Evaluate the model on the test data
rf_rmse <- sqrt(mean((test_data$Salary - rf_predictions)^2))
rf_r_squared <- 1 - sum((test_data$Salary - rf_predictions)^2) / sum((test_data$Salary - mean(test_data$Salary))^2)

# Print RMSE and R-squared
cat("Random Forest RMSE: ", rf_rmse, "\n")
cat("Random Forest R-squared: ", rf_r_squared, "\n")

# Define a threshold for accuracy (e.g., within 90%)
accuracy_threshold <- 0.90

# Calculate the percentage of predictions within the threshold
accuracy_percentage <- mean(abs(test_data$Salary - rf_predictions) / test_data$Salary <= accuracy_threshold) * 100

# Print the accuracy percentage
cat("Accuracy within", accuracy_threshold * 100, "%:", accuracy_percentage, "%\n")
