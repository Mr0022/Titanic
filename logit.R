# Load necessary library
library(readr)
library(dplyr)
# Read the CSV file
data = read_csv("train.csv")
data
data= subset(data, select = -c(PassengerId , Name ,Parch ,Ticket ,Cabin , Embarked,Fare, SibSp ))
data
data= data[, c(2:ncol(data), 1)]
data
# Assume you have a dataframe called df
# Remove rows with NA values
df_clean = na.omit(data)
df_clean
# Group the dataset by 'Sex' and 'Pclass', then calculate the average age, total passengers (n), and 
#survivors (y)
grouped_data <- df_clean %>%
  group_by(Sex, Pclass) %>%
  summarise(
    AvgAge = mean(Age), # Calculate the average age for each group
    n = n(), # Total number of passengers (n)
    y = sum(Survived) # Total number of survivors (y)
  )
doze = function() {
  # Load necessary libraries
  library(dplyr)
  
  # Load the Titanic dataset 
  titanic = read.csv("train.csv")
  
  # Remove rows with NA values in 'Age' to calculate average age correctly
  df_clean = na.omit(titanic)
  
  # Group the dataset by 'Sex' and 'Pclass', then calculate the average age, total passengers (n), and survivors (y)
  grouped_data <- df_clean %>%
    group_by(Sex, Pclass) %>%
    summarise(
      AvgAge = mean(Age, na.rm = TRUE), # Calculate the average age for each group
      n = n(), # Total number of passengers (n)
      y = sum(Survived) # Total number of survivors (y)
    ) %>%
    ungroup()

  
  # Create feature matrix x (Gender, Class, and AvgAge as features)
  # Gender: Convert to numeric (female = 0, male = 1)
  # Class: Already numeric, use as is
  # AvgAge: Already numeric, use as is
  x0 = rep(1, nrow(grouped_data)) # Intercept term
  x1 = as.numeric(grouped_data$Sex == "male") # Gender feature (male = 1, female = 0)
  x2 = as.numeric(grouped_data$Pclass) # Class feature (1, 2, 3)
  x3 = grouped_data$AvgAge # Average Age feature
  
  
  # Create matrix x (intercept, gender, class, avg_age)
  x = matrix(c(x0, x1, x2, x3), ncol = 4)
  
  # Get response variable (y = number of survivors) and n (total passengers)
  y = grouped_data$y
  n = grouped_data$n
  
  # Initialize variables
  b = matrix(c(rep(0, 200)), ncol = 50)
  z = matrix(c(rep(0, 300)), ncol = 50)
  yhat = matrix(c(rep(0, 300)), ncol = 50)
  syhat = matrix(c(rep(0, 200)), ncol = 50)
  kai2 = c(rep(0, 50))
  e = matrix(c(rep(0, 200)), ncol = 50)
  e[, 1] = c(0, 0, 0, 0)
  sy = c(0, 0, 0, 0)
  yhat[, 1] = y
  p = matrix(c(rep(0, 300)), ncol = 50)
  p[, 1] = (y + 0.5) / (n + 0.5)
  
  # Iterative process for logistic regression
  for (i in 1:49) {
    z[, i] = log(p[, i] / (1 - p[, i])) + (y - n * p[, i]) / (n * p[, i] * (1 - p[, i]))
    b[, i + 1] = solve(t(x) %*% diag(n * p[, i] * (1 - p[, i])) %*% x) %*%
      (t(x) %*% diag(n * p[, i] * (1 - p[, i])) %*% z[, i])
    p[, i + 1] = (exp(x %*% b[, i + 1])) / (1 + exp(x %*% b[, i + 1]))
    yhat[, i + 1] = n * p[, i + 1]
    syhat = yhat[, i + 1] %*% x
    sy = t(y) %*% x
    e[, i + 1] = syhat - sy
    kai2[i + 1] = sum(((y - yhat[, i + 1]) * (y - yhat[, i + 1])) / yhat[, i + 1])
    7
    
    # Plot the predicted probabilities for each class (just plotting Class here for example)
    plot(x2, p[, i + 1], main = paste("Iteration", i), xlab = "Passenger Class", ylab = "Predicted Probability", col = "blue", 
         pch = 19)
  }
  
  return(b)
}

 doze()
