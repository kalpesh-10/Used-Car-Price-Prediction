#Installation of used packeages
#install.packages("stringr")
#install.packages("corrplot")
#install.packages("glmnet")

#removing all variable from r to free up the space
#rm(list=ls())

#Loading of libraries
library(stringr)
library(corrplot)
library(mlbench)
library(psych)
library(glmnet)

#loading of the csv data in poland dataframe
poland = read.csv("/Users/kalpeshdhande/Desktop/DMML DATASETS/Polandcars.csv" , head = TRUE)

#Analysing the data and removing NA from data and some irreleavant columns
poland[poland == ""] <- NA
poland$Features[poland$Features == '[]'] <- NA
summary(poland)
colSums(is.na(poland))

poland= poland[,-c(1,3,6,7,8,14,15,20,21,22,23,24)]
colSums(is.na(poland))

#Filling some NA s withe taking out the mean of the varibles from that column
poland$Mileage_km[is.na(poland$Mileage_km)] <- mean(poland$Mileage_km,na.rm = TRUE)
poland$Power_HP[is.na(poland$Power_HP)] <- mean(poland$Power_HP,na.rm = TRUE)
poland$Displacement_cm3[is.na(poland$Displacement_cm3)] <- mean(poland$Displacement_cm3,na.rm = TRUE)
poland$Doors_number[is.na(poland$Doors_number)] <- round(mean(poland$Doors_number,na.rm = TRUE))


colSums(is.na(poland))
poland <- na.omit(poland)

#Transforming some data to factor level data and taking out the insights from the data 
poland$Features <- str_count(poland$Features, ',') +1
poland$Vehicle_brand <- as.factor(poland$Vehicle_brand)
poland$Condition <- as.factor(poland$Condition)
poland$Fuel_type <- as.factor(poland$Fuel_type)
poland$Transmission <- as.factor(poland$Transmission)
poland$Type <- as.factor(poland$Type)
poland$Colour <- as.factor(poland$Colour)
poland$Mileage_km <- as.integer(poland$Mileage_km)
poland$Power_HP <- as.integer(poland$Power_HP)
poland$Displacement_cm3 <- as.integer(poland$Displacement_cm3)
poland$age <- 2021 - poland$Production_year
poland= poland[,-4]

# Taking poland_int_test as new data frame for converting factor data to integer and 
#finding out the correlation between the alll variables
poland_int_test=poland
poland_int_test$Vehicle_brand <- as.numeric(poland_int_test$Vehicle_brand)
poland_int_test$Condition <- as.numeric(poland_int_test$Condition)
poland_int_test$Fuel_type <- as.numeric(poland_int_test$Fuel_type)
poland_int_test$Transmission <- as.numeric(poland_int_test$Transmission)
poland_int_test$Type <- as.numeric(poland_int_test$Type)
poland_int_test$Colour <- as.numeric(poland_int_test$Colour)


#Plotting Histogram for all the variable avilable in dataset
par(mfrow=c(3,5))
hist(poland_int_test$Price, main = "price", xlab = "price", ylab = "Count", col = "green", )
hist(poland_int_test$Condition, main = "condition", xlab = "condition", ylab = "Count", col = "green")
hist(poland_int_test$Vehicle_brand, main = "vehicle_brand", xlab = "vehicle_brand", ylab = "Count", col = "green")
hist(poland_int_test$age, main = "age", xlab = "age", ylab = "Count", col = "green")
hist(poland_int_test$Mileage_km, main = "Mileage_km", xlab = "Mileage_km", ylab = "Count", col = "green")
hist(poland_int_test$Power_HP, main = "Power_HP", xlab = "Power_HP", ylab = "Count", col = "green", )
hist(poland_int_test$Displacement_cm3, main = "Displacement_cm3", xlab = "Displacement_cm3", ylab = "Count", col = "green", )
hist(poland_int_test$Fuel_type, main = "Fuel_type", xlab = "Fuel_type", ylab = "Count", col = "green", )
hist(poland_int_test$Transmission, main = "Transmission", xlab = "Transmission", ylab = "Count", col = "green", )
hist(poland_int_test$Type, main = "Type", xlab = "Type", ylab = "Count", col = "green", )
hist(poland_int_test$Doors_number, main = "Doors_number", xlab = "Doors_number", ylab = "Count", col = "green", )
hist(poland_int_test$Colour, main = "Colour", xlab = "Colour", ylab = "Count", col = "green", )
hist(poland_int_test$Features, main = "Features", xlab = "Features", ylab = "Count", col = "green", )

#Transfoirming some variabels to log function to reduce the skewness in data
poland$Price <- log10(poland$Price)
poland$age <- log10(poland$age +1)
poland$Mileage_km <- log10(poland$Mileage_km)
poland$Power_HP <- sqrt(poland$Power_HP)
poland$Displacement_cm3 <- log10(poland$Displacement_cm3)

#Transfoirming some variabels to log function to reduce the skewness in data for integer dataset also
poland_int_test$Price <- log10(poland_int_test$Price)
poland_int_test$age <- log10(poland_int_test$age +1)
poland_int_test$Mileage_km <- log10(poland_int_test$Mileage_km)
poland_int_test$Power_HP <- sqrt(poland_int_test$Power_HP)
poland_int_test$Displacement_cm3 <- log10(poland_int_test$Displacement_cm3)

#Plotting Histogram for transformed variable  in dataset
par(mfrow=c(2,3))
hist(poland_int_test$Price, main = "price", xlab = "price", ylab = "Count", col = "green", )
hist(poland_int_test$age, main = "age", xlab = "age", ylab = "Count", col = "green")
hist(poland_int_test$Mileage_km, main = "Mileage_km", xlab = "Mileage_km", ylab = "Count", col = "green")
hist(poland_int_test$Power_HP, main = "Power_HP", xlab = "Power_HP", ylab = "Count", col = "green", )
hist(poland_int_test$Displacement_cm3, main = "Displacement_cm3", xlab = "Displacement_cm3", ylab = "Count", col = "green", )

#plotting box plot for all variables to check outliers in data 
par(mfrow=c(3,5))
boxplot(poland$Price, main = "Price", xlab = "Price")
boxplot(poland$Mileage_km, main = "Mileage_km", xlab = "Mileage_km")
boxplot(poland$Displacement_cm3, main = "Displacement_cm3", xlab = "Displacement_cm3")
boxplot(poland$Condition, main = "Condition", xlab = "Condition")
boxplot(poland$Vehicle_brand, main = "Vehicle_brand", xlab = "Vehicle_brand")
boxplot(poland$age, main = "age", xlab = "age")
boxplot(poland$Power_HP, main = "Power_HP", xlab = "Power_HP")
boxplot(poland$Fuel_type, main = "Fuel_type", xlab = "Fuel_type")
boxplot(poland$Transmission, main = "Transmission", xlab = "Transmission")
boxplot(poland$Type, main = "Type", xlab = "Type")
boxplot(poland$Doors_number, main = "Doors_number", xlab = "Doors_number")
boxplot(poland$Colour, main = "Colour", xlab = "Colour")
boxplot(poland$Features, main = "Features", xlab = "Features")

#Applying the outlier removing on datset
poland = subset(poland, Price>3.2 & Price< 6 & Power_HP>5 & Power_HP<17 )
poland_int_test = subset(poland_int_test, Price>3.2 & Price< 6 & Power_HP>5 & Power_HP<17 )

#Plottig graph again for checking how many outliers still present 
par(mfrow=c(1,2))
boxplot(poland$Price, main = "Price", xlab = "Price")
boxplot(poland$Power_HP, main = "Power_HP", xlab = "Power_HP")

#Dividing the data in test data and train data
set.seed(123)
individual_sample <- sample(2, nrow(poland), replace=TRUE, prob=c(0.80, 0.20))
train_data <- poland[individual_sample==1,]
test_data <- poland[individual_sample==2,]

# creating the x and y variable form the given dataset as x is all indepednt variable
# and y are all dependent variabel
x_train_data<- model.matrix(Price~., data = train_data)[,-1]
y_train_data <- train_data$Price

x_test_data <- model.matrix(Price~., data= test_data)[,-1]
y_test_data<- test_data$Price


#creating grid of lambdas for hyper tunning

grid_lambdas <- 10^seq(2, -3, length =100)

#building ridge regression model using glmnet library 
lm_ridge_reg = glmnet(x_train_data, y_train_data, alpha = 0, family = 'gaussian', lambda = grid_lambdas)

#finding out the optimum lambda for the ridge regression
cv_ridge_reg <- cv.glmnet(x_train_data, y_train_data, alpha = 0, lambda = grid_lambdas)
optimal_lambda <- cv_ridge_reg$lambda.min
optimal_lambda
par(mfrow=c(1,1))
plot(cv_ridge_reg)
plot(lm_ridge_reg)

# finding out the RMSE-Root Mean Square Error and R square value
eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(df))
  
  data.frame(
    RMSE = RMSE,
    Rsquare = R_square
  )
  
}

predictions_train <- predict(lm_ridge_reg, s = optimal_lambda, newx = x_train_data)
predictions_test <- predict(lm_ridge_reg, s = optimal_lambda, newx = x_test_data)
eval_results(y_train_data, predictions_train, train_data)
eval_results(y_test_data, predictions_test, test_data)


#building lasso regression model using glmnet library 
lm_lasso_reg <- glmnet(x_train_data, y_train_data, alpha = 1, lambda = grid_lambdas)

#finding out the lambda best for the ridge regression
cv_fit_lasso_reg <- cv.glmnet(x_train_data, y_train_data, alpha = 1, lambda = grid_lambdas)
lambda_best <- cv_fit_lasso_reg$lambda.min 
lambda_best


plot(cv_fit_lasso_reg)

predictions_train <- predict(lm_lasso_reg, s = lambda_best, newx = x_train_data)
predictions_test <- predict(lm_lasso_reg, s = lambda_best, newx = x_test_data)
eval_results(y_train_data, predictions_train, train_data)
eval_results(y_test_data, predictions_test, test_data)

