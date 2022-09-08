#Installation of used packeages
#install.packages("stringr")
#install.packages("corrplot")
#install.packages("rpart.plot")
#install.packages("rpart")
#install.packages("randomForest")
#install.packages("caret")

#removing all variable from r to free up the space
#rm(list=ls())

#Loading of libraries
library(stringr)
library(corrplot)
library(rpart.plot)
library(rpart)
library(randomForest)
library(caret)

#loading of the csv data in belarus dataframe
belarus = read.csv("/Users/kalpeshdhande/Desktop/DMML DATASETS/Belarus.csv" , head = TRUE)

#Analysing the data and removing NA from data and some irreleavant columns 
summary(belarus)
belarus_new = belarus[,20:29]
belarus_new$feature<-paste(belarus_new$feature_0,belarus_new$feature_1,belarus_new$feature_2,belarus_new$feature_3,belarus_new$feature_4,belarus_new$feature_5,belarus_new$feature_6,belarus_new$feature_7,belarus_new$feature_8,belarus_new$feature_9,sep=",")
belarus$features <- str_count(belarus_new$feature, 'True')
belarus= belarus[,-c(2,16,17,20:29)]

colSums(is.na(belarus))
belarus <- na.omit(belarus)

#Transforming some data to factor level data and taking out the insights from the data 
belarus$manufacturer_name <- as.factor(belarus$manufacturer_name)
belarus$transmission <- as.factor(belarus$transmission)
belarus$color <- as.factor(belarus$color)
belarus$engine_fuel <- as.factor(belarus$engine_fuel)
belarus$engine_has_gas <- as.factor(belarus$engine_has_gas)
belarus$engine_type <- as.factor(belarus$engine_type)
belarus$body_type <- as.factor(belarus$body_type)
belarus$has_warranty <- as.factor(belarus$has_warranty)
belarus$state <- as.factor(belarus$state)
belarus$drivetrain <- as.factor(belarus$drivetrain)
belarus$age <- 2020 -belarus$year_produced

# Taking belarus_int_test as new data frame for converting factor data to integer and 
#finding out the correlation between the alll variables
belarus_int_test = belarus
belarus_int_test$manufacturer_name <- as.integer(belarus_int_test$manufacturer_name)
belarus_int_test$transmission <- as.integer(belarus_int_test$transmission)
belarus_int_test$color <- as.integer(belarus_int_test$color)
belarus_int_test$engine_fuel <- as.integer(belarus_int_test$engine_fuel)
belarus_int_test$engine_has_gas <- as.integer(belarus_int_test$engine_has_gas)
belarus_int_test$engine_type <- as.integer(belarus_int_test$engine_type)
belarus_int_test$body_type <- as.integer(belarus_int_test$body_type)
belarus_int_test$has_warranty <- as.integer(belarus_int_test$has_warranty)
belarus_int_test$state <- as.integer(belarus_int_test$state)
belarus_int_test$drivetrain <- as.integer(belarus_int_test$drivetrain)

#Plotting Histogram for all the variable avilable in dataset
par(mfrow=c(3,6))
hist(belarus_int_test$price_usd, main = "price_usd", xlab = "price_usd", ylab = "Count", col = "blue", )
hist(belarus_int_test$manufacturer_name, main = "manufacturer_name", xlab = "manufacturer_name", ylab = "Count", col = "blue")
hist(belarus_int_test$transmission, main = "transmission", xlab = "transmission", ylab = "Count", col = "blue")
hist(belarus_int_test$color, main = "color", xlab = "color", ylab = "Count", col = "blue")
hist(belarus_int_test$odometer_value, main = "odometer_value", xlab = "odometer_value", ylab = "Count", col = "blue")
hist(belarus_int_test$age, main = "age", xlab = "age", ylab = "Count", col = "blue", )
hist(belarus_int_test$engine_fuel, main = "engine_fuel", xlab = "engine_fuel", ylab = "Count", col = "blue", )
hist(belarus_int_test$engine_has_gas, main = "engine_has_gas", xlab = "engine_has_gas", ylab = "Count", col = "blue", )
hist(belarus_int_test$engine_type, main = "engine_type", xlab = "engine_type", ylab = "Count", col = "blue", )
hist(belarus_int_test$engine_capacity, main = "engine_capacity", xlab = "engine_capacity", ylab = "Count", col = "blue", )
hist(belarus_int_test$body_type, main = "body_type", xlab = "body_type", ylab = "Count", col = "blue", )
hist(belarus_int_test$has_warranty, main = "has_warranty", xlab = "has_warranty", ylab = "Count", col = "blue", )
hist(belarus_int_test$state, main = "state", xlab = "state", ylab = "Count", col = "blue", )
hist(belarus_int_test$drivetrain, main = "drivetrain", xlab = "drivetrain", ylab = "Count", col = "blue", )
hist(belarus_int_test$number_of_photos, main = "number_of_photos", xlab = "number_of_photos", ylab = "Count", col = "blue", )
hist(belarus_int_test$up_counter, main = "up_counter", xlab = "up_counter", ylab = "Count", col = "blue", )
hist(belarus_int_test$duration_listed, main = "duration_listed", xlab = "duration_listed", ylab = "Count", col = "blue", )
hist(belarus_int_test$features, main = "features", xlab = "features", ylab = "Count", col = "blue", )

#Transfoirming some variabels to log function to reduce the skewness in data
belarus$price_usd <- log10(belarus$price_usd+1)
belarus$odometer_value<- log10(belarus$odometer_value+1)

#Transfoirming some variabels to log function to reduce the skewness in data for integer dataset also 
belarus_int_test$price_usd <- log10(belarus_int_test$price_usd+1)
belarus_int_test$odometer_value<- log10(belarus_int_test$odometer_value+1)

#Plotting Histogram for transformed variable  in dataset
par(mfrow=c(1,2))
hist(belarus_int_test$price_usd, main = "price_usd", xlab = "price_usd", ylab = "Count", col = "blue", )
hist(belarus_int_test$odometer_value, main = "odometer_value", xlab = "odometer_value", ylab = "Count", col = "blue")


#plotting box plot for all variables to check outliers in data 
par(mfrow=c(3,4))
boxplot(belarus$price_usd, main = "price_usd", xlab = "price_usd")
boxplot(belarus$transmission, main = "transmission", xlab = "transmission")
boxplot(belarus$odometer_value, main = "odometer_value", xlab = "odometer_value")
boxplot(belarus$engine_type, main = "engine_type", xlab = "engine_type")
boxplot(belarus$engine_capacity, main = "engine_capacity", xlab = "engine_capacity")
boxplot(belarus$body_type, main = "body_type", xlab = "body_type")
boxplot(belarus$has_warranty, main = "has_warranty", xlab = "has_warranty")
boxplot(belarus$drivetrain, main = "drivetrain", xlab = "drivetrain")
boxplot(belarus$number_of_photos, main = "number_of_photos", xlab = "number_of_photos")
boxplot(belarus$features, main = "features", xlab = "features")
boxplot(belarus$age, main = "age", xlab = "age")

#Applying the outlier removing on datset
belarus = subset(belarus, price_usd>2.3 & odometer_value>4 & engine_capacity >1 & engine_capacity < 4 & age < 50)
belarus_int_test = subset(belarus_int_test, price_usd>2.3 & odometer_value>4 & engine_capacity >1 & engine_capacity < 4 & age < 50)

#Plottig graph again for checking how many outliers still present 
boxplot(belarus$price_usd, main = "price_usd", xlab = "price_usd")
boxplot(belarus$odometer_value, main = "odometer_value", xlab = "odometer_value")
boxplot(belarus$engine_capacity, main = "engine_capacity", xlab = "engine_capacity")
boxplot(belarus$age, main = "age", xlab = "age")

#Finding out the correlation between all variable and removing independent variable
#whose correlation with each other is more than 0.8
cor(belarus_int_test)
belarus= belarus[,-c(1,3,5,6,7,12,16,17)]


#Dividing the data in test data and train data
set.seed(100)
individual_sample <- sample(2, nrow(belarus), replace=TRUE, prob=c(0.80, 0.20))
train_data <- belarus[individual_sample==1,]
test_data <- belarus[individual_sample==2,]

#Applying regression tree on dataset using the rpart 
reg_tree <-rpart(price_usd~., data=train_data ,cp = 0.0001)
par(mfrow=c(1,1))
plotcp(reg_tree)

#Prunning the regression the on the basis of observarion cp plot of regtree
prune_reg<- prune(reg_tree, cp=0.0005)

#Forming tree prediction using the test data and finding out the 
#RMSE-Root Mean Square Error and R square value
tree_prediction <- predict(prune_reg,newdata = test_data)
sum_square_error <- sum((test_data[,"price_usd"]-tree_prediction)^2)
max_sum_of_square <- sum((test_data[,"price_usd"] - mean(test_data[,"price_usd"]))^2)
R_square <- 1 - sum_square_error/max_sum_of_square
data.frame(
  Rsquare = R_square,
  RMSE = RMSE(tree_prediction, test_data$price_usd)
)
R_square

#Applying regression tree on dataset using the randomForest library 
random_forest <- randomForest(price_usd~.,data = train_data,ntree=500)

#Finding out the importance of variables and plotting it
importance(random_forest)
varImpPlot(random_forest)
random_forest
plot(random_forest)

#Forming random forest prediction using the test data and finding out the 
#RMSE-Root Mean Square Error and R square value
random_forest_pred <- predict(random_forest,newdata = test_data)
sum_square_error_rf <- sum((test_data[,"price_usd"]-random_forest_pred)^2)
max_sum_of_square_rf <- sum((test_data[,"price_usd"] - mean(test_data[,"price_usd"]))^2)
R_square_rf <- 1 - sum_square_error_rf/max_sum_of_square_rf
data.frame(
  Rsquare = R_square_rf,
  RMSE = RMSE(random_forest_pred, test_data$price_usd)
)




