#Installation of used packeages
#install.packages("stringr")
#install.packages("corrplot")
#install.packages("car")

#removing all variable from r to free up the space
#rm(list=ls()) 

#Loading of libraries
library(stringr)
library(corrplot)
library(car)
library(caret)


#loading of the csv data in pak dataframe
pak = read.csv("/Users/kalpeshdhande/Desktop/Masters/DM&ML/Project/DMML DATASETS 2/pakwheels.csv", head = TRUE)

#Analysing the data and removing NA from data and some irreleavant columns
pak[pak == ""] <- NA
pak[pak == "Call for price" ] <- NA

summary(pak)
sum((is.na(pak)))
colSums(is.na(pak))
pak= pak[,-c(1,5,7,15,16)]
cleanpak <- na.omit(pak)
table(is.na(cleanpak))
summary(cleanpak)

#Transforming some data to factor level data and taking out the insights from the data 
cleanpak$Manufacturer=sapply(strsplit(cleanpak$Name,split = " "), `[`, 1)
cleanpak$age_car = 2020 - cleanpak$Model.Year
cleanpak=cleanpak[, -c(1,3)]
cleanpak$Price <- as.integer(cleanpak$Price)
cleanpak$Engine.Type <- as.factor(cleanpak$Engine.Type)
cleanpak$Engine.Capacity <-as.integer(sapply(strsplit(cleanpak$Engine.Capacity,split = " "), `[`, 1))
cleanpak$Transmission <-as.factor(cleanpak$Transmission)
cleanpak$Color <- as.factor(cleanpak$Color)
cleanpak$Assembly <- as.factor(cleanpak$Assembly)
cleanpak$Body.Type <- as.factor(cleanpak$Body.Type)
cleanpak$Manufacturer <- as.factor(cleanpak$Manufacturer)
cleanpak$Features <- str_count(cleanpak$Features, ',') +1

summary(cleanpak)

# Taking testPakInt as new data frame for converting factor data to integer and 
#finding out the correlation between the alll variables
testPakInt <-cleanpak
testPakInt$Engine.Type <-as.numeric(testPakInt$Engine.Type)
testPakInt$Transmission <-as.numeric(testPakInt$Transmission)
testPakInt$Color <- as.numeric(testPakInt$Color)
testPakInt$Assembly <- as.numeric(testPakInt$Assembly)
testPakInt$Body.Type <- as.numeric(testPakInt$Body.Type)
testPakInt$Manufacturer <- as.numeric(testPakInt$Manufacturer)



#Plotting Histogram for all the variable avilable in dataset
par(mfrow=c(3,4))
hist(testPakInt$Price, main = "Price", xlab = "Price", ylab = "Count", col = "green", )
hist(testPakInt$Mileage, main = "Mileage", xlab = "Mileage", ylab = "Count", col = "green")
hist(testPakInt$Engine.Type, main = "Engine.Type", xlab = "Engine.Type", ylab = "Count", col = "green")
hist(testPakInt$Engine.Capacity, main = "Engine.Capacity", xlab = "Engine.Capacity", ylab = "Count", col = "green")
hist(testPakInt$Transmission, main = "Transmission", xlab = "Transmission", ylab = "Count", col = "green", )
hist(testPakInt$Color, main = "Color", xlab = "Color", ylab = "Count", col = "green", )
hist(testPakInt$Assembly, main = "Assembly", xlab = "Assembly", ylab = "Count", col = "green", )
hist(testPakInt$Body.Type, main = "Body.Type", xlab = "Body.Type", ylab = "Count", col = "green", )
hist(testPakInt$Features, main = "Features", xlab = "Features", ylab = "Count", col = "green", )
hist(testPakInt$Manufacturer, main = "Manufacturer", xlab = "Manufacturer", ylab = "Count", col = "green", )
hist(testPakInt$age_car, main = "age_car", xlab = "age_car", ylab = "Count", col = "green", )

#Transfoirming some variabels to log function to reduce the skewness in data
cleanpak$Price <- log10(cleanpak$Price)
cleanpak$Engine.Capacity <- log10(cleanpak$Engine.Capacity)
cleanpak$Mileage <- log10(cleanpak$Mileage)


#Transfoirming some variabels to log function to reduce the skewness in data for integer dataset also 
testPakInt$Price <- log10(testPakInt$Price)
testPakInt$Engine.Capacity <- log10(testPakInt$Engine.Capacity)
testPakInt$Mileage <- log10(testPakInt$Mileage)


#Plotting Histogram for transformed variable  in dataset
par(mfrow=c(2,2))
hist(testPakInt$Price, main = "Price", xlab = "Price", ylab = "Count", col = "green", )
hist(testPakInt$Engine.Capacity, main = "Engine.Capacity", xlab = "Engine.Capacity", ylab = "Count", col = "green")
hist(testPakInt$Mileage, main = "Mileage", xlab = "Mileage", ylab = "Count", col = "green")


#Plotting graph of independent variable vs dependent variable from dataset to check 
#linearity between the variabels
par(mfrow=c(3,4))
plot(x=testPakInt$Mileage, y=cleanpak$Price, xlab="Mileage", ylab="price")
plot(x=testPakInt$Engine.Type, y=cleanpak$Price, xlab="Engine.Type", ylab="price")
plot(x=testPakInt$Engine.Capacity, y=cleanpak$Price, xlab="Engine.Capacity", ylab="price")
plot(x=testPakInt$Transmission, y=cleanpak$Price, xlab="Transmission", ylab="price")
plot(x=testPakInt$Color, y=cleanpak$Price, xlab="Color", ylab="price")
plot(x=testPakInt$Assembly, y=cleanpak$Price, xlab="Assembly", ylab="price")
plot(x=testPakInt$Body.Type, y=cleanpak$Price, xlab="Body.Type", ylab="price")
plot(x=testPakInt$Features, y=cleanpak$Price, xlab="Features", ylab="price")
plot(x=testPakInt$Manufacturer, y=cleanpak$Price, xlab="Manufacturer", ylab="price")
plot(x=testPakInt$age_car, y=cleanpak$Price, xlab="age_car", ylab="price")



#plotting box plot for all variables to check outliers in data 
par(mfrow=c(3,4))
boxplot(cleanpak$Price, main = "Price", xlab = "Price")
boxplot(cleanpak$Mileage, main = "Mileage", xlab = "Mileage")
boxplot(cleanpak$Engine.Type, main = "Engine.Type", xlab = "Engine.Type")
boxplot(cleanpak$Engine.Capacity, main = "Engine.Capacity", xlab = "Engine.Capacity")
boxplot(cleanpak$Transmission, main = "Transmission", xlab = "Transmission")
boxplot(cleanpak$Color, main = "Color", xlab = "Color")
boxplot(cleanpak$Assembly, main = "Assembly", xlab = "Assembly")
boxplot(cleanpak$Body.Type, main = "Body.Type", xlab = "Body.Type")
boxplot(cleanpak$Features, main = "Features", xlab = "Features")
boxplot(cleanpak$Manufacturer, main = "Manufacturer", xlab = "Manufacturer")
boxplot(cleanpak$age_car, main = "age_car", xlab = "age_car")

#Applying the outlier removing on datset
cleanpak = subset(cleanpak, Price>5.5 & Price< 7 & Mileage > 4.2 & Mileage < 5.7 & Engine.Capacity > 2.8 & Engine.Capacity < 3.4 )
testPakInt = subset(testPakInt, Price>5.5 & Price< 7 & Mileage > 4.2 & Mileage < 5.7 & Engine.Capacity > 2.8 & Engine.Capacity < 3.4 )

#Plottig graph again for checking how many outliers still present 
par(mfrow=c(2,2))
boxplot(cleanpak$Price, main = "Price", xlab = "Price")
boxplot(cleanpak$Mileage, main = "Mileage", xlab = "Mileage")
boxplot(cleanpak$Engine.Capacity, main = "Engine.Capacity", xlab = "Engine.Capacity")


#Finding out the correlation between all variable and removing independent variable
#whose correlation with each other is more than 0.8
cor(testPakInt)
cleanpak= cleanpak[, -c(3,6,8,10)]

#Dividing the data in test data and train data
set.seed(1234)
individual_sample <- sample(2, nrow(cleanpak), replace=TRUE, prob=c(0.80, 0.20))
training_data <- cleanpak[individual_sample==1,]
testing_data <- cleanpak[individual_sample==2,]

#Applying multiple linear regression on the dataset 
model <- glm(Price~., data=training_data)
model
summary(model)
par(mfrow=c(2,2))
plot(model)

#Finding out the predictios of the testing dataset
predictions <- model %>% predict(testing_data)

#Finding out RMSE-Root Mean Square Error and RSE - Residual Standard Error - Prediction Error Rate
sigma(model)/mean(testing_data$Price)
RMSE(predictions, testing_data$Price)

#finding out the durbin watson test and vif for checking the assumption of multiple linear regression
durbinWatsonTest(model)
vif(model)

