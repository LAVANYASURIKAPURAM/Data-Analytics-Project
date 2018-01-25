##Author: Lavanya Surikapuram
##File Purpose: To predict if a person is running or walking based on Decision tree
 
####################################Reading the file#############################################################################################
 
install.packages("caret") 
install.packages("e1071") 
install.packages("ggplot2") 
install.packages("rpart") 
library(e1071)
library(caret)
library("ggplot2")
library("rpart") 

#Reading the file from directory  dataset.csv file location
data = read.csv("C:/DA/LAV_DA_PROJECT/dataset.csv")


#####################################visualize the patterns######################################################################################

##Data Pre-Processing for Exploratory Data Analysis in R
# Change the Date format
data$date = as.Date(data$date, "%Y-%m-%d")
# Extract the Year, Month and Day from the Date
data$Year = as.integer(format(data$date, "%Y"))
data$Month = as.integer(format(data$date, "%m"))
data$Day = as.integer(format(data$date, "%d"))
#Split data to Train data (80%) and Test data(20%)
select.data = sample (1:nrow(data), 0.8*nrow(data))
train.data = data[select.data,]
test.data = data[-select.data,]
dim(test.data)
colnames(train.data)
#Load data to corresponding variables from CSV files
username = train.data$username
activity = train.data$activity
wrist = train.data$wrist
acceleration_x = train.data$acceleration_x
acceleration_y = train.data$acceleration_y
acceleration_z = train.data$acceleration_z
gyro_x = train.data$gyro_x
gyro_y = train.data$gyro_y
gyro_z = train.data$gyro_z
Year = train.data$Year
Month = train.data$Month
Day = train.data$Day
date = train.data$date
time = train.data$time	

#####################################Decision Tree######################################################################################


# Decision Tree
model_tree = train(activity ~ acceleration_x +
                     acceleration_y +
                     acceleration_z + 
                     gyro_x + 
                     gyro_y +
                     gyro_z , method = "rpart", data = train.data)

print(model_tree$finalModel)
plot(model_tree$finalModel, uniform = TRUE, main = "Classification Tree")
text(model_tree$finalModel, use.n = TRUE, all = TRUE, cex = 0.8)

# Testing the model on the testing dataset  92%
prediction = predict(model_tree, test.data)
prediction[prediction <= 0.5] = 0
prediction[prediction > 0.5] = 1
cmatrix = table(prediction, test.data[,5])	 
sum(diag(cmatrix))/sum(cmatrix) #overall accuracy
sum(diag(cmatrix))/sum(cmatrix) 
1-sum(diag(cmatrix))/sum(cmatrix) #incorrect classification 