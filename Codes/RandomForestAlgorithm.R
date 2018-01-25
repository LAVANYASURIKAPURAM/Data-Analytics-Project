##Author: Lavanya Surikapuram
##File Purpose: To predict if a person is running or walking based on Random Forest 

####################################Reading the file#############################################################################################
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


#####################################RandomForest Algorithm######################################################################################

#RandomForest Algorithm
install.packages("randomForest")
library(randomForest)
set.seed(415)
fitr = randomForest(as.factor(activity) ~ acceleration_x +
                               acceleration_y +
                               acceleration_z + 
                               gyro_x + 
                               gyro_y +
                               gyro_z , data = train.data, importance = TRUE, ntree=501)


install.packages("caret")
library(caret)
plot(fitr)
response = predict(fitr, test.data)
response
install.packages("e1071")
library(e1071)
library(caret)
confusionMatrix(data=response, reference=test.data$activity)
print(fitr)

#####################################ROC curve and the AUC  Acuuracy Random forest ################################################


##confusion matrix  # 99%
prediction = predict(fitr, test.data)
prediction[prediction <= 0.5] = 0
prediction[prediction > 0.5] = 1
cmatrix = table(prediction, test.data[,5])	 
sum(diag(cmatrix))/sum(cmatrix) #overall accuracy
sum(diag(cmatrix))/sum(cmatrix) 
1-sum(diag(cmatrix))/sum(cmatrix) #incorrect classification 

#####################################ROC curve and the AUC  Acuuracy Random forest  ################################################

# Get the ROC curve and the AUC  
# Apply the algorithm to the training sample
prediction_training = predict(fitr,train.data, type = "response")
prediction_training = ifelse(prediction_training > 0.5, 1, 0)
error = mean(prediction_training != train.data$activity)
print(paste('Model Accuracy after a split along the 0.5 probability',1-error))
# Get the ROC curve and the AUC
p = predict(fitr, train.data, type="response")
pr = prediction(p, train.data$activity)
prf = performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
auc = performance(pr, measure = "auc")
auc = auc@y.values[[1]]
print(paste("General Model Accuracy", auc))