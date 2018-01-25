##Author: Lavanya Surikapuram
##File Purpose: To predict if a person is running or walking based on KNN model 

####################################Reading the file#############################################################################################
#Reading the file from directory  dataset.csv file location
ldataset = read.csv("C:/DA/LAV_DA_PROJECT/dataset.csv")
set.seed(123)
library(caTools)

####################################filtered_data_set#############################################################################################
filtered_data_set = ldataset[,-1:-4]
## spliting data to 80 %
split = sample.split(filtered_data_set$activity, split_Ratio = 4/5)
train_pred = subset(filtered_data_set, split == TRUE)
test_rw = subset(filtered_data_set, split == FALSE)


####################################k-Nearest Neighbor Predictions#############################################################################################
#k-Nearest Neighbor Predictions
library(class)
knnpred = knn(train = train_pred[,2:7], test = test_rw[,2:7], cl = train_pred[,1], k = 5)
conmatrix = table(test_rw[,1], knnpred)
sum(diag(conmatrix))/sum(conmatrix) #overall accuracy
1-sum(diag(conmatrix))/sum(conmatrix) #incorrect classification
