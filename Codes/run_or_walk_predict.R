##Author: Lavanya Surikapuram
##File Purpose: To predict if a person is running or walking based on Logestic Regression and even evaluate the built model 


####################################Reading the file#############################################################################################
#Reading the file from directory  dataset.csv file location 
data = read.csv("C:/DA/LAV_DA_PROJECT/dataset.csv")


#####################################visualize the patterns######################################################################################

##visualize the patterns of NAs(Check for missing values)
install.packages('mice') 
install.packages('VIM') 
library(mice)
missing_values_check=data
md.pattern(missing_values_check)
library(VIM) #visualize the pattern of NAs
mp <- aggr(missing_values_check, col=c('blue','yellow'),
                    numbers=TRUE, sortVars=TRUE,
                    labels=names(missing_values_check), cex.axis=.7,
                    gap=3, ylab=c("Missing data","Pattern"))
					

#####################################corrgrams###################################################################################################
					
##corrgrams in order to visualize a matrix of correlations					
install.packages('corrgram') 
library(corrgram)
# Corrgram of the entire dataset
corrgram(data, order=NULL, lower.panel=panel.shade, upper.panel=NULL, text.panel=panel.txt,
         main="Corrgram of the data")

		 
#####################################Data Pre-Processing#########################################################################################
		 
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


#####################################Distribution of activity####################################################################################

# Distribution of activity
install.packages('ggplot2')
install.packages('dplyr')
install.packages('data.table')
install.packages('gridExtra')
install.packages('ROCR')
library("ggplot2")
library("dplyr") 
library("data.table")
library("gridExtra")
library("ROCR")
activity_count = group_by(train.data, activity) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
ggplot(activity_count, aes(x = activity, y = count)) +
  geom_bar(stat = "identity", fill = "blue") + 
  xlab("Activity") + 
  ylab("Count") +
  geom_text(aes(label = count), vjust = -0.3, size = 4.5) +
  ggtitle("Distribution of the Activity")


#####################################Logistic regression#########################################################################################

#sinosuidal shaped data- variance decreases towards 0 and 1
#binomial 
library(ggplot2)
ggplot(train.data , aes(x=acceleration_y, y=activity)) + geom_point() + 
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)


#####################################Backward Elimination Logestic Regression model1#############################################################   

##Backward Elimination Logestic Regression model1
model_fit_01 = glm(as.factor(activity) ~ acceleration_x +
                               acceleration_y +
                               acceleration_z + 
                               gyro_x + 
                               gyro_y +
                               gyro_z , family = binomial(), data = train.data)

summary(model_fit_01)
##Backward Elimination Logestic Regression model2
model_fit_02 = glm(as.factor(activity) ~ acceleration_x +
                               acceleration_y +
                               acceleration_z + 
                               gyro_y +
                               gyro_z, family = binomial(), data = train.data)

summary(model_fit_02)


#####################################confusion matrix Backward Elimination####################################################################### 
##confusion matrix
prediction = predict(model_fit_02, test.data)
prediction[prediction <= 0.5] = 0
prediction[prediction > 0.5] = 1
con_matrix_be = table(prediction, test.data[,5])
con_matrix_be
## Accuracy #Backward Elimination 0.8532566  85%
sum(diag(con_matrix_be))/sum(con_matrix_be) #overall accuracy
sum(diag(con_matrix_be))/sum(con_matrix_be) 
1-sum(diag(con_matrix_be))/sum(con_matrix_be) #incorrect classification 


##################################### Step wise backward selections ############################################################################# 

##base model and backward selections 85%
base = glm(as.factor(activity)~ acceleration_x, data= train.data, family = "binomial")
full = model_fit_01
step_backwd_model = step(full, direction="backward", trace=T)
summary(step_backwd_model)


#####################################confusion matrix Step wise backward selections############################################################## 

##confusion matrix
prediction_sbm = predict(step_backwd_model, test.data)
prediction_sbm[prediction_sbm <= 0.5] = 0
prediction_sbm[prediction_sbm > 0.5] = 1
cmatrix_sbm = table(prediction_sbm, test.data[,5])
cmatrix_sbm
## Accuracy #Backward Elimination 0.8532566  85%
sum(diag(cmatrix_sbm))/sum(cmatrix_sbm) #overall accuracy
1-sum(diag(cmatrix_sbm))/sum(cmatrix_sbm) #incorrect classification 


##################################### Step wise forward selections ############################################################################## 

##step wise Forward 85%
step_fwd_model = step(base, scope= list(upper=full, lower=~1), direction="forward", trace=F)
summary(step_fwd_model)


#####################################confusion matrix Step wise forward selections ############################################################## 

##confusion matrix
prediction_sf = predict(step_fwd_model, test.data)
prediction_sf[prediction_sf <= 0.5] = 0
prediction_sf[prediction_sf > 0.5] = 1
cmatrix_sf = table(prediction_sf, test.data[,5])
cmatrix_sf
## Accuracy #Backward Elimination 0.8532566  85%
sum(diag(cmatrix_sf))/sum(cmatrix_sf) #overall accuracy
1-sum(diag(cmatrix_sf))/sum(cmatrix_sf) #incorrect classification


#####################################hist plot for fit Step wise forward selections #############################################################

# Get the fitted values and plot them
fit <- step_fwd_model$fitted
hist(fit)


#####################################Adj R Square for backward elimination, backward selection, forward selection ###############################

##Adj R Square
nullmod = glm(activity ~ 1, family = "binomial")
1-logLik(step_backwd_model)/logLik(nullmod)
1-logLik(step_fwd_model)/logLik(nullmod)
1-logLik(model_fit_02)/logLik(nullmod)


#####################################chi square step wise forward selection #####################################################################

# chi square
r <- (train.data$activity - fit)/(sqrt(fit*(1-fit)))
# Sum of squares of these residuals follows a chi-square
# with 200 - 4 = 196 degrees of freedom
sum(r^2)
1- pchisq(231250869, df=70864)


#####################################Hosmer and Lemeshow goodness of fit (GOF) test step wise forward selection #################################

#Hosmer and Lemeshow goodness of fit (GOF) test
#The null deviance shows how well the response variable is 
#predicted by a model that includes only the intercept (grand mean).
#DF number of observations-1
#The residual deviance shows how well the response variable is 
#predicted by a model that includes both predictor vars (DF declines by 2 more)
#residual deviance for a well-fitting model 
#should be approximately equal to its degrees of freedom
#-----------------------------------
# how well does the model fit the data
install.packages('ResourceSelection')
library(ResourceSelection)
hoslem.test(train.data$activity, fitted(step_fwd_model))


#####################################ROC curve and the AUC  Acuuracy step wise forward selection ################################################

# Get the ROC curve and the AUC  Acuuracy =92%
# Apply the algorithm to the training sample
prediction_training = predict(step_fwd_model,train.data, type = "response")
prediction_training = ifelse(prediction_training > 0.5, 1, 0)
error = mean(prediction_training != train.data$activity)
print(paste('Model Accuracy after a split along the 0.5 probability',1-error))
# Get the ROC curve and the AUC
p = predict(step_fwd_model, train.data, type="response")
pr = prediction(p, train.data$activity)
prf = performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
auc = performance(pr, measure = "auc")
auc = auc@y.values[[1]]
print(paste("General Model Accuracy", auc))


#####################################Overdispersion for step wise forward selection #############################################################

##Overdispersion means that the data show 
# discrepancies between the observed responses yi and their predicted values 
#larger than what the binomial model would predict
#overdispersion is present in a dataset, the estimated standard errors and test statistics 
#the overall goodness-of-fit will be distorted
install.packages('arm')
library(arm)
x=predict(step_fwd_model)
y=resid(step_fwd_model)
binnedplot(x,y)


