##Author: Lavanya Surikapuram
##File Purpose: To predict if a person is running or walking based on Hypothesis Testing 

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

#####################################Two Sampled Hypothesis#########################################################################################

## Two Sampled Hypothesis Testing on acceleration_x
install.packages('datasets') 
install.packages('dplyr')
library(datasets)
library(dplyr)
selecn=select(train.data,activity,acceleration_x)
head(selecn)
t.test(acceleration_x ~ activity, data=selecn)


#####################################one Sampled Hypothesis two tailed#########################################################################################
		 
## One sample Hypothesis testing One Sample t-test two tailed
activity_run=filter(selecn, activity == 1)
activity_walk=filter(selecn, activity == 0)
t.test(activity_run, NULL, alternative="two.sided", mu=-0.09, paired=F, conf.level=0.95)
t.test(activity_walk, NULL, alternative="two.sided", mu=-0.11, paired=F, conf.level=0.95)

#####################################one Sampled Hypothesis one tailed#########################################################################################

## One sample Hypothesis testing One Sample t-test one tailed
t.test(activity_run, mu=-0.09, alternative = 'greater')