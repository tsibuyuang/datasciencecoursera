## load dplyr library
library(dplyr)

## clean up environtment
rm(list=ls())

## Section 1: Get the dataset

### 1. setting the source directory for the files
old_wd=getwd()
setwd("./getting-and-cleaning-data/week-4-project")
path <- getwd()

### 2. download the dataset
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, file.path(path, "dataFiles.zip"))

### 3. unzip
unzip(zipfile = "dataFiles.zip")

## Section 2. Read raw data and assignin column names 

### Read in the data from files
#### read features.txt
features     = read.table('./UCI HAR Dataset/features.txt',header=FALSE) 
#### read activity_labels.txt
activityType = read.table('./UCI HAR Dataset/activity_labels.txt',header=FALSE)
#### read subject_train.txt
subjectTrain = read.table('./UCI HAR Dataset/train/subject_train.txt',header=FALSE)
#### read x_train.txt
xTrain       = read.table('./UCI HAR Dataset/train/x_train.txt',header=FALSE)
#### read y_train.txt
yTrain       = read.table('./UCI HAR Dataset/train/y_train.txt',header=FALSE)

### Appropriately labels the data set with descriptive variable names
colnames(activityType)  = c('activityId','activityType')
colnames(subjectTrain)  = "subjectId"
colnames(xTrain)        = features[,2] 
colnames(yTrain)        = "activityId"

### Create the final training set by merging yTrain, subjectTrain, and xTrain
trainingData = bind_cols(yTrain,subjectTrain,xTrain)

### Read in the test data
#### read subject_test.txt
subjectTest = read.table('./UCI HAR Dataset/test/subject_test.txt',header=FALSE)
#### read x_test.txt
xTest       = read.table('./UCI HAR Dataset/test/x_test.txt',header=FALSE)
#### read y_test.txt
yTest       = read.table('./UCI HAR Dataset/test/y_test.txt',header=FALSE)

### Appropriately labels the data set with descriptive variable names
colnames(subjectTest) = "subjectId"
colnames(xTest)       = features[,2]
colnames(yTest)       = "activityId"

### Create the final test set by merging the xTest, yTest and subjectTest data
testData = bind_cols(yTest,subjectTest,xTest);

## Section 3: Merges the training and the test sets to create one data set
tidyData <- 
  ### Combine training and test data   
  bind_rows(trainingData,testData) %>%
  ### Uses descriptive activity names to name the activities in the data set by joining with activityType table
  left_join(activityType, by = c("activityId" = "activityId")) %>%
  ### Extracts only activity type, subject id and the measurements on the mean and standard deviation for each measurement
  select(activityType, subjectId, contains("mean"), contains("std"))

## Section 4: Create a second, independent tidy data set with the average of 
## each variable for each activity and each subject.
tidyData2 <-
  ### read first tidy dataset (tidyData)
  tidyData %>%  
  ### group by activity type and subject
  group_by(activityType, subjectId) %>%
  ### apply average function to every column
  summarise_all(mean)
  
# Section 5: Export the tidyData  
write.table(tidyData2, './tidyData.txt',row.names=FALSE,sep='\t')
# write.table(tidyData, './tidyData.txt',row.names=FALSE,sep='\t')
