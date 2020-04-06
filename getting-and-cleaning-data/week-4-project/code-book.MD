---
title: "code book"
author: "Gema Fatra"
date: "4/6/2020"
output:
  word_document: default
  html_document: default
---

## Getting and Cleaning Data Project

### Description
Additional information about the variables, data and transformations used in the course project for the Johns Hopkins Getting and Cleaning Data course.

### Source Data
A full description of the data used in this project can be found at [The UCI Machine Learning Repository](http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones)

[The source data for this project can be found here.](https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip)

### Data Set Information
The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.

### Attribute Information
For each record in the dataset it is provided: 
- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration. 
- Triaxial Angular velocity from the gyroscope. 
- A 561-feature vector with time and frequency domain variables. 
- Its activity label. 
- An identifier of the subject who carried out the experiment.

### Section 1: Get the dataset
1. setting the source directory for the files 
2. download the dataset 
3. unzip


### Section 2. Read raw data and assignin column names 
After download and unzip the source dataset, read into tables the data located in
- features.txt
- activity_labels.txt
- subject_train.txt
- x_train.txt
- y_train.txt
- subject_test.txt
- x_test.txt
- y_test.txt

Appropriately labels the data set with descriptive variable names


### Section 3: Merges the training and the test sets to create one data set
1. Combine training and test data
2. Uses descriptive activity names to name the activities in the data set by joining with activityType table
3. extracts only activity type, subject id and the measurements on the mean and standard deviation for each measurement.
4. assing to first tidy dataset variables called: tidyData

### Section 4. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 
Per the project instructions, we need to produce a data set with the average of each variable for each activity and subject.
1. read first tidy dataset (tidyData)
2. group by activity type and subject
3. apply average function to every column
4. assign to second tidy dataset variable called: tidyData2

### Section 5: Export the tidyData 
write the tidy data set into files