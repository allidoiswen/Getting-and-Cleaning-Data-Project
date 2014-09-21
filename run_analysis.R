### Getting and Cleaning Course Project
### You should create one R script called run_analysis.R that does the following. 
### 1. Merges the training and the test sets to create one data set.
### 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
### 3. Uses descriptive activity names to name the activities in the data set
### 4. Appropriately labels the data set with descriptive variable names. 
### 5. From the data set in step 4, creates a second, independent tidy data set with the 
###    average of each variable for each activity and each subject.

### Data Source
link = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

### Set Work Directory

setwd("C:/Users/Austin/Google Drive/Education/Getting and Cleaning Data/Data/UCI HAR Dataset/")

### Step 1. Merges the training and the test sets to create one data set.
### Read Data

activity.label = read.table('./activity_labels.txt', header = FALSE)
feature = read.table('./features.txt', header = FALSE)
subject.test = read.table('./test/subject_test.txt', header = FALSE)
subject.train = read.table('./train/subject_train.txt', header = FALSE)
test.x = read.table('./test/x_test.txt', header = FALSE) #Be patient... the files are large.
test.y = read.table('./test/y_test.txt', header = FALSE)
train.x = read.table('./train/x_train.txt', header = FALSE)
train.y = read.table('./train/y_train.txt', header = FALSE)

### Check each files dimensions
dim(activity.label); dim(feature); dim(subject.test); dim(subject.train)
dim(test.x); dim(test.y); dim(train.x); dim(train.y)

### Change Column names
colnames(activity.label) = c("activity.id", "activities")
colnames(subject.test) = "subject.id" ; 
colnames(subject.train) = "subject.id";
colnames(test.x) = feature[, 2]; 
colnames(train.x) = feature[,2];
colnames(test.y) = "activity.id"; 
colnames(train.y) = "activity.id"

### Combine data files
test = cbind(test.y, subject.test, test.x)
train = cbind(train.y, subject.train, train.x)
UCI = rbind(test, train)  
### End of Step 1

### Step 2. Extracts only the measurements on the mean and 
###         standard deviation for each measurement. 

### Pattern Recognition using function grep to identify columes contain mean or std
col.mean = grep("mean+", names(UCI), perl = TRUE, value = FALSE)
col.std = grep("std+", names(UCI), perl = TRUE, value = FALSE)

### Create a subset include columes that contain mean or std
UCI.ms = UCI[,c(1,2, col.mean, col.std)]
### End of Step 2

### Step 3. Uses descriptive activity names to name the activities in the data set
UCI.ms = merge(UCI.ms, activity.label, by = "activity.id", all.x = TRUE)
### End of Step 3

### Step 4. Appropriately labels the data set with descriptive variable names.

### Add Space into the colume names
names(UCI.ms) = gsub("([[:upper:]])", " \\1", names(UCI.ms))
### Changing columns names elemenet by element
names(UCI.ms) = gsub("^(t)", "Time", names(UCI.ms));
names(UCI.ms) = gsub("^(f)", "Frequency", names(UCI.ms));
names(UCI.ms) = gsub("Acc", "Acceleration", names(UCI.ms));
names(UCI.ms) = gsub("Mag", "Magnitude", names(UCI.ms));
names(UCI.ms) = gsub("std", "StdDev", names(UCI.ms));
names(UCI.ms) = gsub("\\()", "", names(UCI.ms));
names(UCI.ms) = gsub("Body Body", "Body", names(UCI.ms))
### End of Step 4


### Step 5. From the data set in step 4, creates a second, independent tidy data set with the 
###    average of each variable for each activity and each subject.

### Remove the activity colume because it's not numeric

UCI.ms = UCI.ms[,1:(length(names(UCI.ms))-1)]

### Split the data frame to one activity with one subject and caluclate mean
tidy.data = aggregate(UCI.ms[,3:length(names(UCI.ms))],
                      by=list(activity.id = UCI.ms$activity.id, subject.id = UCI.ms$subject.id), 
                      FUN = mean)

# !!! Can't get the lapply(split) to work!!! #

### Save tidy.data into a txt file 
write.table(tidy.data, './tidydata.txt',row.names=FALSE ,sep='\t')
### End of Step 5