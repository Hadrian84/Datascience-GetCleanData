## Getting and Cleaning Data Course Project
## Hadrian84

############
## STEP 0 ##
############

## check whether required packages are installed
## download file from the internet and unzip dataset

required.packages = c("reshape2")
new.packages = required.packages[!(required.packages %in% installed.packages()[, "Package"])]
if(length(new.packages)) install.packages(new.packages)
library(reshape2)

if(!file.exists("UCI HAR Dataset.zip")) {
        url = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
        download.file(url, "UCI HAR Dataset.zip")
}
if(!file.exists("UCI HAR Dataset")) unzip("UCI HAR Dataset.zip")

## obtain features
## read train data
## read test data
## obtain labels

features = read.table("./UCI HAR Dataset/features.txt", header = FALSE)

train_subjects = read.table("./UCI HAR Dataset/train/subject_train.txt", header = FALSE)
train_X = read.table("./UCI HAR Dataset/train/X_train.txt", header = FALSE)
train_Y = read.table("./UCI HAR Dataset/train/y_train.txt", header = FALSE)

test_subjects = read.table("./UCI HAR Dataset/test/subject_test.txt", header = FALSE)
test_X = read.table("./UCI HAR Dataset/test/X_test.txt", header = FALSE)
test_Y = read.table("./UCI HAR Dataset/test/y_test.txt", header = FALSE)

labels = read.table("./UCI HAR Dataset/activity_labels.txt")
names(labels) = c("code", "description")
labels$description = as.character(labels$description)

############
## STEP 1 ##
############

## merge train and test data in one dataset
## provide headers for datasets

subjects = rbind(train_subjects, test_subjects); names(subjects) = "subject"
Xvals = rbind(train_X, test_X); names(Xvals) = features[,2]
Yvals = rbind(train_Y, test_Y); names(Yvals) = "activity"
rm(train_X, test_X, train_Y, test_Y, train_subjects, test_subjects)

complete_data = data.frame(subjects, Yvals, Xvals)
rm(subjects, Yvals, Xvals)

##############
## STEP 2/3 ##
##############

## from Xvals: only take variables with mean and std data AND subject and activity
## adapt column names

index = grep("subject|activity|mean\\.|std", x = names(complete_data))
complete_data = complete_data[,index]

names(complete_data) = gsub("\\.", "", names(complete_data))
names(complete_data) = gsub("mean", "Mean", names(complete_data))
names(complete_data) = gsub("std", "Std", names(complete_data))

############
## STEP 4 ##
############

## appropriately labels with descriptive variable names
for(ii in 1:6) {
        ww = which(complete_data$activity == ii)
        complete_data$activity[ww] = tolower(labels$description[ii])
}


############
## STEP 5 ##
############

## create second tidy data set 
## with the average of each variable for each activity and each subject

temp = melt(complete_data, c("activity", "subject"))
tidy_data = dcast(temp, formula = activity + subject ~ variable, mean)

## write tidy_data into a txt file
write.table(tidy_data, "tidy_data.txt", row.names = FALSE, quote = FALSE, sep = "\t")
