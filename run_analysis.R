#Load libraries needed for the script
library(RCurl)
library(data.table)
library(dplyr)
library(reshape2)

#Download the file and ectract to the temp directory
tmpdir <- tempdir()
url <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
file <- basename(url)
download.file(url, file, method = "curl")
unzip(file, exdir = tmpdir)

#Directory name
dir_name <- paste0(tmpdir,"/UCI HAR Dataset")

#Load the data sets into variables
features <- read.csv(paste0(dir_name,"/features.txt"), sep=" ", header = FALSE, stringsAsFactors = FALSE)
test_set <- data.table(read.table(paste0(dir_name,"/test/X_test.txt")))
test_labels <- data.table(read.table(paste0(dir_name,"/test/y_test.txt")))
test_subjects <- data.table(read.table(paste0(dir_name,"/test/subject_test.txt")))
train_set <- data.table(read.table(paste0(dir_name,"/train/X_train.txt")))
train_labels <- data.table(read.table(paste0(dir_name,"/train/y_train.txt")))
train_subjects <- data.table(read.table(paste0(dir_name,"/train/subject_train.txt")))
activity_labels <- read.csv(paste0(dir_name,"/activity_labels.txt"), sep = " ", header = FALSE, stringsAsFactors = FALSE)

#Use the features data sets and set them as the names of the columns for the test and train sets
features <- data.frame(t(features$V2))
features <- sapply(features, as.character)
setnames(test_set, features)
setnames(train_set, features)

#Select the mean and standard deviation measurement features and isolate them in the test and train sets
select_features <- features[grep((".*mean.*|.*std.*"), features)]
test_set <- subset(test_set, select = select_features)
train_set <- subset(train_set, select = select_features)

#Add subjects columns to the train and test sets
test_set <- cbind(subject = test_subjects$V1, test_set)
train_set <- cbind(subject = train_subjects$V1, train_set)

#Add activities columns to the train and test sets
test_set <- cbind(activity = test_labels$V1, test_set)
train_set <- cbind(activity = train_labels$V1, train_set)

#Create the combined sets of the test + train sets
combined_set <- rbind(test_set, train_set)

#Fix the names of the columns by renaming the mean and std and removing the '[-()]' and then restoring the column names
fixed_names <- colnames(combined_set)
fixed_names <- gsub('-mean', 'Mean', fixed_names)
fixed_names <- gsub('-std', 'Std', fixed_names)
fixed_names <- gsub('[-()]', '', fixed_names)
setnames(combined_set, fixed_names)

#Replace the activity labels numbers with the actual names of the activity
for(i in 1:length(activity_labels$V1))
{combined_set$activity[combined_set$activity == activity_labels$V1[i]] <- activity_labels$V2[i]}

#Reshape the data
combined_set_melt <- melt(combined_set, id = c("subject", "activity"))

#Combine subject and activity and fix the column lables
combined_set_mean <- dcast(combined_set_melt, subject + activity ~ variable, mean)
setnames(combined_set_melt, c("subject","activity","measurement","reading"))

write.table(combined_set_melt, paste0(dir_name,"/tidy_data.txt"), row.names = FALSE)
