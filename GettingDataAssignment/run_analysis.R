library(dplyr)
library(plyr)
library(data.table)

#Load all the test data
testdfSub <- read.table("UCI HAR Dataset/test/subject_test.txt")
testdfXTest <- read.table("UCI HAR Dataset/test/X_test.txt")
testdfYTest <- read.table("UCI HAR Dataset/test/Y_test.txt")

#Load all the training data
traindfSub <- read.table("UCI HAR Dataset/train/subject_train.txt")
traindfXTrain <- read.table("UCI HAR Dataset/train/X_train.txt")
traindfYTrain <- read.table("UCI HAR Dataset/train/Y_train.txt")

#Get all the correct measurement names
namesdf <- read.table("UCI HAR Dataset/features.txt")
names(namesdf) <- c("SNo","Features")

#Name the appropriate measurement names for both test and train categories
names(testdfXTest) <- namesdf$Features
names(traindfXTrain) <- namesdf$Features

#Volunteer and Activity details for both categories
names(testdfSub) <- "Volunteer_ID"
names(testdfYTest) <- "Activity_ID"
names(traindfSub) <- "Volunteer_ID"
names(traindfYTrain) <- "Activity_ID"

#Activity table describing the details of activities
activityTable <- read.table("UCI HAR Dataset/activity_labels.txt")
names(activityTable) <- c("SNo","Activity")

#Step 1 - Combine the data

#Combine data into a single data set
combinedTestData <- cbind(testdfSub,testdfYTest,testdfXTest)
combinedTrainData <- cbind(traindfSub,traindfYTrain,traindfXTrain)
combinedData <- rbind(combinedTestData,combinedTrainData)

#Step 2 - Extract only mean and std dev 

#Identify the variable names with only mean and std from combined set
#and extract those columns

varnames <- grep("mean|std",names(combinedData))
requiredDF <- combinedData[,c(1,2,varnames)]

#Step 3 - Use descriptive activity names

#Convert the values in activity column from activity table and rename the variable

requiredDF[,2] <- activityTable[requiredDF[,2],2]
names(requiredDF)[2] <- "Activity"

#Step 4 - Appropriately labels the data set with descriptive variable names
names(requiredDF) <- gsub("^t", "Time ", names(requiredDF))
names(requiredDF) <- gsub("^f", "Frequency ",names(requiredDF))
names(requiredDF) <- gsub("Acc-mean\\(\\)\\-", " Acceleration Mean ", names(requiredDF))
names(requiredDF) <- gsub("Acc-std\\(\\)\\-", " Acceleration STD ", names(requiredDF))
names(requiredDF) <- gsub("AccJerk-mean\\(\\)\\-", " Acceleration Jerk Mean ", names(requiredDF))
names(requiredDF) <- gsub("AccJerk-std\\(\\)\\-", " Acceleration Jerk STD ", names(requiredDF))
names(requiredDF) <- gsub("AccJerkMag-mean\\(\\)\\-", " Acceleration Jerk Magnitude Mean ", names(requiredDF))
names(requiredDF) <- gsub("AccJerkMag-std\\(\\)\\-", " Acceleration Jerk Magnitude STD ", names(requiredDF))
names(requiredDF) <- gsub("AccMag-mean\\(\\)\\-", " Acceleration Magnitude Mean ", names(requiredDF))
names(requiredDF) <- gsub("AccMag-std\\(\\)\\-", " Acceleration Magnitude STD ", names(requiredDF))
names(requiredDF) <- gsub("Gyro-mean\\(\\)\\-", " Gyro Mean ", names(requiredDF))
names(requiredDF) <- gsub("Gyro-std\\(\\)\\-", " Gyro STD ", names(requiredDF))
names(requiredDF) <- gsub("GyroJerkMag-mean\\(\\)\\-", " Gyro Jerk Magnitude Mean ", names(requiredDF))
names(requiredDF) <- gsub("GyroJerkMag-std\\(\\)\\-", " Gyro Jerk Magnitude STD ", names(requiredDF))
names(requiredDF) <- gsub("GyroJerk-mean\\(\\)\\-", " Gyro Jerk Mean ", names(requiredDF))
names(requiredDF) <- gsub("GyroJerk-std\\(\\)\\-", " Gyro Jerk STD ", names(requiredDF))
names(requiredDF) <- gsub("GyroMag-mean\\(\\)\\-", " Gyro Magnitude Mean ", names(requiredDF))
names(requiredDF) <- gsub("GyroMag-std\\(\\)\\-", " Gyro Magnitude STD ", names(requiredDF))

#Step 5 - Average of each variable for each activity and each subject

activityBasedMean <- aggregate(requiredDF,by = list(requiredDF$Activity,requiredDF$Volunteer_ID), mean)

activityBasedMean$Volunteer_ID <- NULL
activityBasedMean$Activity <- NULL
names(activityBasedMean)[1] <- "Activity"
names(activityBasedMean)[2] <- "Volunteer ID" 

#Tidy data set stored here
write.table(activityBasedMean, "tidydata.txt", row.names = FALSE)








