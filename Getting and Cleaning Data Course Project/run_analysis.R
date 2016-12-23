##Getting and Cleaning Data - Course Project
###Lindsey Gummersall
##Downloaded and unzipped the file in working directory.
setwd("/Users/lindseygummersall/Documents/Data Science Coursera")
library(plyr)


##Section 1: Read in and Merge Data Sets
##Read in activity labels & features
activityLabels <- read.table('activity_labels.txt', header = FALSE)
features <- read.table('features.txt', header=FALSE)

  ##Assign column names for activity labels
  colnames(activityLabels) = c('activityID', 'activityType')

##Read in data from training files
x_train <- read.table('X_train.txt', header=FALSE)
y_train <- read.table('y_train.txt', header=FALSE)

  ##Asign column names for training files
  colnames(x_train) = features[,2]
  colnames(y_train) = "activityID"
  colnames(subject_train) = "subjectID"

##Read in data from test files
x_test <- read.table('X_test.txt')
y_test <- read.table('y_test.txt')
subject_test <- read.table('subject_test.txt')

  ##Asign column names for test files
  colnames(x_test) = features[,2]
  colnames(y_test) = "activityID"
  colnames(subject_test) = "subjectID"
  
##Merge the training and test sets to create one combined data set.
train <- cbind(subject_train, y_train, x_train)
test <- cbind(subject_test, y_test, x_test)
combined <- rbind(train, test)


##Section 2: Extract mean and standard deviation data from data set using a logical vector.
colNames= colnames(combined)
lv = (grepl("activity..", colNames)| grepl("subject..", colNames)| grepl("-mean..", colNames) & 
        !grepl("-meanFreq..", colNames)| grepl("-std..", colNames) & !grepl("-std()..-", colNames))
finalData = combined[lv == TRUE]

  ##Merge final data set with the activityLabels to include descriptive activity names
  finalData= merge(finalData, activityLabels, by='activityID', all.x = TRUE)
  colNames= colnames(finalData)


##Section 3: Appropriately label the data set with descriptive activity names 
##using the gsub() function.
for (i in 1:length(colNames)){
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","Time",colNames[i])
  colNames[i] = gsub("^(f)","Freqeuncy",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","AngularSpeed",colNames[i])
  colNames[i] = gsub("AccMag","AccelerationMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","AccelerationMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","AngularMagnitude",colNames[i])
}
colnames(finalData)=colNames


##Section 5: Create second, independently tidy data set with the average of each,
##variable for each activity, and each subject.
finalData_noat = finalData[, names(finalData) !='activityType']
tidyData= ddply(finalData_noat, c("subjectID", "activityID"), numcolwise(mean))
  write.table(tidyData, file="TidyData.txt")

