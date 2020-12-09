library(dplyr)

## set working directory
getwd()
setwd(dir = "R/R01_Course_Files/UCI HAR Dataset/")

## 1. start reading x data files
x_test<-read.table("test/X_test.txt")
x_train <- read.table("train/X_train.txt")
xcombine <- rbind(x_test, x_train)

## start reading activity data files

y_test <-read.table(file = "test/y_test.txt")
y_train<- read.table("train/y_train.txt")
ycombine<-rbind(y_test,y_train)

## read subject data

Subject_test <- read.table("test/subject_test.txt")
subject_train <- read.table("train/subject_train.txt")
subjectcombine <-rbind(Subject_test,subject_train)

## read activity labels file
activityLabels <- read.table("activity_labels.txt")


## read features file
features <- read.table("R/R01_Course_Files/UCI HAR Dataset/features.txt")

## name each data
colnames(xcombine) = features[ , 2]
colnames(ycombine) = "activityID"
colnames(subjectcombine) = "subjectID"
colnames(activityLabels) = c("activityID" , "activityTYPE")

## merge all data make complete data set
completedata <- cbind(subjectcombine,ycombine,xcombine)

## 2. Extracts only the measurements on the mean and standard deviation for each measurement.

DatawithMeanandStd <- completedata[,grepl("subjectID|activityID|mean|std",colnames(completedata))]

## 3. Uses descriptive activity names to name the activities in the data set

DatawithMeanandStd$activityID <- factor(DatawithMeanandStd$activityID, levels = activityLabels[,1], labels = activityLabels[,2])

## 4. Appropriately labels the data set with descriptive variable names.

## 4.1 create a new dataframe only with column names

columnnamesneedtobeadjusted <- colnames(DatawithMeanandStd)

## 4.2 remove special characters
columnnamesneedtobeadjusted <- gsub("-","", columnnamesneedtobeadjusted)
columnnamesneedtobeadjusted <- gsub("[][()]","", columnnamesneedtobeadjusted)

## 4.3 rename abbreviation/typo with correct name
columnnamesneedtobeadjusted <- gsub("^t", "time", columnnamesneedtobeadjusted)
columnnamesneedtobeadjusted <- gsub("^f", "frequency", columnnamesneedtobeadjusted)
columnnamesneedtobeadjusted <- gsub("Acc", "Accelerometer", columnnamesneedtobeadjusted)
columnnamesneedtobeadjusted <- gsub("Gyro", "Gyroscope", columnnamesneedtobeadjusted)
columnnamesneedtobeadjusted <- gsub("Mag", "Magnitude", columnnamesneedtobeadjusted)
columnnamesneedtobeadjusted <- gsub("BodyBody", "Body", columnnamesneedtobeadjusted)

## 4.4 apply new colnames to the data set
colnames(DatawithMeanandStd) <- columnnamesneedtobeadjusted

## 5. creates a second, independent tidy data set with the average of each variable for each activity and each subject.

## 5.1 group by subject and activity
groupbysubjectandactivity <- DatawithMeanandStd %>% group_by(subjectID,activityID)

## 5.2 summarize data with mean of each column
tidydataframe <- summarise_each(groupbysubjectandactivity, funs(mean))

## 5.3 generate new data in txt
write.table(tidydataframe, "tidy_data.txt", row.names = F)
