library(dplyr)

filename <- "Coursera_Data_Cleaning.zip"

# Check and download the file if the file has not been downloaded.
if (!file.exists(filename)){
    fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(fileURL, filename, method="curl")
}  

# Check if "UCI HAR Dataset" folder exists and if not, unzip the file
if (!file.exists("UCI HAR Dataset")) { 
    unzip(filename) 
}

### Create data frames
# features.txt - All features list
features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
# activity_lavels.txt - All activities
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
#subject_test.txt - Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
#subject_train.txt - Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
# Test set - functions in features.txt are column names 
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
# Test labels (1-6: Activiy code)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
# Training set - functions in features.txt are column names 
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
# Training labels (1-6: Activiy code)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")

# 1. Merge the training and the test sets to create one data set
# Test
X <- rbind(x_test, x_train)
Labels <- rbind(y_test, y_train)
SUBJ <- rbind(subject_test, subject_train)
data <- cbind(SUBJ, Labels, X)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement
meanandstd <- select(data, subject:code, contains("mean"), contains("std"))

# 3. Uses descriptive activity names to name the activities in the data set
meanandstd$code <- activities[meanandstd$code, 2]

# 4. Appropriately label the data set with descriptive variable names
colnames(meanandstd)[2] = "activity"
colnames(meanandstd)<-gsub("Acc", "Accelerometer", colnames(meanandstd))
colnames(meanandstd)<-gsub("Gyro", "Gyroscope", colnames(meanandstd))
colnames(meanandstd)<-gsub("BodyBody", "Body", colnames(meanandstd))
colnames(meanandstd)<-gsub("Mag", "Magnitude", colnames(meanandstd))
colnames(meanandstd)<-gsub("^t", "Time", colnames(meanandstd))
colnames(meanandstd)<-gsub("^f", "Frequency", colnames(meanandstd))
colnames(meanandstd)<-gsub("tBody", "TimeBody", colnames(meanandstd))
colnames(meanandstd)<-gsub("-std()", "Standard", colnames(meanandstd), ignore.case = TRUE)
colnames(meanandstd)<-gsub("-freq()", "Frequency", colnames(meanandstd), ignore.case = TRUE)

# 5. From the data set in step 4., create a second, independent tidy data set with the average of each variable for each activity and each subject
tidyData <- group_by(meanandstd, subject, activity)
tidyData <- summarise_all(tidyData, mean)
write.table(tidyData, "tidyData.txt", row.name=FALSE)
