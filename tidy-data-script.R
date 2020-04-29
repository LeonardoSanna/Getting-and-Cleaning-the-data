library(data.table)
library(dplyr)

#download file and unzip file
data_source <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
file <- "data-week-4.zip"
download.file(data_source, destfile = file)
dataset <- unzip(file)


#changing wd to unzipped directory. You can find out your path using str(dataset)
setwd("./UCI HAR Dataset")

#read the data
features <- read.table("features.txt", col.names = c("n","functions")) 
activities_labels <- read.table("activity_labels.txt", col.names = c("code", "activity"))
subject_test <- read.table("./test/subject_test.txt", col.names = "subject")
subject_train <- read.table("./train/subject_train.txt", col.names = "subject")

x_test <- read.table("./test/X_test.txt", col.names = features$functions)
y_test <- read.table("./test/y_test.txt", col.names = "code")

x_train <- read.table("./train/X_train.txt", col.names = features$functions)
y_train <- read.table("./train/y_train.txt", col.names = "code")

#create a merged dataset
x_data <- rbind(x_train, x_test)
y_data <- rbind(y_train, y_test)
subject_data <- rbind(subject_train, subject_test)

new_dataset <- cbind(subject_data, y_data, x_data) 

#storing the label of activities mapped as labels of codes
labelling <- factor(new_dataset$code, levels = 1:6, labels = activities_labels$activity)

#tidying the data selecting mean and standard deviation and replacing actvity id with description
tidy_data <- new_dataset %>% 
        select(subject, code, contains("mean"), contains("std")) %>%
        mutate(code = labelling) %>%
        rename(activity = code)


#cleaning some variables names
names(tidy_data)<-gsub("Acc", "Accelerometer", names(tidy_data))
names(tidy_data)<-gsub("Gyro", "Gyroscope", names(tidy_data))
names(tidy_data)<-gsub("BodyBody", "Body", names(tidy_data))
names(tidy_data)<-gsub("Mag", "Magnitude", names(tidy_data))
names(tidy_data)<-gsub("^t", "Time", names(tidy_data))
names(tidy_data)<-gsub("^f", "Frequency", names(tidy_data))
names(tidy_data)<-gsub("tBody", "TimeBody", names(tidy_data))
names(tidy_data)<-gsub("-mean()", "Mean", names(tidy_data), ignore.case = TRUE)
names(tidy_data)<-gsub("-std()", "STD", names(tidy_data), ignore.case = TRUE)
names(tidy_data)<-gsub("-freq()", "Frequency", names(tidy_data), ignore.case = TRUE)

#exporting clean data with the average of each subject per activity
ultimate_data <- tidy_data %>%
        group_by(subject, activity) %>%
        summarise_all(funs(mean))
write.table(ultimate_data, "FinalData.txt", row.name=FALSE)

