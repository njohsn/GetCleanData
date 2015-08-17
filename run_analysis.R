##See README within this repository for additional explanation of code

## read in data from each file related to the test data set
test_subjects <- read.csv("subject_test.txt", header=FALSE)
test_act <- read.csv("y_test.txt", header=FALSE)
test_obs <- read.table("X_test.txt", col.names=c(1:561), header=FALSE)

## combine into one data set for test data
test <- cbind(test_subjects, test_act, test_obs)

## read in data from each file related to the training data set
train_subjects <- read.csv("subject_train.txt", header=FALSE)
train_act <- read.csv("y_train.txt", header=FALSE)
train_obs <- read.table("X_train.txt", col.names=c(1:561), header=FALSE)

## combine into one data set for training data
train <- cbind(train_subjects, train_act, train_obs)

## combine test and training data into one data set
full_data <- rbind(train, test)

## read features file into new variable
obs_names <- read.table("features.txt", header=FALSE)

## establish an object containing names for all columns in full data set
labels <- make.names(c("Subject", "Activity", as.vector(obs_names[,2])), unique=TRUE)

## assign names to columns of the full data set
colnames(full_data) <- labels

## select only variables that relate to a mean or standard deviation
stat_data <- select(full_data, Subject, Activity, contains("mean", ignore.case=TRUE), contains("std", ignore.case=TRUE))

## establish an object containing more descriptive column names - see code book for more information
clean_names <-c("Subject", "Activity", "TimeBodyAccelerometerMeanX", "TimeBodyAccelerometerMeanY", "TimeBodyAccelerometerMeanZ", "TimeGravityAccelerometerMeanX", "TimeGravityAccelerometerMeanY", "TimeGravityAccelerometerMeanZ", "TimeBodyAccelerometerJerkMeanX", "TimeBodyAccelerometerJerkMeanY", "TimeBodyAccelerometerJerkMeanZ", "TimeBodyGyroscopeMeanX", "TimeBodyGyroscopeMeanY", "TimeBodyGyroscopeMeanZ", "TimeBodyGyroscopeJerkMeanX", "TimeBodyGyroscopeJerkMeanY", "TimeBodyGyroscopeJerkMeanZ", "TimeBodyAccelerometerMagnitudeMean", "TimeGravityAccelerometerMagnitudeMean", "TimeBodyAccelerometerJerkMagnitudeMean", "TimeBodyGyroscopeMagnitudeMean", "TimeBodyGyroscopeJerkMagnitudeMean", "FrequencyBodyAccelerometerMeanX", "FrequencyBodyAccelerometerMeanY", "FrequencyBodyAccelerometerMeanZ", "FrequencyBodyAccelerometerMeanFrequencyX", "FrequencyBodyAccelerometerMeanFrequencyY", "FrequencyBodyAccelerometerMeanFrequencyZ", "FrequencyBodyAccelerometerJerkMeanX", "FrequencyBodyAccelerometerJerkMeanY", "FrequencyBodyAccelerometerJerkMeanZ", "FrequencyBodyAccelerometerJerkMeanFrequencyX", "FrequencyBodyAccelerometerJerkMeanFrequencyY", "FrequencyBodyAccelerometerJerkMeanFrequencyZ", "FrequencyBodyGyroscopeMeanX", "FrequencyBodyGyroscopeMeanY", "FrequencyBodyGyroscopeMeanZ", "FrequencyBodyGyroscopeMeanFrequencyX", "FrequencyBodyGyroscopeMeanFrequencyY", "FrequencyBodyGyroscopeMeanFrequencyZ", "FrequencyBodyAccelerometerMagnitudeMean", "FrequencyBodyAccelerometerMagnitudeMeanFrequency", "FrequencyBodyAccelerometerJerkMagnitudeMean", "FrequencyBodyAccelerometerJerkMagnitudeMeanFrequency", "FrequencyBodyGyroscopeMagnitudeMean", "FrequencyBodyGyroscopeMagnitudeMeanFrequency", "FrequencyBodyGyroscopeJerkMagnitudeMean", "FrequencyBodyGyroscopeJerkMagnitudeMeanFrequency", "AngleTimeBodyAccelerometerMeanGravity", "AngleTimeBodyAccelerometerJerkMeanGravityMean", "AngletBodyGyroscopeMeanGravityMean", "AngleTimeBodyGyroscopeJerkMeanGravityMean", "AngleXGravityMean", "AngleYGravityMean", "AngleZGravityMean", "TimeBodyAccelerometerSTDX", "TimeBodyAccelerometerSTDY", "TimeBodyAccelerometerSTDZ", "TimeGravityAccelerometerSTDX", "TimeGravityAccelerometerSTDY", "TimeGravityAccelerometerSTDZ", "TimeBodyAccelerometerJerkSTDX", "TimeBodyAccelerometerJerkSTDY", "TimeBodyAccelerometerJerkSTDZ", "TimeBodyGyroscopeSTDX", "TimeBodyGyroscopeSTDY", "TimeBodyGyroscopeSTDZ", "TimeBodyGyroscopeJerkSTDX", "TimeBodyGyroscopeJerkSTDY", "TimeBodyGyroscopeJerkSTDZ", "TimeBodyAccelerometerMagnitudeSTD", "TimeGravityAccelerometerMagnitudeSTD", "TimeBodyAccelerometerJerkMagnitudeSTD", "TimeBodyGyroscopeMagnitudeSTD", "TimeBodyGyroscopeJerkMagnitudeSTD", "FrequencyBodyAccelerometerSTDX", "FrequencyBodyAccelerometerSTDY", "FrequencyBodyAccelerometerSTDZ", "FrequencyBodyAccelerometerJerkSTDX", "FrequencyBodyAccelerometerJerkSTDY", "FrequencyBodyAccelerometerJerkSTDZ", "FrequencyBodyGyroscopeSTDX", "FrequencyBodyGyroscopeSTDY", "FrequencyBodyGyroscopeSTDZ", "FrequencyBodyAccelerometerMagnitudeSTD", "FrequencyBodyAccelerometerJerkMagnitudeSTD", "FrequencyBodyGyroscopeMagnitudeSTD", "FrequencyBodyGyroscopeJerkMagnitudeSTD")

## assign names to columns of this data set
colnames(stat_data) <- clean_names

## convert activity codes to descriptive activity names
stat_data <- stat_data %>% mutate(Activity =  ifelse(Activity == 1, "WALKING",
                                                     ifelse(Activity == 2, "WALKING_UPSTAIRS",
                                                            ifelse(Activity == 3, "WALKING_DOWNSTAIRS",
                                                                   ifelse(Activity == 4, "SITTING",
                                                                          ifelse(Activity == 5, "STANDING",
                                                                                 ifelse(Activity == 6, "LAYING", "")))))))

## create new data set based on mean and standard deviation measurements of the original full data set
## containing the average for each variable grouped by activity and subject
Subject_Activity_means <- stat_data %>% group_by(Subject, Activity) %>% summarise_each(funs(mean), -(Subject:Activity))

## write that data set to a txt file in the working directory
write.table(Subject_Activity_means, file="Subject_Activity_means.txt", row.names=FALSE, col.names=TRUE)
