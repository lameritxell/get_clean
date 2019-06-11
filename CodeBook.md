Assignment Cleaning and Getting Data

1-Introduction

One of the most exciting areas in all of data science right now is wearable computing. Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. 

1.1 Data

The data description is obtained from http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

The data is downloaded from https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

The file provides a readme.txt with necessary information as follows:

For each record it is provided:

- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.
- Triaxial Angular velocity from the gyroscope. 
- A 561-feature vector with time and frequency domain variables. 
- Its activity label : 1 WALKING, 2 WALKING_UPSTAIRS, 3 WALKING_DOWNSTAIRS, 4 SITTING, 5 STANDING, 6 LAYING.

- An identifier of the subject who carried out the experiment

- 'features_info.txt': Shows information about the variables used on the feature vector.

- 'features.txt': List of all features.

- 'activity_labels.txt': Links the class labels with their activity name.

- 'train/X_train.txt': Training set.

- 'train/y_train.txt': Training labels.

- 'test/X_test.txt': Test set.

- 'test/y_test.txt': Test labels.

The following files are available for the train and test data. Their descriptions are equivalent. 

- 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 

- 'train/Inertial Signals/total_acc_x_train.txt': The acceleration signal from the smartphone accelerometer X axis in standard gravity units 'g'. Every row shows a 128 element vector. The same description applies for the 'total_acc_x_train.txt' and 'total_acc_z_train.txt' files for the Y and Z axis. 

- 'train/Inertial Signals/body_acc_x_train.txt': The body acceleration signal obtained by subtracting the gravity from the total acceleration. 

- 'train/Inertial Signals/body_gyro_x_train.txt': The angular velocity vector measured by the gyroscope for each window sample. The units are radians/second. 

Notes: 
======
- Features are normalized and bounded within [-1,1].
- Each feature vector is a row on the text file.
- I use Mean value (mean) and Standard deviation (std) variables for the assignment.

More info is provided in the features_info.txt from the zip folder

References: Jorge L. Reyes-Ortiz, Alessandro Ghio, Luca Oneto, Davide Anguita. November 2012.


2. The Assignment

I unzip and load the files. 

I call the libraries to be used (dplyr).

I assign variables names to the "activity_labels.txt" database. 1 WALKING, 2 WALKING_UPSTAIRS, 3 WALKING_DOWNSTAIRS, 4 SITTING, 5 STANDING, 6 LAYING.

I read.table "features.txt" and look for the features that measure mean or standard deviation (std). For that, I search through literals and metacharacters for "mean" and "std" characters.  
mean_r_features<-grep("mean", r_features$variable, value = FALSE)
SD_r_features<-grep("std", r_features$variable, value = FALSE)
From the initial 561 variables, I choose the ones that result from this screening.

I read.table "subject_test.txt". We have 2947 data in the test data set coming from the subjects indicated on the table and the number of observations per subject as read after command "table(r_subject_test)"  2(302)   4(317)   9(288)  10(294)  12(320)  13(327)  18(364)  20(354)  24(381).
Similarly, I use the same procedure for "subject_train.txt".

I read.table "test/train.txt" data sets. There are initially 561 columns representing the variables. As explained in previously, I will take into account part of those variables for the dataframe construction and the 2947/7352 observations. 



2.1 Merges the training and the test sets to create one data set. 
test<-cbind(r_Y_test, r_X_test)
train<-cbind(r_Y_train, r_X_train)
test_train<-rbind(test, train)

2.2 Extracts only the measurements on the mean and standard deviation for each measurement. 
SD_r_features1<-SD_r_features + 1 #the first corresponds to the index
mean_r_features1<-mean_r_features + 1 #these are the columns to select
test_train_selected<-test_train%>%select(1, mean_r_features1, SD_r_features1)

2.3 Uses descriptive activity names to name the activities in the data set
f_test_train_selected<-as.factor(test_train_selected[, 1])
levels(f_test_train_selected) <- r_activity_labels$activity
test_train_selected$index_activity <- f_test_train_selected

2.4 Appropriately labels the data set with descriptive variable names. 
sd_mean<-c(mean_r_features, SD_r_features) # I gather mean_r_features and sd_r_features (using the index variable)
sort_sd_mean<-sort(sd_mean) # I sort the two data sets by index number
colnames(test_train_selected)<-c("activity", sort_sd_mean) #change the names
l<-slice(r_features, c(1:6, 41:46, 81:86, 121:126, 161:166, 201, 202, 214, 215, 227, 228, 240, 241, 253, 254, 266:271, 294:296, 345:350, 373:375, 424:429, 452:454, 503:504, 513, 516, 517,526, 529, 530, 539, 542, 543, 552))
# I take the features I need
k<-l[,2] #I select the second column to have the name
colnames(test_train_selected)<-c("activity", k) # new names

2.5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable
for each activity and each subject.
I merge train and test data subject sets to put these data into the final dataframe
rb<-rbind(r_subject_test, r_subject_train)
colnames(testdf)<-"tr_or_test"
colnames(traindf)<-"tr_or_test"
t_t<-rbind(testdf, traindf)
and I merge with dataframe "test_train_selected"
df<-cbind(t_t, rb, test_train_selected) 
tbl_df(df) #I make it more compact to run the analysis

Summaries by activity or subject
by_activity<-select(df, -subject)
by_activity_1<-by_activity%>% group_by(activity, tr_or_test)
by_activity_1 %>% summarise_all(list(mean=mean, sd=sd))

by_subject<-select(df, -activity)
by_subject_1<-by_subject%>% group_by(subject, tr_or_test)
by_subject_1 %>% summarise_all(list(mean=mean, sd=sd))


Codebook of final dataframe (df)

1: test_or_train
Factor w/ 2 levels "test","train"
2:  subject
int: it can go from 1 to 30
3: activity: Factor w/ 6 levels "WALKING","WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING"
4 to 82: variables measured (numeric variable).


