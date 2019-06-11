#downloading files
url<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, destfile = "~/cli/cleaning/fi")
un<-unzip("fi", exdir = "/home/didac/cli/cleaning/")

#libraries
library(dplyr)

#1<<activity labels 
activity_labels<-un[1]
r_activity_labels<-read.table(activity_labels, stringsAsFactors = TRUE)
colnames(r_activity_labels)<-c("index_activity", "activity") #change col names

#2<<features
features<-un[2]
r_features<-read.table(features, stringsAsFactors = FALSE)
# there are 561 variables
dim(r_features)
colnames(r_features)<-c("index_variable", "variable") #change col names
#search through literals and metacharacters for mean and standard deviations characters features
mean_r_features<-grep("mean", r_features$variable, value = FALSE)
SD_r_features<-grep("std", r_features$variable, value = FALSE)

#<< subject test
subject_test<-un[14]
r_subject_test<-read.table(subject_test, col.names = "subject")
str(r_subject_test)
#2947 obs of one var
table(r_subject_test) # obs where obtained for those subjects
# I create a dataframe to indicate whether the data comes from test individuals. 
l_test<-length(r_subject_test$subject)  
testdf<-data.frame(rep("test", l_test))

#<< X_test
X_test<-un[15]
r_X_test<-read.table(X_test)
str(r_X_test, 1) #2947 obs of 561 var


#<< Y_test
Y_test<-un[16]
r_Y_test<-read.table(Y_test)
head(r_Y_test, 1) #2947 obs of 1 var (activity index)
colnames(r_Y_test)<-c("index_activity") #change col names

#<< subject_train
subject_train<-un[26]
r_subject_train<-read.table(subject_train, col.names = "subject")
head(r_subject_train, 1)
dim(r_subject_train)
table(r_subject_train) #see subject test for details
l_train<-length(r_subject_train$subject)
traindf<-data.frame(rep("train", l_train))

#<< X_train
X_train<-un[27]
r_X_train<-read.table(X_train)
dim(r_X_train) # 7352 obs from 561 variables


#<< Y_train
Y_train<-un[28]
r_Y_train<-read.table(Y_train)
dim(r_Y_train)
colnames(r_Y_train)<-c("index_activity") #change col names


# 1.Merge test and training datasets 

test<-cbind(r_Y_test, r_X_test)
train<-cbind(r_Y_train, r_X_train)
test_train<-rbind(test, train)
dim(test_train)
tbl_df(test_train)

# 2. Extracts only the measurements on the mean and standard deviation

SD_r_features1<-SD_r_features + 1 #the first corresponds to the index
mean_r_features1<-mean_r_features + 1 #these are the columns to select
test_train_selected<-test_train%>%select(1, mean_r_features1, SD_r_features1)

# 3. Uses descriptive activity names to name the activities in the data set

f_test_train_selected<-as.factor(test_train_selected[, 1])
levels(f_test_train_selected) <- r_activity_labels$activity
test_train_selected$index_activity <- f_test_train_selected
str(test_train_selected$index_activity)
head(test_train_selected)

# 4. Appropriately labels the data set with descriptive variable names. 
SD_r_features<-grep("std", r_features$variable, value = FALSE)
mean_r_features<-grep("mean", r_features$variable, value = FALSE)
sd_mean<-c(mean_r_features, SD_r_features)
sort_sd_mean<-sort(sd_mean)
colnames(test_train_selected)<-c("activity", sort_sd_mean)
mean_r_features<-grep("mean", r_features$variable, value = FALSE)
SD_r_features<-grep("std", r_features$variable, value = FALSE)
head(test_train_selected, 1)
l<-slice(r_features, c(1:6, 41:46, 81:86, 121:126, 161:166, 201, 202, 214, 215, 227, 228, 240, 241, 253, 254, 266:271, 294:296, 345:350, 373:375, 424:429, 452:454, 503:504, 513, 516, 517,526, 529, 530, 539, 542, 543, 552))
class(l)
k<-l[,2]
colnames(test_train_selected)<-c("activity", k)

#From the data set in step 4, creates a second, independent tidy data set with the average of each variable
#for each activity and each subject.

# merge train and test data sets

rb<-rbind(r_subject_test, r_subject_train)

colnames(testdf)<-"tr_or_test"
colnames(traindf)<-"tr_or_test"
t_t<-rbind(testdf, traindf)
#and merge with dataframe test train selected
df<-cbind(t_t, rb, test_train_selected) #all merged
str(df)
head(df, 1)
tbl_df(df)

by_activity<-select(df, -subject)
by_activity_1<-by_activity%>% group_by(activity, tr_or_test)
by_activity_1 %>% summarise_all(list(mean=mean, sd=sd))

by_subject<-select(df, -activity)
by_subject_1<-by_subject%>% group_by(subject, tr_or_test)
by_subject_1 %>% summarise_all(list(mean=mean, sd=sd))

