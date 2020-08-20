library(dplyr)

##first load all the files

activity_labels<-read.table("activity_labels.txt")
features<-read.table("features.txt")
subject_test<-read.table("test/subject_test.txt")
X_test<-read.table("test/X_test.txt")
y_test<-read.table("test/y_test.txt")
subject_train<-read.table("train/subject_train.txt")
X_train<-read.table("train/X_train.txt")
y_train<-read.table("train/y_train.txt")

##bind them together
train<-cbind(subject_train,y_train,X_train)
test<-cbind(subject_test,y_test,X_test)
mydf<-rbind(train,test)

##add the column names using "features.txt"
colnames(mydf)<-c("Subject","Activity",features[,2])

##add the activity labels by merging and rename "V2" to "Activity"
labeled_df<-merge(activity_labels,mydf,by.x = "V1",by.y = "Activity")
names(labeled_df)[2]<-"Activity"

##select the columns with mean and standard dev
mean_dev<-grep("Subject|Activity|-(mean|std)",names(labeled_df))
selectedCol<-labeled_df[mean_dev]

##make the columns more readable
names(selectedCol)<-gsub("^t", "time", names(selectedCol))
names(selectedCol)<-gsub("^f", "frequency", names(selectedCol))
names(selectedCol)<-gsub("Acc", "Accelerometer", names(selectedCol))
names(selectedCol)<-gsub("Gyro", "Gyroscope", names(selectedCol))
names(selectedCol)<-gsub("Mag", "Magnitude", names(selectedCol))
names(selectedCol)<-gsub("BodyBody", "Body", names(selectedCol))

##tidy data set by group them by Subject and Activity
FinalData <- selectedCol%>%
  group_by(Subject, Activity) %>%
  summarise_all(funs(mean))
write.table(FinalData, "FinalData.txt", row.name=FALSE)

