library(memisc)
##Lets set the working directory
setwd(dir = "D:/Documents and Settings/tnjunge/Documents/R")
##Download the file from the internet
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", destfile = "D:/Documents and Settings/tnjunge/Documents/R/UCIHARdataset.zip")
##merge the training and test sets to create one dataset
##read in the training datasets
Trainingdata<-read.table("D:/Documents and Settings/tnjunge/Documents/R/UCIHARdataset/UCI HAR Dataset/train/X_train.txt")
Trainlabels<-read.table("D:/Documents and Settings/tnjunge/Documents/R/UCIHARdataset/UCI HAR Dataset/train/Y_train.txt")
Trainsubject<-read.table("D:/Documents and Settings/tnjunge/Documents/R/UCIHARdataset/UCI HAR Dataset/train/subject_train.txt")
##read in the test datasets

Testdata<-read.table("D:/Documents and Settings/tnjunge/Documents/R/UCIHARdataset/UCI HAR Dataset/test/X_test.txt")
Testlabels<-read.table("D:/Documents and Settings/tnjunge/Documents/R/UCIHARdataset/UCI HAR Dataset/test/Y_test.txt")
Testsubject<-read.table("D:/Documents and Settings/tnjunge/Documents/R/UCIHARdataset/UCI HAR Dataset/test/subject_test.txt")
##join the datasets per table
Traintestdata<-rbind(Trainingdata,Testdata)
Traintestlabels<-rbind(Trainlabels,Testlabels)
Traintestsubject<-rbind(Trainsubject,Testsubject)

##Extract only the measuremments on standard deviation and mean for each measurement
features<-read.table("D:/Documents and Settings/tnjunge/Documents/R/UCIHARdataset/UCI HAR Dataset/features.txt")
meanstddev <- grep("mean\\(\\)|std\\(\\)", features[, 2])

## uses descriptive activity names to name the activities in the data set
Activities<-read.table("D:/Documents and Settings/tnjunge/Documents/R/UCIHARdataset/UCI HAR Dataset/activity_labels.txt")
ActivityLabel <- Activities[Traintestlabels[, 1], 2]
Traintestlabels[, 1] <- ActivityLabel
names(Traintestlabels) <- "Activities"

##Appropriatetly label the datasets with decriptive variable names
names(Traintestsubject) <-"subject"
combineddata<- cbind(Traintestsubject,Traintestlabels,Traintestdata)

##From the "combinedata" above,create a second independent tidy data set with the average for each 
##activity and subject
subjectLength <- length(table(Traintestsubject)) 
activityLength <- dim(Activities)[1] 
columnLength <- dim(combineddata)[2]
tidy <- matrix(NA, nrow=subjectLength*activityLength, ncol=columnLength) 
tidy <- as.data.frame(tidy)
colnames(tidy) <- colnames(combineddata)
row <- 1
for(i in 1:subjectLength) {
  for(j in 1:activityLength) {
    tidy[row, 1] <- sort(unique(Traintestsubject)[, 1])[i]
    tidy[row, 2] <- Activities[j, 2]
    bool1 <- i == combineddata$subject
    bool2 <- Activities[j, 2] == combineddata$Activities
    tidy[row, 3:columnLength] <- colMeans(combineddata[bool1&bool2, 3:columnLength])
    row <- row + 1
  }
}
head(tidy)
dim(tidy)
write.table(tidy, "tidy.txt") 





