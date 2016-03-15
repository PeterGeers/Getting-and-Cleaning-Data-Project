# Getting-and-Cleaning-Data-Project
## This file explains the analysis files is clear and understandable

## Set variables to Null for later use to test if files are read or not
XTrain <- XTest <- NULL
YTrain <- YTest <- NULL
featureNames <- activityNames<-NULL
subjectTrain <- subjectTest <- NULL

## If file does not exist dowmnload source file
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipFile <-"./data/datset.zip"
if(!file.exists(zipFile))
        {download.file(url, destfile = zipFile)}

## if file not unzipped, unzip zipFile
dataDir <- "./data/UCI HAR Dataset"
if(!file.exists(dataDir)) 
        { unzip(zipFile, exdir = "./data/.") }

## Show extracted .txt files only
filesDownloaded <- list.files(path=dataDir, pattern =".txt$" ,full.names = TRUE, recursive = TRUE)
## filesDownloaded ## if you want to look

## Explore data sets to be loaded
fileXTrain <- grep(pattern ="train/X_train.txt$",filesDownloaded, value = TRUE)       
fileXTest <-  grep(pattern ="test/X_test.txt$",filesDownloaded, value = TRUE)
fileYTrain <- grep(pattern ="train/y_train.txt$", filesDownloaded, value =TRUE)       
fileYTest <-  grep(pattern ="test/y_test.txt$", filesDownloaded, value =TRUE)
fileFeaturenames <- grep(pattern ="features.txt$" ,filesDownloaded, value = TRUE)
fileActivityNames <- grep(pattern ="/activity_labels.txt", filesDownloaded, value = TRUE)
fileSubjectTrain <- grep(pattern = "subject_train.txt", filesDownloaded, value = TRUE)
fileSubjectTest <- grep(pattern = "subject_test.txt", filesDownloaded, value = TRUE)

## If files are not loaded then read table in data.tables
if (is.null(XTrain)) {XTrain <- read.table(fileXTrain) }
if (is.null(XTest)) {XTest <- read.table(fileXTest) }
if (is.null(featureNames)) {featureNames <- read.table(fileFeaturenames)[,2]}
if (is.null(YTrain)) {YTrain <- read.table(fileYTrain) }
if (is.null(YTest)) {YTest <- read.table(fileYTest) }
if (is.null(activityNames)) {activityNames <- read.table(fileActivityNames)[,2]}
if (is.null(subjectTrain)){subjectTrain <- read.table(fileSubjectTrain)}
if (is.null(subjectTest)) {subjectTest  <- read.table(fileSubjectTest)}

## Merge X-axis
X_Merged <- rbind (XTrain, XTest)

## Set columnnames to featureNames
names(X_Merged) <- featureNames

## Extract only the measurements on the mean and standard deviation for each measurement.
## Limit to columns with feature names matching mean() or std():
matches <- grep("(mean|std)\\(\\)", names(X_Merged))
## create dataset (linmited) with only mean and stdev observatiions per row
limited <- X_Merged[, matches]

## Use descriptive activity names to name the activities in the data set.
## Get the activity data and map to nicer names:
Y_Merged <- rbind(YTrain, YTest)[, 1]

## set activities "Walking", "Walking Upstairs", "Walking Downstairs", "Sitting", "Standing", "Laying"
activities <- activityNames[Y_Merged]

## Appropriately label the data set with descriptive variable names.
## Change t to Time, f to Frequency, mean() to Mean and std() to StdDev
## Remove extra dashes and BodyBody naming error from original feature names
names(limited) <- gsub("^t", "Time", names(limited))
names(limited) <- gsub("^f", "Frequency", names(limited))
names(limited) <- gsub("-mean\\(\\)", "Mean", names(limited))
names(limited) <- gsub("-std\\(\\)", "StdDev", names(limited))
names(limited) <- gsub("-", "", names(limited))
names(limited) <- gsub("BodyBody", "Body", names(limited))

## Add activities and subject with nice names
subjects <- rbind(subjectTrain, subjectTest)[, 1]

tidy <- cbind(Subject = subjects, Activity = activities, limited)

## Create a second, independent data set (tidyMeans) with the average of each variable for each activity and each subject.
library(plyr)
# Column means for all but the subject and activity columns
limitedColMeans <- function(data) { colMeans(data[,-c(1,2)]) }
tidyMeans <- ddply(tidy, .(Subject, Activity), limitedColMeans)
names(tidyMeans)[-c(1,2)] <- paste0("Mean", names(tidyMeans)[-c(1,2)])

## Write file and check result
write.table(tidyMeans, "./data/UCI HAR Dataset/tidyMeans.txt", row.names = FALSE)

## Use to check that the tidyMeans.txt is properly readable
checkData <-  read.table("./data/UCI HAR Dataset/tidyMeans.txt", header = TRUE)
head(checkData)
summary(checkData)
str(checkData)

## Build from to table for Features for CodeBook.md
tidyMeansNames <- names(tidyMeans)
tidyNames <- names(tidy)
tn <- cbind(tidyNames, tidyMeansNames)
tn <- paste(tn[,2], tn[,1], sep = " is Mean of ")
tn[3:68]
