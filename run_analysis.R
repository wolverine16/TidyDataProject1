## Function mrgTestAndTrain
## Assumes the following files exist in the working directory
## - subject_test.txt
## - y_test.txt
## - X_test.txt
## - subject_train.txt
## - y_train.txt
## - X_train.txt
## - features.txt
## - activity_labels.txt
## The function returns a data frame with the merged data.
## The first 561 column names are of the features from the features.txt file
## There are two more columns one for the subjectid and other for the activity
## The function writes the data frame to file so that it can be loaded later

mrgTestAndTrain <- function() {
  
  activitylabels <- read.table("activity_labels.txt",header = FALSE)
  features <- read.table("features.txt",header = FALSE)
  featureNames <- fixNames(features[,2])
  featureCols <- paste(features[,1],featureNames,sep = "")
  
  ## load test data
  subjectsTest <- read.table("subject_test.txt", header = FALSE)
  activityTest <- read.table("y_test.txt", header = FALSE)
  activityTestFac <- cut(activityTest[,1], breaks = 6, labels = as.character(activitylabels[,2]))
  dfData <- read.table("X_test.txt",header = FALSE)
  colnames(dfData) <- as.character(featureCols)
  dfData$subjectid <- subjectsTest[,1]
  dfData$activity <- activityTestFac
  
  
  ## load train data
  subjectsTrain <- read.table("subject_train.txt", header = FALSE)
  activityTrain <- read.table("y_train.txt", header = FALSE)
  activityTrainFac <- cut(activityTrain[,1], breaks = 6, labels = as.character(activitylabels[,2]))
  dfTrain <- read.table("X_train.txt", header = FALSE)
  colnames(dfTrain) <- as.character(featureCols)
  dfTrain$subjectid <- subjectsTrain[,1]
  dfTrain$activity <- activityTrainFac
  
  ## Merge Data
  dfData <- rbind(dfData,dfTrain)
  
  ## write data
  write.table(dfData,"totalData-v1.txt", row.names = FALSE, col.names = TRUE)
  
  dfData
  
}

fixNames <- function(inputdata){
    
    x <- inputdata
    x <- gsub("^t","time",x)
    x <- gsub("^f","frequency",x)
    x <- gsub("-","",x)
    x <- gsub("Acc","acceleration",x)
    x <- gsub("Mag","magnitude",x)
    x <- gsub("std","standarddeviation",x)
    x <- gsub("mad","medianabsolutedeviation",x)
    x <- gsub("max","maximum",x)
    x <- gsub("min","minimum",x)
    x <- gsub("sma","signalmagnitudearea",x)
    x <- gsub("\\()","",x)
    x <- gsub("iqr","interquartilerange",x)
    x <- gsub("arCoeffs","autoregressioncoefficients",x)
    x <- gsub(",","to",x)
    
    
    write.table(x, "CodeBook.md",col.names = FALSE)
    x
}

## Function extMeanSDOnly
## Assumes the dplyr package is installed

extMeanSDOnly <- function(inputData) {
  
  ## Assumes first tidy data set exists
  suppressMessages(library(plyr))
  suppressMessages(library(dplyr))
  
  ## get mean and sd cols
  ## allcols <- names(inputData)
  meansdCols <- grep("mean|std",names(inputData),value = TRUE)
  
  meansdData <- inputData[,meansdCols]
  
  ## write data to file in mean-sd-data.txt
  write.table(inputData,"mean-sd-data.txt",row.names = FALSE, col.names = TRUE)
  
  meansdData
  
}



avgBySubjectActivity <- function(inputData) {
  
  splitBySubject <- split(inputData,inputData$subjectid)
  
  subjects <- length(splitBySubject)
  
  finaldf <- as.data.frame(matrix(ncol = 563,nrow = 0))
  counter <- 1
  
  for (i in 1:subjects)
  {
     splitBySubjectAndActivity <- split(splitBySubject[[i]],splitBySubject[[i]]$activity)
     activities <- length(splitBySubjectAndActivity)
     
     for (j in 1:activities)
     {
        onesubject <- splitBySubjectAndActivity[[j]]$subjectid[1]
        oneactivity <- splitBySubjectAndActivity[[j]]$activity[1]
        onedf <- splitBySubjectAndActivity[[j]][,1:561]
        oneRow <- colMeans(onedf)
        finaldf[counter,] <- c(oneRow,onesubject,oneactivity)
        counter <- counter + 1
     }
  }
  
  colnames(finaldf) <- names(inputData)
  write.table(finaldf, "final-tidy.txt", row.names = FALSE, col.names = TRUE)
  finaldf
}