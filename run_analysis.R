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
  featureCols <- paste(features[,1],features[,2],sep = "")
  
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
  ## Create Column name vector
  ## Add Column Vector
  
  ## Write tidy Data set to file
  
}

fixNames <- function(features = x){
    
    dfChanges <- data.frame(pattern = character(), replacement = character(), stringsAsFactors = FALSE)
    dfChanges[1,] <- c("-","")
    dfChanges[2,] <- c("Acc","acceleration")
    dfChanges[3,] <- c("Mag", "magnitude")
    dfChanges[4,] <- c("std", "standarddeviation")
    dfChanges[5,] <- c("mad","medianabsolutedeviation")
    dfChanges[6,] <- c("max","maximum")
    dfChanges[7,] <- c("min","minimum")
    dfChanges[8,] <- c("sma","signalmagnitudearea")
    dfChanges[9,] <- c("\\()","")
    dfChanges[10,] <- c("iqr","interquartilerange")
    dfChanges[11,] <- c("arCoeff","autoregressioncoefficients")
    dfChanges[12,] <- c(",","to")
    
    mgsub(dfChanges,x)
   
}

mgsub <- function(dfPatterns, x, ...) {
  
  rows <- nrow(dfPatterns)
  result <- x
  
  for (i in 1:rows)
  {
      result <- gsub(dfPatterns$pattern[i],dfPatterns$replacement[i],result,...)
  }
  
  result
  
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
  
  finaldf <- data.frame(stringsAsFactors = FALSE)
  counter <- 1
  
  for (i in 1:subjects)
  {
     splitBySubjectAndActivity <- split(splitBySubject[[i]],splitBySubject[[i]]$activity)
     activities <- length(splitBySubjectAndActivity)
     
     for (j in 1:activities)
     {
        onedf <- splitBySubjectAndActivity[[j]][,1:561]
        oneRow <- colMeans(onedf)
        finaldf[counter,] <- list(oneRow,splitBySubjectAndActivity[[j]]$subjectid,splitBySubjectAndActivity[[j]]$activity)
        
     }
  }
  
  write.table(finaldf, "final-tidy.txt", row.names = FALSE, col.names = TRUE)
  
}