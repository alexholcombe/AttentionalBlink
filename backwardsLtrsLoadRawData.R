#Loads raw data from MATLAB files for second backwards paper first experiment
require(R.matlab)

userNameOnMachineOnWayToGoogleDrive<- "alexh" #"admin" on the machine that Chris Bush used
GoogleDrivePath<-"/Google\ Drive/Backwards\ paper/"

#random orientation each trial. First experiment of second backwards-letters paper

directoryOfRawData<- paste0("/Users/",userNameOnMachineOnWayToGoogleDrive,GoogleDrivePath,
                            "secondPaper/E1/Data/RawData/Data/")
#raw data path containing .mat file for each subject
rawDataPath <- file.path(directoryOfRawData)


data <- readMat( str_c(rawDataPath, "AA_17-05-01_1.mat") )
#It's imported as a list of lists
numTrials<- length( unlist( data$allConditions ) )
#Some of the list members are one-off fields pertaining to entire experiment. Remove them, put in miscInfo
mydata<-data
miscInfo <- list()
membersToDelete<- list()
#loop through MATLAB .mat file data structure
for (i in 1:length(mydata)) {
  thisListMember <- mydata[i]
  name<- names(thisListMember)
  len<- length(unlist(thisListMember))
  cat(name, len,'\n')           
  if (len < numTrials) { #one-off information, not whole experiment
    miscInfo[name] <- thisListMember
    membersToDelete<-c(membersToDelete,name)
  } 
}
for (m in membersToDelete) {
  mydata[m]<-NULL #remove from list to leave only every-trial fields
}

#Remaining fields are matrices, one dimension of which is the trial number. Other dimension is whichTarget in dual-stream case
#To melt, need to unpack the matrix, going through the second dimension and taking it out, putting at end to create flat dataframe
# with one row for each trial.
meltedDf <- data.frame()
for (target in 1:2) {
  dfThis <- data.frame(trial=seq(1,numTrials))
  dfThis$target<- target
  for ( i in 1:length(mydata) ) {  #Go through each field
    thisMember <- mydata[i]
    name<- names( thisMember )
    thisMember<- thisMember[[name]] #for some reason it's always a list of 1 (with the name name)
    #if only one dimension then it is a condition name applying to both streams
    thisMember <- drop(thisMember) #remove any singleton dimensions
    ndim <- length( dim( thisMember ) )
    if ((ndim==0)) {  #it is a condition name applying to both streams
      dfThis[[name]] <- thisMember
    }
    else { #else it has separate values for each target
      dfThis[[name]] <- thisMember[target]
    }
  }
  meltedDf<-rbind(meltedDf,dfThis)
}

col_headings <- c('participantID',
                  'block',
                  'randomSeed',
                  'letterSeparation',
                  'letterEccentricity',
                  'itemRate',
                  'condition', 
                  'expDegrees', #Angle the letter is drawn in 
                  'letterOrder1',
                  'target1',
                  'response1',
                  'responseLetter1',
                  'RT1',
                  'letterOrder2',
                  'target2',
                  'response2',
                  'responseLetter2',
                  'RT2',
                  'endTime')

#Meaning of condition and expDegrees variables
#1
#2
             
E1 <- data.frame(matrix(ncol = 19, nrow = 0)) # Dataframe to hold all data
colnames(E2) <- col_headings #Assign column headings
tempData <- data.frame(matrix(ncol = 19, nrow = 100)) #Temp dataFrame for single data files
colnames(tempData) <- col_headings #Assign headings to temp dataFrame

for (fileName in list.files(path, pattern="*.mat")) #Loop through and read in each .mat data file
{
  singleSubjectData <- readMat(file.path(path, fileName)) #Read in single data file
  cat("reading",fileName,"\n")
  
  #Wrangle data into appropraite format
  tempData$participantID <- singleSubjectData$participantID[1]
  tempData$block <- substr(fileName, nchar(fileName) - 4, nchar(fileName) - 4) #Retrieve block number from file name
  tempData$randomSeed <- singleSubjectData$randomSeed[1]
  tempData$letterSeparation <- singleSubjectData$letterSeparation[1]
  tempData$letterEccentricity <- singleSubjectData$letterEccentricity[1]
  tempData$itemRate <- singleSubjectData$itemRate[1]
  tempData$condition <- sapply(singleSubjectData$allConditions, paste0)
  tempData$expDegrees <- paste(singleSubjectData$expDegrees[1,], collapse = ",")
  tempData$letterOrder1 <- apply(singleSubjectData$allLetterOrder[,1,], 1, paste, collapse=",") 
  tempData$target1 <- sapply(singleSubjectData$allTargets[,1], paste0)
  tempData$responseLetter1 <- sapply(singleSubjectData$allResponses[,1], paste0)
  tempData$RT1 <- sapply(singleSubjectData$allRTs[,1], paste0)
  tempData$letterOrder2 <- apply(singleSubjectData$allLetterOrder[,2,], 1, paste, collapse=",")
  tempData$target2 <- sapply(singleSubjectData$allTargets[,2], paste0)
  tempData$responseLetter2 <- sapply(singleSubjectData$allResponses[,2], paste0)
  tempData$RT2 <- sapply(singleSubjectData$allRTs[,2], paste0)
  tempData$endTime <- singleSubjectData$endTime[1]
  
  ##Add serial postion columns for responses
  rowCount <- 1
  for (responses in tempData$response1)
  {
    tempData$response1[rowCount] <- match(tempData$responseLetter1[rowCount],singleSubjectData$allLetterOrder[rowCount,1,])
    tempData$response2[rowCount] <- match(tempData$responseLetter2[rowCount],singleSubjectData$allLetterOrder[rowCount,2,])
    rowCount <- rowCount + 1
  }
  
  E2 <- rbind(E2,tempData) # Add subjects data to main dataFrame
}
#save dataframe as csv file
saveSubdir<-"analysisInR_postLizzy/loadRawData/"
directorytoSaveData<- paste0("/Users/",userNameOnMachineOnWayToGoogleDrive,GoogleDrivePath,
                             saveSubdir)
                            
write.csv(E2,paste0(directorytoSaveData, "E2_BwdsLtrs_Rotate_RawData.csv"),row.names=FALSE)