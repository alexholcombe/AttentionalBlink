#Loads raw data from MATLAB files for second backwards paper first experiment
require(R.matlab)

userNameOnMachineOnWayToGoogleDrive<- "alexh" #"admin" on the machine that Chris Bush used
GoogleDrivePath<-"/Google\ Drive/Backwards\ paper/"

#random orientation each trial. First experiment of second backwards-letters paper

directoryOfRawData<- paste0("/Users/",userNameOnMachineOnWayToGoogleDrive,GoogleDrivePath,
                            "secondPaper/E1/Data/RawData/Data/")
#raw data path containing .mat file for each subject
rawDataPath <- file.path(directoryOfRawData)

turnMATintoMeltedDataframe<- function(fromMAT) {
  #It was imported as a list of lists
  mydata<-fromMAT
  numTrials<- length( unlist( data$allConditions ) )
  #Some of the list members are one-off fields pertaining to entire experiment. Remove them, put in miscInfo
  miscInfo <- list()
  membersToDelete<- list()
  #loop through MATLAB .mat file data structure
  for (i in 1:length(mydata)) {
    thisListMember <- mydata[i]
    name<- names(thisListMember)
    len<- length(unlist(thisListMember))
    #cat(name, len,'\n')           
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
      } else if (ndim==2) { #else it has separate values for each target
        dfThis[[name]] <- thisMember[,target] 
      } else if (ndim==3) {
        dfThis[[name]] <-  thisMember[,target,]
      } else { print("unexpected dims")}
      #The above should work for any variables, don't need to know their name or specific formats
      
      #The below is based on kmnowledge of the variables in this case
      dfThis$subject<- miscInfo$participantID[[1]]
      
      #calculate the serial position of the response from the allLetterOrder
      thisStreamLetterOrder<- mydata$allLetterOrder[,target,]
      allResponses<- mydata$allResponses[,target]
      
      #first dimension is trial. second dimension is serial position
      #for (trial in 1:numTrials) {
      #  #for each trial in allResponses, do the match
      #  respSP<- match(allResponses[trial], thisStreamLetterOrder[trial,])
      #}
      #mapply doesn't work because thisStreamLetterOrder is not a list of things I want to operate on
      #mapply(match, 1, list(c(3,2,1))) #this works. So, need to turn thisStreamLetterOrder into many lists
      eachTrialLetterOrderList<- split(thisStreamLetterOrder, row(thisStreamLetterOrder))
      responsePositions<- mapply(match, allResponses, eachTrialLetterOrderList )
      dfThis$respSP<- responsePositions
    }
    meltedDf<-rbind(meltedDf,dfThis)
  }
  
  E<-meltedDf
  names(E)[names(E) == 'allTargets'] <- 'targetSP'
  names(E)[names(E) == 'allLetterOrder'] <- 'letterSeq'
  names(E)[names(E) == 'allResponses'] <- 'resp'
  names(E)[names(E) == 'allConditions'] <- 'condition'
  return(E)
}
temp<-turnMATintoMeltedDataframe(rawDataLoad)

files <- dir(path=rawDataPath,pattern='.mat')  #find all data files in this directory
dfAll<-data.frame()
for (i in 1:length(files)) { #read in each file
  fileThis<- file.path(rawDataPath,files[i])
  rawDataLoad=tryCatch( 
    readMat(fileThis), 
    error=function(e) { 
      stop( paste0("ERROR reading the file ",fname," :",e) )
    } )
  apparentSubjectName <- strsplit(files[i],split="_")[[1]][1]
  subjectName<- rawDataLoad$participantID[[1]]
  if (apparentSubjectName != subjectName) {
    stop( paste0("WARNING apparentSubjectName",apparentSubjectName," from filename does not match subjectName in data structure",subjectName) )
  }
  rawDataLoad$file <- files[i]

  dfThis<- turnMATintoMeltedDataframe(rawDataLoad)

  tryCatch( 
    dfAll<-rbind(dfAll,dfThis), #if fail to bind new with old,
    error=function(e) { #Give feedback about how the error happened
      cat(paste0("Tried to merge but error:",e) )
    } )
}
E<-dfAll

#Calculate the serial position error
E$SPE<- E$respSP - E$targetSP

#Meaning of condition and expDegrees variables
#1 - upright
#2 - inverted
      
#save dataframe as csv file
saveDir<-"data/"

saveRDS(E, file=paste0(saveDir,"alexImportBackwardsPaper2E1.Rdata"))
#write.csv(E,paste0(saveDir, "alexImportBackwardsPaper2E1.csv"),row.names=FALSE) #write.csv loses the dimensions


#sanity check the histogram
