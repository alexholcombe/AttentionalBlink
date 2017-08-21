rm(list=ls()) #clear all variables
#random orientation each trial. First experiment of second backwards-letters paper

mixModelingPath<- file.path("mixtureModeling")


MATLABmixtureModelOutputPath<-"~/Google\ Drive/Backwards\ paper/secondPaper/E1/Data/MixModData"
importedToRbyChris <- "allParams.RData"
MATLABmixtureModelOutput<- file.path( MATLABmixtureModelOutputPath, importedToRbyChris )
  

#raw data path containing .mat file for each subject
directFromMAT <- FALSE #.mat file is in particular format
if (directFromMAT) {  #Have full code for importing lots of MAT Files in backwardsLtrsLoadRawData.R
  rawDataPath<- file.path("~/Google\ Drive/Backwards\ paper/secondPaper/E1/Data/RawData/Data/")
  data <- readMat(str_c(rawDataPath, "AA_17-05-01_1.mat"))  #The raw multidimensional matrix format
  
  # Work out possible positions in the stream for T1.
  #Is the values in allTargets the positions of the targets in the stream?
  # Indeed, looks like allTargets[1,1] is position of one target in first trial, allTargets[1,2] is position of other target
  targetSP<- data$allTargets[,1] #target1 position each trial
  possibleTargetSP <- sort(unique(targetSP))
  numItemsInStream <- length( data$allLetterOrder[1,1,] )
  #possiblePos <- sort( unique(data$allResponses[,1]) ) #serial position of response1 every trial
  nTargetPos <- length(targetSP)
} else {
  rawDataPath<- file.path("data/")
  #Load one subject to get
  data<- readRDS( file.path(rawDataPath, "alexImportBackwardsPaper2E1.Rdata") ) #.mat file been preprocessed into melted long dataframe
}
#Would be nice to add my helper function to take note of whether counterbalanced

numItemsInStream<- length( data$letterSeq[1,] )  
df<- data
#It seems that to work with dplyr, can't have array field like letterSeq
df$letterSeq<- NULL


library(dplyr)

source( file.path(mixModelingPath,"analyzeOneCondition.R") )

bestEstimates<- analyzeOneCondition(df, numItemsInStream)

#Break data by condition to  send to fitModel
condtnVariableNames <- c("target", "condition") # c("expDegrees")

estimates<- df %>% 
  group_by_(.dots = condtnVariableNames) %>%  #.dots needed when you have a variable containing multiple factor names
  do(analyzeOneCondition(.,numItemsInStream))


# the negative log likelihood of the fitted model.



#Load in Chris-created MATLAB parameter estimates
load(MATLABmixtureModelOutput, verbose=FALSE)
#join into single long dataframe
results_MATLAB<- merge(efficacy.long,latency.long)
results_MATLAB<- merge(results_MATLAB,precision.long)
