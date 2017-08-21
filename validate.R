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
#https://stackoverflow.com/questions/17185829/check-that-all-combinations-occur-equally-often-for-specified-columns-of-a-dataf/17185830#17185830


numItemsInStream<- length( data$letterSeq[1,] )  
df<- data
#It seems that to work with dplyr, can't have array field like letterSeq
df$letterSeq<- NULL

source('mixtureModeling/checkCounterbalancing.R')
checkAllGroupsOccurEquallyOften(df,c("subject","target","condition"),dropZeros=FALSE,verbose=TRUE) 

#checkAllGroupsOccurEquallyOften(df,c("targetSP","target","condition","subject"),dropZeros=FALSE,verbose=TRUE) 
#targetSP is not counterbalanced. Instead, it's random

library(dplyr)

source( file.path(mixModelingPath,"analyzeOneCondition.R") )


#Break data by condition to  send to fitModel
condtnVariableNames <- c("subject","target", "condition") # c("expDegrees")

estimates<- df %>% filter(subject=="AA") %>%
  group_by_(.dots = condtnVariableNames) %>%  #.dots needed when you have a variable containing multiple factor names
  do(analyzeOneCondition(.,numItemsInStream))

estimates<-data.frame(estimates)
estimates
#round all numeric field for easy reading
data.frame(lapply(estimates, function(y) if(is.numeric(y)) round(y, 2) else y)) 


#improve the column names, to match MATLAB column names
#mutate target to Stream
names(estimates)[names(estimates) == 'target'] <- 'stream'
estimates <- estimates %>% mutate( stream =ifelse(stream==1, "Left","Right") )
#mutate condition to Orientation
names(estimates)[names(estimates) == 'condition'] <- 'orientation'
estimates <- estimates %>% mutate( orientation =ifelse(orientation==1, "Canonical","Inverted") )

#Load in Chris-created MATLAB parameter estimates
load(MATLABmixtureModelOutput, verbose=FALSE)
#join into single long dataframe
results_MATLAB<- merge(efficacy.long,latency.long)
results_MATLAB<- merge(results_MATLAB,precision.long)
results_MATLAB<- data.frame(results_MATLAB)
names(results_MATLAB) <- tolower(names(results_MATLAB)) #lower case the column names
results_MATLAB<-results_MATLAB %>% mutate(efficacy=efficacy/100,latency=latency/100,precision=precision/100)

#Compare MATLAB parameter estimates to R
#I suppose by 
merged<- merge(estimates,results_MATLAB)  
merged<- merged %>% mutate( effDiff = p1-efficacy, latDiff= p2-latency, preDiff= p3-precision )

#show differences columns only
diffs<- select(merged, ends_with("Diff"))
round(diffs,3)*100

#calc discrepancies
meanDiscrepancy<- summarise_all(abs(diffs),mean)
biasDiff<- summarise_all(diffs,mean)
meanDiscrepancy
biasDiff

#To-do print out which subjects,conditions crapping out on

