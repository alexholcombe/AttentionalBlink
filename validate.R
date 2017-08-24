rm(list=ls()) #clear all variables
#random orientation each trial. First experiment of second backwards-letters paper

mixModelingPath<- file.path("mixtureModeling")

#Import MATLAB fits
MATLABmixtureModelOutputPath<-"~/Google\ Drive/Backwards\ paper/secondPaper/E1/Data/MixModData"
importedToRbyChris <- "allParams.RData"
MATLABmixtureModelOutput<- file.path( MATLABmixtureModelOutputPath, importedToRbyChris )
  
#Import raw data
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
  #.mat file been preprocessed into melted long dataframe by backwarsLtrsLoadRawData.R
  data<- readRDS( file.path(rawDataPath, "alexImportBackwardsPaper2E1.Rdata") ) 
}

#Would be nice to add my helper function to take note of whether counterbalanced
#https://stackoverflow.com/questions/17185829/check-that-all-combinations-occur-equally-often-for-specified-columns-of-a-dataf/17185830#17185830

numItemsInStream<- length( data$letterSeq[1,] )  
minSPE<- -17; maxSPE<- 17
df<- data
#It seems that to work with dplyr, can't have array field like letterSeq
df$letterSeq<- NULL


#improve the column names, to match MATLAB column names
#mutate target to Stream
names(df)[names(df) == 'target'] <- 'stream'
df <- df %>% mutate( stream =ifelse(stream==1, "Left","Right") )
#mutate condition to Orientation
names(df)[names(df) == 'condition'] <- 'orientation'
df <- df %>% mutate( orientation =ifelse(orientation==1, "Canonical","Inverted") )


source('mixtureModeling/checkCounterbalancing.R')
checkAllGroupsOccurEquallyOften(df,c("subject","stream","orientation"),dropZeros=FALSE,verbose=TRUE) 

#checkAllGroupsOccurEquallyOften(df,c("targetSP","target","condition","subject"),dropZeros=FALSE,verbose=TRUE) 
#targetSP is not counterbalanced. Instead, it's random

library(dplyr)

source( file.path(mixModelingPath,"analyzeOneCondition.R") )

#Break data by condition to  send to fitModel
condtnVariableNames <- c("subject","stream", "orientation") # c("expDegrees")

estimates<- df %>%    #filter(subject=="AA") %>%
  group_by_(.dots = condtnVariableNames) %>%  #.dots needed when you have a variable containing multiple factor names
  do(analyzeOneCondition(.,numItemsInStream))
estimates<- estimates %>% rename(efficacy = p1, latency = p2, precision = p3)

#round all numeric field for easy reading
source('mixtureModeling/helpers.R')
roundDataframe(estimates,2)

#Cases with mysterious errors, that aren't caught as warnings by optimx somehow
#BO,1,1   #BA,2,2 or BE,2,1
#1e-05  4 1e-05 334.385593546 but didn't end up being the winning replicate
#Inspect AD,2,1 and AI,1,2 because very poor fit

#Load in Chris-created MATLAB parameter estimates
load(MATLABmixtureModelOutput, verbose=FALSE)
#join into single long dataframe
estimates_MATLAB<- merge(efficacy.long,latency.long)
estimates_MATLAB<- merge(estimates_MATLAB,precision.long)
estimates_MATLAB<- data.frame(estimates_MATLAB)
names(estimates_MATLAB) <- tolower(names(estimates_MATLAB)) #lower case the column names
estimates_MATLAB<-estimates_MATLAB %>% mutate(efficacy=efficacy/100,latency=latency/100,precision=precision/100)
#Compare MATLAB parameter estimates to R
estimates_MATLAB<- estimates_MATLAB %>% rename(efficacy_M = efficacy, latency_M = latency, precision_M = precision)
merged<- merge(estimates,estimates_MATLAB)  
merged<- merged %>% mutate( effDiff = efficacy-efficacy_M, latDiff= latency_M-latency, preDiff= precision_M-precision )
#Plot the data with my code using the MATLAB parameters.

#show differences columns only
diffs<- select(merged, ends_with("Diff"))
round(diffs,3)*100
#calc discrepancies
meanDiscrepancy<- summarise_all(abs(diffs),mean)
biasDiff<- summarise_all(diffs,mean)
meanDiscrepancy
biasDiff

source('mixtureModeling/histogramPlotting.R')

#Need to integrate estimates with df so that can plot curve fits without having to re-fit data 
#merge estimates with df

#plot data with R fit
df<- merge(df,estimates) 

df<- df %>% dplyr::filter(subject <="AD")  #dplyr::filter(subject <="AP") #dplyr::filter(subject <= "BD" & subject >="AP")
fitDfs<- df %>% group_by(orientation,stream,subject) %>% 
        do(calcFitDataframes(.,minSPE,maxSPE,numItemsInStream))

quartz(title=tit,width=12,height=6) 
g=ggplot(df, aes(x=SPE)) + facet_grid(orientation~subject+stream)
g<-g+geom_histogram(binwidth=1) + xlim(minSPE,maxSPE)
sz=.3
g<-g+ geom_line(data=fitDfs,aes(x=x,y=guessingFreq),color="yellow",size=sz)
g<-g+ geom_line(data=fitDfs,aes(x=x,y=gaussianFreq),color="lightblue",size=sz)
g<-g+ geom_point(data=fitDfs,aes(x=x,y=combinedFitFreq),color="green",size=sz)
g<-g + theme_bw() +theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank())# hide all gridlines.
numGroups<- length(table(df$orientation,df$subject,df$stream))
fontSz = 80/numGroups
#uses plotmath, not just string interpretation http://ggplot2.tidyverse.org/reference/geom_text.html
g +geom_text(data=fitDfs,aes(x=-8,y=32, label = paste("plain(e)==", round(efficacy,2), sep = "")),  parse = TRUE,size=fontSz) +
  geom_text(data=fitDfs,aes(x=-8,y=27, label = paste("mu==", round(latency,2), sep = "")),  parse = TRUE,size=fontSz)+
  geom_text(data=fitDfs,aes(x=-8,y=23, label = paste("sigma==", round(precision,2), sep = "")),  parse = TRUE,size=fontSz)

#plot data with MATLAB fit
#swap estimate for MATLAB
estimates_M<- estimates_MATLAB %>% rename(efficacy=efficacy_M,latency=latency_M,precision=precision_M)
dg<-merge( select(df,-efficacy,-latency,-precision), estimates_M )
fitDfsM<- dg %>% group_by(orientation,stream,subject) %>% 
  do(calcFitDataframes(.,minSPE,maxSPE,numItemsInStream))


quartz(title="MATLAB",width=13,height=6) 
g=ggplot(df, aes(x=SPE)) + facet_grid(orientation~subject+stream)
g<-g+geom_histogram(binwidth=1) + xlim(minSPE,maxSPE)
sz=.3
g<-g+ geom_line(data=fitDfs,aes(x=x,y=guessingFreq),color="yellow",size=sz)
g<-g+ geom_line(data=fitDfs,aes(x=x,y=gaussianFreq),color="lightblue",size=sz)
g<-g+ geom_point(data=fitDfs,aes(x=x,y=combinedFitFreq),color="green",size=sz)
g<-g + theme_bw() +theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank())# hide all gridlines.
numGroups<- length(table(df$orientation,df$subject,df$stream))
fontSz = 80/numGroups
#uses plotmath, not just string interpretation http://ggplot2.tidyverse.org/reference/geom_text.html
g +geom_text(data=fitDfs,aes(x=-8,y=32, label = paste("plain(e)==", round(efficacy,2), sep = "")),  parse = TRUE,size=fontSz) +
  geom_text(data=fitDfs,aes(x=-8,y=27, label = paste("mu==", round(latency,2), sep = "")),  parse = TRUE,size=fontSz)+
  geom_text(data=fitDfs,aes(x=-8,y=23, label = paste("sigma==", round(precision,2), sep = "")),  parse = TRUE,size=fontSz)


#create R curves
df<- df %>% dplyr::filter(subject <="AD")  #dplyr::filter(subject <="AP") #dplyr::filter(subject <= "BD" & subject >="AP")
fitDfs<- df %>% group_by(orientation,stream,subject) %>% 
  do(calcFitDataframes(.,minSPE,maxSPE,numItemsInStream))

#create MATLAB curves
estimates_M<- estimates_MATLAB %>% rename(efficacy=efficacy_M,latency=latency_M,precision=precision_M)
dg<-merge( select(df,-efficacy,-latency,-precision), estimates_M )
fitsMATLAB<- dg %>% group_by(orientation,stream,subject) %>% 
  do(calcFitDataframes(.,minSPE,maxSPE,numItemsInStream))

#Concatenate R and MATLAB into single dataframe
fitDfs$lang<-"R"; fitsMATLAB$lang<-"MATLAB"
fits_R_MATLAB <- rbind( data.frame(fitDfs), data.frame(fitDfsMATLAB) )
dfBoth<-rbind(   df %>% mutate(lang="R"), df %>% mutate(lang="MATLAB")  )

g=ggplot(dfBoth, aes(x=SPE)) + facet_grid(lang~subject+stream+orientation)
g<-g+geom_histogram(binwidth=1,color="gray77") + xlim(minSPE,maxSPE)
sz=.3
g<-g+ geom_point(data=fits_R_MATLAB,aes(x=x,y=combinedFitFreq),color="chartreuse3",size=sz)
g<-g+ geom_line(data=fits_R_MATLAB,aes(x=x,y=guessingFreq),color="yellow",size=sz)
g<-g+ geom_line(data=fits_R_MATLAB,aes(x=x,y=gaussianFreq),color="lightblue",size=sz)
g<-g + theme_bw() +theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank())# hide all gridlines.
numGroups<- length(table(df$orientation,df$subject,df$stream,df$lang))
fontSz = 80/numGroups
g +geom_text(data=fits_R_MATLAB,aes(x=-8,y=32, label = paste("plain(e)==", round(efficacy,2), sep = "")),  parse = TRUE,size=fontSz) +
  geom_text(data=fits_R_MATLAB,aes(x=-8,y=27, label = paste("mu==", round(latency,2), sep = "")),  parse = TRUE,size=fontSz)+
  geom_text(data=fits_R_MATLAB,aes(x=-8,y=23, label = paste("sigma==", round(precision,2), sep = "")),  parse = TRUE,size=fontSz)

#TODO: Also should compare different-starting-point fit variability to R/MATLAB variability
