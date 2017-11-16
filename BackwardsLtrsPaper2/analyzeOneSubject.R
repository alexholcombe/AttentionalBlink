
pathNeeded<-"mixtureModeling"
source(file.path(pathNeeded,"analyzeOneCondition.R"))
source(file.path(pathNeeded,"parameterBounds.R"))
source(file.path(pathNeeded,"calcCurvesDataframes.R"))
source(file.path(pathNeeded,"theme_apa.R"))

fileWithPath<- file.path("data","P2E2_PilotData.Rdata") 

if (file.exists(fileWithPath)) {
  dg<- readRDS( fileWithPath ) #.mat file been preprocessed into melted long dataframe
} else {
  print("Error! Could not find file. Probably your working directory is bad")
}


library(dplyr)
library(testthat)

library(dplyr)
numItemsInStream<- length( dg$letterSeq[1,] )  
#It seems that to work with dplyr, can't have array field like letterSeq
dg$letterSeq<- NULL
CB11 <- dg %>% dplyr::filter(subject=="CB" & target==1 & condition==1)
#Rather low efficacy dataset
estimates<- analyzeOneCondition(CB11,numItemsInStream,parameterBounds())
#print(estimates) # p1=.28, p2=.07, p3=.75

expect_that( estimates$warnings == "None", is_true() )

minSPE<- -11; maxSPE<- 11

curveDfs<- calcCurvesDataframes(CB11,minSPE,maxSPE,numItemsInStream)

g=ggplot(CB11, aes(x=SPE)) + theme_apa()
#plot data
g<-g+geom_histogram(binwidth=1) + xlim(minSPE,maxSPE)
g<-g+ geom_line(data=curveDfs,aes(x=x,y=guessingFreq),color="yellow",size=1.2)
g<-g+ geom_line(data=curveDfs,aes(x=x,y=gaussianFreq),color="lightblue",size=1.2)
g<-g+ geom_point(data=curveDfs,aes(x=x,y=combinedFitFreq),color="green",size=1.2)

show(g)
