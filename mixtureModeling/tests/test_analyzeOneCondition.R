
#To test, run test_file("mixtureModeling/test_analyzeOneCondition.R")
#But that doesn't work because then the path gets set to mixtureModeling/
print(getwd())
if (basename(getwd()) != "mixtureModeling") {
  pathNeeded<- "mixtureModeling"
} else { 
  pathNeeded <- "." 
}
source(file.path(pathNeeded,"analyzeOneCondition.R"))

df<-readRDS(file.path(pathNeeded,"exampleSubject.Rdata"))
library(dplyr)
df<- filter(df, condition==1 & target==1)

estimates<- analyzeOneCondition(df, 24)
estimates

#Test with a problematic dataset
test_that("Problematic cases", {
  
  data<- readRDS( file.path(pathNeeded, "alexImportBackwardsPaper2E1.Rdata") ) #.mat file been preprocessed into melted long dataframe
  #data<- readRDS( file.path("mixtureModeling/tests", "alexImportBackwardsPaper2E1.Rdata") ) #.mat file been preprocessed into melted long dataframe
  library(dplyr)
  numItemsInStream<- length( data$letterSeq[1,] )  
  data<- data
  #It seems that to work with dplyr, can't have array field like letterSeq
  data$letterSeq<- NULL
  BA22 <- data %>% filter(subject=="BA" & target==2 & condition==2)
  estimates<- analyzeOneCondition(BA22,numItemsInStream)
  #plot histogram
  require(ggplot2)
  df<-BA22
  #sanity check
  minSPE<- -17; maxSPE<- 17
  g=ggplot(df, aes(x=SPE))  
  #g<-g+facet_grid(condName~exp)
  g<-g+geom_histogram(binwidth=1) + xlim(minSPE,maxSPE)
  g
  
  source( file.path(pathNeeded,"histogramPlotting.R") )
  #plot dnorm and guessing distribution
  pseudoUniform <- createGuessingDistribution(minSPE,maxSPE,df$targetSP,numItemsInStream)
  #unitise it
  pseudoUniform<-pseudoUniform/sum(pseudoUniform)
  pseudoUniform
  #calculate points at appropriate height for this data
  guessingThis<- (1-estimates$p1) * pseudoUniform * length(df$SPE)
  guessingThis<-data.frame(x=minSPE:maxSPE, SPE=guessingThis)
  g<-g+ geom_line(data=dg,aes(x=x,y=SPE),color="yellow",size=1.2)
  g
  #calculate fitted Gaussian distribution
  grain<-0.1
  domain<-seq(minSPE,maxSPE,grain)
  gaussianThis<- gaussianScaledForData(estimates$p1,estimates$p2,estimates$p3,df$SPE,minSPE,maxSPE,grain) 
  
  #gaussianThis<- dnorm(domain,estimates$p2,estimates$p3)
  #Calculate points at appropriate height fot this data
  #gaussianThis<- gaussianThis * estimates$p1 * length(df$SPE)
  #gaussianThis<-data.frame(x=domain, SPE=gaussianThis)
  g<-g+ geom_line(data=gaussianThis,aes(x=x,y=SPE),color="blue",size=1.2)
  g
  #Calculate sum
  #Need the quantized Gaussian
  grain<-1
  gaussianThis<- gaussianScaledForData(estimates$p1,estimates$p2,estimates$p3,df$SPE,minSPE,maxSPE,grain) 
  combined<-guessingThis
  combined$SPE <- combined$SPE + gaussianThis$SPE
  g<-g+ geom_point(data=combined,aes(x=x,y=SPE),color="green",size=1.7)
  g  

)


#BO,1,1
estimates %>% filter(round(p1,3)==0.280) #BA,2,2 or BE,2,1
#1e-05  4 1e-05 334.385593546 but didn't end up being the winning replicate
#Inspect AD,2,1 and AI,1,2 because very poor fit
