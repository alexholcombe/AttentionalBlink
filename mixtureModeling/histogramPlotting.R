library(ggplot2)

#Need to adjust path because Testthat might not work because path gets set to mixtureModeling/tests/
if (basename(getwd()) != "tests") {
  pathNeeded<- "mixtureModeling"
} else { 
  pathNeeded <- ".." 
}
source( file.path(pathNeeded, "createGuessingDistribution.R") )
source( file.path(pathNeeded, "parameterBounds.R") )
source(file.path(pathNeeded,"theme_apa.R"))


plotHistWithFit<- function(df,minSPE,maxSPE,targetSP,numItemsInStream) {
  #targetSP is needed to construct empirical guessing distribution
  
  curveDfs<- calcCurvesDataframes(df,minSPE,maxSPE,numItemsInStream)
  
  #plot data
  g=ggplot(df, aes(x=SPE)) + theme_apa()
  #plot data
  g<-g+geom_histogram(binwidth=1) + xlim(minSPE,maxSPE)
  g<-g+ geom_line(data=curveDfs,aes(x=x,y=guessingFreq),color="yellow",size=1.2)
  g<-g+ geom_line(data=curveDfs,aes(x=x,y=gaussianFreq),color="lightblue",size=1.2)
  g<-g+ geom_point(data=curveDfs,aes(x=x,y=combinedFitFreq),color="green",size=1.2)
  g 
  
  return(g)  
}

# plotHistWithFit<- function(SPE,minSPE,maxSPE,targetSP,numItemsInStream,efficacy,latency,precision) {
#   #targetSP is needed to construct empirical guessing distribution
#   
#   g=ggplot(df, aes(x=SPE)) 
#   
#   #plot data
#   g<-g+geom_histogram(binwidth=1) + xlim(minSPE,maxSPE)
# 
#   #plot Gaussian and guessing distribution
#   pseudoUniform <- createGuessingDistribution(minSPE,maxSPE,targetSP,numItemsInStream)
#   #unitise it
#   pseudoUniform<-pseudoUniform/sum(pseudoUniform)
#   #calculate points at appropriate height for this data
#   guessingThis<- (1-efficacy) * pseudoUniform * length(df$SPE)
#   guessingThis<-data.frame(x=minSPE:maxSPE, SPE=guessingThis)
#   g<-g+ geom_line(data=guessingThis,aes(x=x,y=SPE),color="yellow",size=1.2)
# 
#   #calculate fitted Gaussian distribution
#   grain<-0.1
#   domain<-seq(minSPE,maxSPE,grain)
#   #Calculate fitted points at appropriate height for this data
#   gaussianThis<- gaussianScaledForData(efficacy,latency,precision,length(SPE),minSPE,maxSPE,grain) 
#   g<-g+ geom_line(data=gaussianThis,aes(x=x,y=SPE),color="lightblue",size=1.2)
# 
#   #Calculate sum
#   #Need the quantized Gaussian
#   grain<-1
#   gaussianThis<- gaussianScaledForData(efficacy,latency,precision,length(SPE),minSPE,maxSPE,grain) 
#   combined<-guessingThis
#   combined$SPE <- combined$SPE + gaussianThis$gaussianFreq
#   g<-g+ geom_point(data=combined,aes(x=x,y=SPE),color="green",size=1.7)
#   return(g)  
# }


# fitAndPlotHist<- function(df,minSPE,maxSPE,numItemsInStream) {
#   #THIS SEEMS OUTDATED BECAUSE FITTING HAPPENS AUTOMATICALLY IF NOT PROVIDED
#   estimates<- analyzeOneCondition(df,numItemsInStream,parameterBounds())
#   g<- plotHistWithFit(df$SPE,minSPE,maxSPE,df$targetSP,numItemsInStream,estimates$p1,estimates$p2,estimates$p3)
#   return( list(estimates=estimates, plot=g) )
# }



  