library(ggplot2)

#Need to adjust path because Testthat might not work because path gets set to mixtureModeling/tests/
if (basename(getwd()) != "tests") {
  pathNeeded<- "mixtureModeling"
} else { 
  pathNeeded <- ".." 
}
print(getwd())
source( file.path(pathNeeded, "createGuessingDistribution.R") )
source( file.path(pathNeeded, "parameterBounds.R") )
source(file.path(pathNeeded,"calcCurvesDataFrames.R"))
source(file.path(pathNeeded,"theme_apa.R"))


plotHistWithFit<- function(df,minSPE,maxSPE,targetSP,numItemsInStream,plotContinuousGaussian,annotateIt) {
  #targetSP is needed to construct empirical guessing distribution
  #calculate curves (predicted heights of bins for each component and combination of components
  curveDfs<- calcCurvesDataframes(df,minSPE,maxSPE,numItemsInStream) #this also does the parameter estimation

  if (plotContinuousGaussian) {
    #Calculate continuous fitted Gaussian, not discrete version.
    grain<-.05
    numObservations<- length(df$SPE)
    gaussianThis<- gaussianScaledForData(curveDfs$efficacy[1],curveDfs$latency[1],curveDfs$precision[1],
                                         numObservations,minSPE,maxSPE,grain) 
  }
  
  #plot data
  g=ggplot(df, aes(x=SPE)) + theme_apa()
  #plot data
  g<-g+geom_histogram(binwidth=1) + xlim(minSPE,maxSPE)
  if (plotContinuousGaussian) {
    g<-g + geom_line(data=gaussianThis,aes(x=x,y=gaussianFreq),color="darkblue",size=1.2)
  }
  g<-g+ geom_line(data=curveDfs,aes(x=x,y=guessingFreq),color="yellow",size=1.2)
  g<-g+ geom_line(data=curveDfs,aes(x=x,y=gaussianFreq),color="lightblue",size=1.2)
  g<-g+ geom_point(data=curveDfs,aes(x=x,y=combinedFitFreq),color="green",size=1.2)

  if (annotateIt) {
    g<-g + geom_text(data=curveDfs,aes(x=-9,y=32, label = paste("-logLik==", round(val,1), sep = "")), parse=TRUE,hjust="left") +
      geom_text(data=curveDfs,aes(x=-7,y=28, label = paste("plain(e)==", round(efficacy,2), sep = "")),  parse=TRUE,hjust="left") +
      geom_text(data=curveDfs,aes(x=-7,y=25, label = paste("mu==", round(latency,2), sep = "")),  parse=TRUE,hjust="left")+
      geom_text(data=curveDfs,aes(x=-7,y=22, label = paste("sigma==", round(precision,2), sep = "")), parse=TRUE,hjust="left")
  }
  show(g)
  return(g)  
}


# fitAndPlotHist<- function(df,minSPE,maxSPE,numItemsInStream) {
#   #THIS SEEMS OUTDATED BECAUSE FITTING HAPPENS AUTOMATICALLY IF NOT PROVIDED
#   estimates<- analyzeOneCondition(df,numItemsInStream,parameterBounds())
#   g<- plotHistWithFit(df$SPE,minSPE,maxSPE,df$targetSP,numItemsInStream,estimates$p1,estimates$p2,estimates$p3)
#   return( list(estimates=estimates, plot=g) )
# }



  