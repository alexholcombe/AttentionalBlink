
#To be used with ggplot. Idea is to split dataframe into conditions,
# send it to calcCurvesDataframes to fit data if not already fit but definitely
# calculate the curves.

calcCurvesDataframes<- function(df,minSPE,maxSPE,numItemsInStream) {
  #Calculate dataframes containing the fitted curve, so can plot data and curves at same time
  #User can optionally supply estimates, otherwise need to do the fitting
  if ( "efficacy" %in% names(df) ) { #user supplied efficacy, latency, precision
    efficacy<- df$efficacy[1]
    latency<- df$latency[1]; precision<- df$precision[1]
    if ( "val" %in% names(df) ) { #likelihood, preserve
      val <- df$val[1]
    }
  } else {
    estimates<- analyzeOneCondition(df,numItemsInStream,parameterBounds())
    efficacy<-estimates$p1; latency<-estimates$p2; precision<-estimates$p3
    val<- estimates$val
  }
  
  #create guessing distribution
  pseudoUniform <- createGuessingDistribution(minSPE,maxSPE,df$targetSP,numItemsInStream)
  #unitise it
  pseudoUniform<-pseudoUniform/sum(pseudoUniform)
  #calculate points at appropriate height for this data
  guessingThis<- (1-efficacy) * pseudoUniform * length(df$SPE)
  fitDFs<-data.frame(x=minSPE:maxSPE, 
                     guessingFreq=guessingThis,
                     efficacy=efficacy, latency=latency, precision=precision, val=val) 
  #Calculate Gaussian and sum
  #Need the quantized Gaussian
  grain<-1
  #print(paste0("efficacy=",efficacy,"latency=",latency,"precision=",precision))
  #print(paste0("minSPE=",minSPE,"maxSPE=",maxSPE))
  #print(paste0("df$SPE=",df$SPE))
  gaussianThis<- gaussianScaledForData(efficacy,latency,precision,df$SPE,minSPE,maxSPE,grain) 
  #print(gaussianThis)
  fitDFs$gaussianFreq<- gaussianThis$SPE
  
  fitDFs$combinedFitFreq<- fitDFs$gaussianFreq + fitDFs$guessingFreq
  
  return( fitDFs )
}