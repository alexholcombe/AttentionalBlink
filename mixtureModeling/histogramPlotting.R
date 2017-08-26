library(ggplot2)

gaussianScaledForData<- function(efficacy,latency,precision,SPE,minSPE,maxSPE,grain) {
  domain<-seq(minSPE,maxSPE,grain)
  gaussianThis<- dnorm(domain,latency,precision)
  #Calculate points at appropriate height fot this data
  gaussianThis<- gaussianThis * efficacy * length(SPE)
  gaussianThis<-data.frame(x=domain, SPE=gaussianThis)
  return(gaussianThis)
}

#Need to adjust path because Testthat might not work because path gets set to mixtureModeling/tests/
if (basename(getwd()) != "tests") {
  pathNeeded<- "mixtureModeling"
} else { 
  pathNeeded <- ".." 
}
source( file.path(pathNeeded, "createGuessingDistribution.R") )
                  
plotHistWithFit<- function(SPE,minSPE,maxSPE,targetSP,numItemsInStream,efficacy,latency,precision) {
  #targetSP is needed to construct empirical guessing distribution
  
  g=ggplot(df, aes(x=SPE)) 
  
  #plot data
  g<-g+geom_histogram(binwidth=1) + xlim(minSPE,maxSPE)

  #plot Gaussian and guessing distribution
  pseudoUniform <- createGuessingDistribution(minSPE,maxSPE,targetSP,numItemsInStream)
  print(pseudoUniform)
  #unitise it
  pseudoUniform<-pseudoUniform/sum(pseudoUniform)
  #calculate points at appropriate height for this data
  guessingThis<- (1-efficacy) * pseudoUniform * length(df$SPE)
  guessingThis<-data.frame(x=minSPE:maxSPE, SPE=guessingThis)
  g<-g+ geom_line(data=guessingThis,aes(x=x,y=SPE),color="yellow",size=1.2)

  #calculate fitted Gaussian distribution
  grain<-0.1
  domain<-seq(minSPE,maxSPE,grain)
  #Calculate fitted points at appropriate height for this data
  gaussianThis<- gaussianScaledForData(efficacy,latency,precision,SPE,minSPE,maxSPE,grain) 
  g<-g+ geom_line(data=gaussianThis,aes(x=x,y=SPE),color="lightblue",size=1.2)

  #Calculate sum
  #Need the quantized Gaussian
  grain<-1
  gaussianThis<- gaussianScaledForData(efficacy,latency,precision,SPE,minSPE,maxSPE,grain) 
  combined<-guessingThis
  combined$SPE <- combined$SPE + gaussianThis$SPE
  g<-g+ geom_point(data=combined,aes(x=x,y=SPE),color="green",size=1.7)
  return(g)  
}

fitAndPlotHist<- function(df,minSPE,maxSPE,numItemsInStream) {
  
  estimates<- analyzeOneCondition(df,numItemsInStream)
  plotHistWithFit(df$SPE,minSPE,maxSPE,df$targetSP,numItemsInStream,estimates$p1,estimates$p2,estimates$p3)
  return(estimates)
}

#Use for for ggplot so can split and 
calcFitDataframes<- function(df,minSPE,maxSPE,numItemsInStream) {
  #Calculate dataframes containing the fitted curve, so can plot data and curves at same time
  #User can optionally supply estimates, otherwise need to do the fitting
  if ( "efficacy" %in% names(df) ) { #user supplied efficacy, latency, precision
    efficacy<- df$efficacy[1]
    latency<- df$latency[1]; precision<- df$precision[1]
    if ( "val" %in% names(df) ) { #likelihood, preserve
      val <- df$val[1]
    }
  } else {
    estimates<- analyzeOneCondition(df,numItemsInStream)
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
  gaussianThis<- gaussianScaledForData(efficacy,latency,precision,df$SPE,minSPE,maxSPE,grain) 
  fitDFs$gaussianFreq<- gaussianThis$SPE
  
  fitDFs$combinedFitFreq<- fitDFs$gaussianFreq + fitDFs$guessingFreq
  
  return( fitDFs )
}

  