
#Compensate for path getting set to mixtureModeling/tests/ by testthat
if (basename(getwd()) != "tests") { #directory of this file, which should be mixtureModeling/
  pathNeeded<- "mixtureModeling" 
} else { 
  pathNeeded <- ".." 
}

source( file.path(pathNeeded,"createGuessingDistribution.R")  )
source( file.path(pathNeeded,"likelihood_mixture.R")  )


likelihoodOneConditionGivenParams<- function(df, numItemsInStream, params) {
  #Note that the likelihood depends on the number of observations. So it'd be unfair to compare the 
  # fit across different datasets. Could divide by the number of observations to calculate
  # the average likelihood but probably probabilities don't work that way.
  possibleTargetSP<- sort(unique(df$targetSP))
  minTargetSP <- min(possibleTargetSP)
  maxTargetSP <- max(possibleTargetSP)
  minSPE <- 1 - maxTargetSP
  maxSPE <- numItemsInStream - minTargetSP
  #calculate the guessing distribution, empirically (based on actual targetSP)
  pseudoUniform <- createGuessingDistribution(minSPE,maxSPE,df$targetSP,numItemsInStream)
  p<- params[[1]]
  mu<- params[[2]]
  sigma<- params[[3]]
  likelihoodEachObservation <- likelihood_mixture(df$SPE, p, mu, sigma, minSPE,maxSPE, pseudoUniform)
  # Sometimes pdf_normmixture_single returns 0. And the log of 0 is -Inf. So we add
  # 1e-8 to make the value we return finite. This allows optim() to successfully
  # optimise the function.
  return(-sum(log(likelihoodEachObservation + 1e-8)))
}

################
likelihoodOneConditionForDplyr<- function(df,numItemsInStream) {
  params<- df[1,c("efficacy","latency","precision")]
  l<- likelihoodOneConditionGivenParams(df, numItemsInStream, params)
  return(data.frame(val=l))
}
