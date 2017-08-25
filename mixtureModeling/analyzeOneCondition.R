
#Compensate for path getting set to mixtureModeling/tests/ by testthat
if (basename(getwd()) != "tests") { #directory of this file, which should be mixtureModeling/
  pathNeeded<- "mixtureModeling" 
} else { 
  pathNeeded <- ".." 
}

source( file.path(pathNeeded,"createGuessingDistribution.R")  )
source( file.path(pathNeeded,"fitModel.R") )


source( file.path(pathNeeded,"parametersGuess.R") )


analyzeOneCondition<- function(df, numItemsInStream, paramBounds) {
  # Calculate the domain of possible serial position errors.
  possibleTargetSP<- sort(unique(df$targetSP))
  minTargetSP <- min(possibleTargetSP)
  maxTargetSP <- max(possibleTargetSP)
  minSPE <- 1 - maxTargetSP
  maxSPE <- numItemsInStream - minTargetSP
  
  #calculate the guessing distribution, empirically (based on actual targetSP)
  pseudoUniform <- createGuessingDistribution(minSPE,maxSPE,df$targetSP,numItemsInStream)
  
  # Set some model-fitting parameters.
  nReplicates <- 3# 100# Number of times to repeat each fit with different starting values
  fitMaxIter <- 10^4# Maximum number of fit iterations
  fitMaxFunEvals <- 10^4# Maximum number of model evaluations
  
  #Use RT to check which is left target and which is right target
  fitModelDF <- function( SPE, minSPE, maxSPE ) {
    #Calculate parameter guess
    startingParams<- parametersGuess( paramBounds$lower, paramBounds$upper )
    fit<- fitModel(SPE, minSPE, maxSPE, pseudoUniform, startingParams)
    fit<- fit$content
    warns<- fit$warnings
    #print(fit)
    return( data.frame(efficay=fit[1], latency=fit[2], precision=fit[3], val=fit$value, warnings="None") )
  }
  
  for (n in 1:nReplicates) { #fit the model many times (with different starting parameters)
    
    paramsPlusNegLogLikelihood<- fitModelDF( df$SPE, minSPE, maxSPE )
    #print(paramsPlusNegLogLikelihood)
    #Save the best estimate
    if (n==1) {
      bestEstimate <- paramsPlusNegLogLikelihood
    } else {
      if (paramsPlusNegLogLikelihood$val < bestEstimate$val) 
      { bestEstimate <- paramsPlusNegLogLikelihood }
    }
  }  #End fitting the model many times with different starting params
  
  return( bestEstimate )
}



