pathThis<-"mixtureModeling/"  #From directory of this file, which should be mixtureModeling/

source( file.path(pathThis,"createGuessingDistribution.R")  )
source( file.path(pathThis,"fitModel.R") )

# Set PARAMETER BOUNDS. Pat apparently found these were needed to 
# prevent over-fitting to blips in the distributions. These
# values are about right in most cases, but might need some tweaking if
# e.g. you were analysing data with an unusually high or low item rate.
muBound <- 4   #will only consider -4 to +4 for mu 
sigmaBound <- 4 #will only consider 0 to 4 for sigma

smallNonZeroNumber <- 10^-5# Useful number for when limits can't be exactly zero but can be anything larger
#efficacy,          latency,    precision
parametersLowerBound <- c(smallNonZeroNumber, -muBound, smallNonZeroNumber)
parametersUpperBound <- c(1,                   muBound, sigmaBound)
# END PARAMETER BOUNDS

# Randomise starting values for each parameter.
parametersGuess<- function( parametersLowerBound, parametersUpperBound ) {
  guess<- rep(0,3)
  for (i in 1:length(guess)) {
    #random value between min and max possible value
    guess[i] <- runif(n=1, min=parametersLowerBound[i], max=parametersUpperBound[i] ) 
  } 
  return (guess)
}


analyzeOneCondition<- function(df, numItemsInStream) {
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
    startingParams<- parametersGuess( parametersLowerBound, parametersUpperBound )
    fit<- fitModel(SPE, minSPE, maxSPE, pseudoUniform, startingParams)
    fit<- fit$content
    warns<- fit$warnings
    print(warns)
    print(fit)
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


#bestEstimates<- analyzeOneCondition(df, numItemsInStream)
