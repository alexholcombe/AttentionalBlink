rm(list=ls()) #clear all variables
#random orientation each trial. First experiment of second backwards-letters paper

mixModelingPath<- file.path("mixtureModeling")


MATLABmixtureModelOutputPath<-"~/Google\ Drive/Backwards\ paper/secondPaper/E1/Data/MixModData"
importedToRbyChris <- "allParams.RData"
MATLABmixtureModelOutput<- file.path( MATLABmixtureModelOutputPath, importedToRbyChris )
  

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
  #Load one subject to get
  data<- readRDS( file.path(rawDataPath, "alexImportBackwardsPaper2E1.Rdata") ) #.mat file been preprocessed into melted long dataframe
}
#Would be nice to add my helper function to take note of whether counterbalanced

source( file.path(mixModelingPath,"createGuessingDistribution.R")  )
source( file.path(mixModelingPath,"fitModel.R") )

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

numItemsInStream<- length( data$letterSeq[1,] )  
df<- data
#It seems that to work with dplyr, can't have array field like letterSeq
df$letterSeq<- NULL


library(dplyr)
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
  nReplicates <- 2# 100# Number of times to repeat each fit with different starting values
  fitMaxIter <- 10^4# Maximum number of fit iterations
  fitMaxFunEvals <- 10^4# Maximum number of model evaluations
  
  #Break data by condition to  send to fitModel
  condtnVariableNames <- c("target", "condition") # c("expDegrees")
  
  #Use RT to check which is left target and which is right target
  fitModelDF <- function( SPE, minSPE, maxSPE ) {
    #Calculate parameter guess
    #I THINK YOU'RE ALLOWED TO SEND ADDITIONAL PARAMS WITH DDPLY
    startingParams<- parametersGuess( parametersLowerBound, parametersUpperBound )
    fit<- fitModel(SPE, minSPE, maxSPE, pseudoUniform, startingParams)
    params<- fit$params
    return( data.frame(efficay=params[1], latency=params[2], precision=params[3], val=fit$value) )
  }
  lowestVal<-999999999
  bestFitN<- 1
  for (n in 1:nReplicates) {
    ans<- fitModelDF( df$SPE, minSPE, maxSPE )
    if (ans$val < lowestVal) {
      bestFitN <- n
    }
  }
}

analyzeOneCondition(df, numItemsInStream)

# Compute the negative log likelihood of the fitted model.
thisNegLogLikelihood <- -sum(log(pdf_normmixture_single(theseT1Error,currentEstimates[1],currentEstimates[2],currentEstimates[3])))

# Check whether this is lower than the lowest so far.
if (minNegLogLikelihood > thisNegLogLikelihood){
  
  # If so, store this as the current best estimate.
  minNegLogLikelihood <- thisNegLogLikelihood
  bestEstimates <- currentEstimates
  # bestEstimateCIs <- currentCIs
  
}

data %>% 
  group_by_(.dots = condtnVariableNames) %>%  #.dots needed when you have a variable containing multiple factor names
  do(fitModelDF(.$SPE,minSPE,maxSPE))




startingParams<- parametersGuess( parametersLowerBound, parametersUpperBound )
params <- fitModel(data$SPE, minSPE, maxSPE, startingParams)
cat("startingParams=",startingParams,"this estimate params=", params, "\n")



#Load in Chris-created MATLAB parameter estimates
load(MATLABmixtureModelOutput, verbose=FALSE)
#join into single long dataframe
results_MATLAB<- merge(efficacy.long,latency.long)
results_MATLAB<- merge(results_MATLAB,precision.long)
