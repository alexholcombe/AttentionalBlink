#rm(list=ls())
#random orientation each trial. First experiment of second backwards-letters paper

mixModelingPath<- file.path("mixtureModeling")

mixtureModelOutputPath<-"~/Google\ Drive/Backwards\ paper/secondPaper/E1/Data/MixModData"
importedToRbyChris <- "allParams.RData"

#raw data path containing .mat file for each subject
directFromMAT <- FALSE #.mat file is in particular format
if (directFromMAT) {
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
  #Load one subject
  data<- readRDS( file.path(rawDataPath, "data/alexImportBackwardsPaper2E1.Rdata") ) #.mat file been preprocessed into melted long dataframe
  possibleTargetSP<- sort(unique(data$targetSP))
  numItemsInStream<- length( data$letterSeq[1,] )  
}
  
source( file.path(mixModelingPath,"pdf_Mixture_Single.R") ) 


condtnVariableNames <- c("expDegrees")

if (directFromMAT) {

}
# Calculate the domain of possible serial position errors.
minTargetSP <- min(possibleTargetSP)
maxTargetSP <- max(possibleTargetSP)
minSPE <- 1 - maxTargetSP
maxSPE <- numItemsInStream - minTargetSP

#Would be nice to add my helper function to take note of whether counterbalanced
source( file.path(mixModelingPath,"createGuessingDistribution.R")  )

pseudoUniform <- createGuessingDistribution(minSPE,maxSPE,data$targetSP,numItemsInStream)

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


# Set some model-fitting parameters.
nReplicates <- 100# Number of times to repeat each fit with different starting values
fitMaxIter <- 10^4# Maximum number of fit iterations
fitMaxFunEvals <- 10^4# Maximum number of model evaluations



runif(min=

pGuess<- runif( min=parametersLowerBound

pGuess <- runif(1)
pGuess<- max( c(smallNonZeroNumber, pGuess ) )

muGuess<- runif(min=-1,max=1) * muBound #random number between -muBound and muBound

sigmaGuess<- runif(1)*sigmaBound
sigmaGuess<- 
  
sigmaGuess <- sigmaBound*runif(1)+smallNonZeroNumber
# Compile to feed into the MLE function.
parameterGuess <- c(pGuess, muGuess, sigmaGuess)
cat("parameterGuess", parameterGuess, "\n")
#

# Randomise starting values for each parameter.
parameterGuess<- rep(0,3)
for (i in 1:length(parameterGuess)) {
  parameterGuess[i] <- runif(n=1, min=parametersLowerBound[i], max=parametersUpperBound[i] ) 
}
source( file.path(mixModelingPath,"fitModel.R") )
parameterGuess
params <- fitModel(data$SPE, parameterGuess)
cat("this estimate params=", params, "\n")

#fitOnce <- function(
