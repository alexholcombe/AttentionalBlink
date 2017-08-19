#rm(list=ls())
#random orientation each trial. First experiment of second backwards-letters paper

mixtureModelOutputPath<-"~/Google\ Drive/Backwards\ paper/secondPaper/E1/Data/MixModData"
importedToRbyChris <- "allParams.RData"

#raw data path containing .mat file for each subject
rawDataPath<- "~/Google\ Drive/Backwards\ paper/secondPaper/E1/Data/RawData/Data/"

source("pdf_Mixture_Single.R") 

#Load one subject
#data <- readMat(str_c(rawDataPath, "AA_17-05-01_1.mat"))  #The raw multidimensional matrix format
data<- readRDS("data/alexImportBackwardsPaper2E1.Rdata") #.mat file been preprocessed into melted long dataframe

condtnVariableNames <- c("expDegrees")

directFromMAT <- FALSE
if (directFromMAT) {
  # Work out possible positions in the stream for T1.
  #Is the values in allTargets the positions of the targets in the stream?
  # Indeed, looks like allTargets[1,1] is position of one target in first trial, allTargets[1,2] is position of other target
  targetSP<- data$allTargets[,1] #target1 position each trial
  possibleTargetSP <- sort(unique(targetSP))
  numItemsInStream <- length( data$allLetterOrder[1,1,] )
  #possiblePos <- sort( unique(data$allResponses[,1]) ) #serial position of response1 every trial
  nTargetPos <- length(targetSP)
} else {
  possibleTargetSP<- sort(unique(data$targetSP))
  numItemsInStream<- length( data$letterSeq[1,] )
}
# Calculate the domain of possible errors (xDomain).
minTargetSP <- min(possibleTargetSP)
maxTargetSP <- max(possibleTargetSP)
minSPE <- 1 - maxTargetSP
maxSPE <- numItemsInStream - minTargetSP
xDomain<- minSPE:maxSPE

#Would be nice to add my helper function to take note of whether counterbalanced
  
createGuessingDistribution<- function(minSPE,maxSPE,targetSP,numItemsInStream) {
  # Generate the 'pseudo-uniform' distribution, which is the expected distribution of errors if a random guess was
  # provided on every trial. This isn't an actual uniform distribution because the most extreme errors are only
  # possible on trials in which targets appear at their most extreme positions.
  xDomain<- minSPE:maxSPE
  pseudoUniform <- matrix(xDomain, ncol=2,nrow=35, byrow=FALSE)
  #first column will be SPE. Second column will be expected frequency of that SPE from guessing
  pseudoUniform[,2] <-0
  # Cycle through each possible T1 position.
  # Will use empirical T1 positions (doesn't assume was perfectly counterbalanced), because that yields the objective expectation
  # if you simply guess on every trial.
  for (i in 1:length( targetSP )) {  
    #for (i in 1:length(possibleT1Pos)){ #theoretical  (assumes each possible position occurred equally often)
    
    thisPos <- data$targetSP[i]   #For example, the first position number might be the 7th position in the stream.
    
    # Add to the pseudo-uniform distribution one unit for every
    # possible error given that T1 position.
    
    #what's the min and max SPE corresponding to this T1 position
    minSPEthis<- thisPos - numItemsInStream
    maxSPEthis<- thisPos - 1
    #where does this fit in to the entire range of possible SPEs
    minSPEthisRel<- minSPEthis - minSPE
    maxSPEthisRel<- maxSPEthis - minSPE
    
    
    minThis <- 1-thisPos-minSPE+1
    maxThis <- numItemsInStream-thisPos-minSPE+1
    pseudoUniform[minThis:maxThis,2] = pseudoUniform[minThis:maxThis,2] + 1
  }
  #Only want second column, not labels
  pseudoUniform<- pseudoUniform[,2]
  
  #Pat padded with extra zero for some reason. And only want second column, not labels
  pad<-FALSE
  if (pad) {
    pseudoUniform<- c(0, pseudoUniform, 0)
  }
  return(pseudoUniform)
  #length(pseudoUniform)#pseudoUniform is 37 long, padded or 35 unpadded
  #Needs to match up with
  #length(dnorm(xDomain,muGuess,sigmaGuess)) #35 long, which makes sense. -17->17
  
}
pseudoUniform <- createGuessingDistribution(minSPE,maxSPE,data$targetSP,numItemsInStream)

# Set some parameter bounds. Pat apparently found these were needed to 
# prevent over-fitting to blips in the distributions. These
# values are about right in most cases, but might need some tweaking if
# e.g. you were analysing data with an unusually high or low item rate.
muBound <- 4   #will only consider -4 to +4 for mu 
sigmaBound <- 4 #will only consider 0 to 4 for sigma

smallNonZeroNumber <- 10^-5# Useful number for when limits can't be exactly zero but can be anything larger
                          #efficacy,          latency,    precision
parametersLowerBound <- c(smallNonZeroNumber, -muBound, smallNonZeroNumber)
parametersUpperBound <- c(1,                   muBound, sigmaBound)

# Run the MLE function.
# [currentEstimates, currentCIs] <- mle(theseT1Error, 'pdf', pdf_normmixture_single, 'start', parameterGuess, 'lower', parameterLowerBound, 'upper', parameterUpperBound, 'options', options)
fitModel <- function(SPEs, parameterGuess)
{
  pdf_normmixture_single_par <- function(par)
  {
    p <- par[1]
    mu <- par[2]
    sigma <- par[3]
    result <- pdf_Mixture_Single(SPEs, p, mu, sigma)
    # Sometimes pdf_normmixture_single returns 0. And the log of 0 is -Inf. So we add
    # 1e-8 to make the value we return finite. This allows optim() to successfully
    # optimise the function.
    return(-sum(log(result + 1e-8)))
  }                
  fit <- optim(parameterGuess, pdf_normmixture_single_par, lower=parametersLowerBound, upper=parametersUpperBound,
               control=list(trace=0), method="L-BFGS-B")
  return(fit$par)                    
}

# Set some model-fitting parameters.
nReplicates <- 100# Number of times to repeat each fit with different starting values
smallNonZeroNumber <- 10^-5# Useful number for when limits can't be exactly zero but can be anything larger
fitMaxIter <- 10^4# Maximum number of fit iterations
fitMaxFunEvals <- 10^4# Maximum number of model evaluations

# Randomise starting values for each parameter.
# Set some parameter bounds. Pat apparently found these were needed to 
# prevent over-fitting to blips in the distributions. These
# values are about right in most cases, but might need some tweaking if
# e.g. you were analysing data with an unusually high or low item rate.
muBound <- 4   #will only consider -4 to +4 for mu 
sigmaBound <- 4 #will only consider 0 to 4 for sigma

pGuess <- max(c(smallNonZeroNumber, runif(1)))
muGuess <- (2*muBound*runif(1))-muBound
sigmaGuess <- sigmaBound*runif(1)+smallNonZeroNumber

# Compile to feed into the MLE function.
parameterGuess <- c(pGuess, muGuess, sigmaGuess)

cat("parameterGuess", parameterGuess, "\n")
params <- fitModel(data$SPE, parameterGuess)
cat("this estimate params=", params, "\n")

