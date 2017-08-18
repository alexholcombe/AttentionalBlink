
#random orientation each trial. First experiment of second backwards-letters paper

mixtureModelOutputPath<-"~/Google\ Drive/Backwards\ paper/secondPaper/E1/Data/MixModData"
importedToRbyChris <- "allParams.RData"

#raw data path containing .mat file for each subject
rawDataPath<- "~/Google\ Drive/Backwards\ paper/secondPaper/E1/Data/RawData/Data/"

source("pdf_Mixture_Single.R") 
pdf_normmixture_single <- pdf_Mixture_Single

#Load one subject
#data <- readMat(str_c(rawDataPath, "AA_17-05-01_1.mat"))  #The raw multidimensional matrix format
data<- readRDS("data/alexImportBackwardsPaper2E1.Rdata") #.mat file been preprocessed into melted long dataframe

condtnVariableNames <- c("expDegrees")

# Work out possible positions in the stream for T1.
#Is the values in allTargets the positions of the targets in the stream?
# Indeed, looks like allTargets[1,1] is position of one target in first trial, allTargets[1,2] is position of other target
str(data$allTargets)
unique(data$allTargets)
target1Pos<- data$allTargets[,1] #target1 position each trial

possibleT1Pos <- sort(unique(target1Pos))

numItemsInStream <- length( data$allLetterOrder[1,1,] )
#possiblePos <- sort( unique(data$allResponses[,1]) ) #serial position of response1 every trial

nT1Pos <- length(listT1Pos)


# Calculate the domain of possible errors (xDomain).
minT1pos <- min(possibleT1Pos)
maxT1pos <- max(possibleT1Pos)
minSPE <- 1 - maxT1pos #-1
maxSPE <- numItemsInStream - minT1pos #+1
xDomain<- minSPE:maxSPE
  
# Generate the 'pseudo-uniform' distribution, which is the expected distribution of errors if a random guess was
# provided on every trial. This isn't an actual uniform distribution because the most extreme errors are only
# possible on trials in which targets appear at their most extreme positions.
pseudoUniform <- matrix(xDomain, ncol=2,nrow=35, byrow=FALSE)
pseudoUniform[,2] <-0
# Cycle through each possible T1 position.
for (i in 1:length( target1Pos )) {  #empirical T1 positions (doesn't assume was perfectly counterbalanced)
#for (i in 1:length(possibleT1Pos)){ #theoretical  (assumes each possible position occurred equally often)

  thisPos <- target1Pos[i]   #For example, the first position number might be the 7th position in the stream.
  
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
#Have to pad with extra zero


# Run the MLE function.
# [currentEstimates, currentCIs] <- mle(theseT1Error, 'pdf', pdf_normmixture_single, 'start', parameterGuess, 'lower', parameterLowerBound, 'upper', parameterUpperBound, 'options', options)
fitModel <- function(SPEs, parameterGuess)
{
  pdf_normmixture_single_par <- function(par)
  {
    p <- par[1]
    mu <- par[2]
    sigma <- par[3]
    result <- pdf_normmixture_single(SPEs, p, mu, sigma)
    # Sometimes pdf_normmixture_single returns 0. And the log of 0 is -Inf. So we add
    # 1e-8 to make the value we return finite. This allows optim() to successfully
    # optimise the function.
    return(-sum(log(result + 1e-8)))
  }                
  fit <- optim(parameterGuess, pdf_normmixture_single_par, lower=parameterLowerBound, upper=parameterUpperBound, control=list(trace=0), method="L-BFGS-B")
  return(fit$par)                    
}

SPEs<- data$allResponses[,1] - data$allTargets[

fitModel(
