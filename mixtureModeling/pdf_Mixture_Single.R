library(signal) # Provides interp1 function
# interp1 <- function(x, v, xq)
# {
#   cat("x ", x, " v ", v, " xq ", xq, "\n")
#   x2 <- x[order(x)]
#   v2 <- v[order(x)]
#   
#   vq <- rep(0, length(xq))
#   for (idx in 1:length(xq)) {
#     i <- 1
#     while (xq[idx] >= x2[i]) i = i + 1
# 
#     # Now x[i - 1] < xq and x[i] >= xq
#     m <- (v2[i] - v2[i - 1]) / (x2[i] - x2[i - 1])
#     vq[idx] <- v2[i - 1] + m * (xq[idx] - x2[i - 1])
#   }
# 
#   return(vq)
# }

pdf_Mixture_Single <- function(x,p,mu,sigma,minSPE,maxSPE,guessingDistribution){
    
    xDomain <- minSPE:maxSPE
    pseudo_normal <- dnorm(xDomain,mu,sigma)*guessingDistribution
    #cat("pseudo_normal ", pseudo_normal, "\n")
    
    #normalising factors
    normFactor_uniform <- sum(guessingDistribution)
    normFactor_normal <- sum(pseudo_normal)

    if (normFactor_uniform  == 0 || is.nan(normFactor_uniform)){
        normFactor_uniform <- 10^-8
    }

    if (normFactor_normal == 0 || is.nan(normFactor_normal)){
        normFactor_normal <- 10^-8
    }

    #For all SPEs, determine the height of the guessingDistribution
    #Use interpolate in case there is a rounding problem? Alex doesn't understand why interp used
    uniResultTemp <- interp1(xDomain, guessingDistribution, x)
    uniResultTemp[is.na(uniResultTemp)] <- 0

    #Multiply Gaussian density by guessing density
    normResultTemp <- dnorm(x,mu,sigma) * uniResultTemp

    #normalize
    uniResultTemp <- uniResultTemp/normFactor_uniform
    normResultTemp <- normResultTemp/normFactor_normal

    propNorm <- p
    propUniform <- 1-p

    normResult <- propNorm * normResultTemp
    uniResult <-  propUniform * uniResultTemp

    if (sum(length(normResult)==length(uniResult))==2){
        result <- normResult+uniResult
    } else {
        result <- normResult+t(uniResult)
    }

    #xIndex = x-min(xDomain)+1;
    #results = tempResult(xIndex);
    # cat("tempResult ", tempResult, "\n")
    #tempResult <- tempResult[1]
    return(result)
}
