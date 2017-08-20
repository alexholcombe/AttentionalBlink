
fitModel <- function(SPEs, minSPE, maxSPE, parameterGuess)
{
  #create function that calculates log likelihood of data given particular parameter values
  pdf_normmixture_single_par <- function(par)
  {
    p <- par[1]
    mu <- par[2]
    sigma <- par[3]
    result <- pdf_Mixture_Single(SPEs, p, mu, sigma, minSPE,maxSPE, pseudoUniform)
    # Sometimes pdf_normmixture_single returns 0. And the log of 0 is -Inf. So we add
    # 1e-8 to make the value we return finite. This allows optim() to successfully
    # optimise the function.
    return(-sum(log(result + 1e-8)))
  }                
  #Traverse parameter space to find parameter values that maximise the log likelihood
  fit <- optim(parameterGuess, pdf_normmixture_single_par, lower=parametersLowerBound, upper=parametersUpperBound,
               control=list(trace=0), method="L-BFGS-B")
  return(fit$par)                    
}

# Alternatively, could use the MLE function.
# [currentEstimates, currentCIs] <- mle(theseT1Error, 'pdf', pdf_normmixture_single, 'start', parameterGuess, 'lower', parameterLowerBound, 'upper', parameterUpperBound, 'options', options)
