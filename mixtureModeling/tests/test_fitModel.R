# test of fitModel intended to be used with the testthat package

#To testthat, run test_file("mixtureModeling/tests/test_fitModel.r")
#Compensate for path getting set to mixtureModeling/tests/
print(getwd())
if (basename(getwd()) != "tests") {
  pathNeeded<- "mixtureModeling"
} else { 
  pathNeeded <- ".." 
}
source(file.path(pathNeeded,"fitModel.R"))

test_that("Decent estimates", {
  
  df<-readRDS(file="exampleSubject.Rdata")
  library(dplyr)
  df<- filter(df, condition==1 & target==1)
  
  startingParams<- parametersGuess( parametersLowerBound, parametersUpperBound )

  possibleTargetSP<- sort(unique(df$targetSP))
  minTargetSP <- min(possibleTargetSP)
  maxTargetSP <- max(possibleTargetSP)
  minSPE <- 1 - maxTargetSP
  maxSPE <- numItemsInStream - minTargetSP
  numItemsInStream<-24
  #calculate the guessing distribution, empirically (based on actual targetSP)
  pseudoUniform <- createGuessingDistribution(minSPE,maxSPE,df$targetSP,numItemsInStream)
  
  fit<- fitModel(df$SPE, minSPE, maxSPE, pseudoUniform, startingParams)
  fit<- fit$content 
  warns<- fit$warnings
  print(fit)
  
  #Check that standard fit method gives decent results
  expectedParamEstimates<- c(.84,.48,.99) # c(.37,1.2,.017)  #from L-BFGS-B
  LBFGSBparams<-  fit["L-BFGS-B",]
  discrepancy <- LBFGSBparams[1:3] - expectedParamEstimates
  discrepancyLow <- all( abs( discrepancy ) < .1 )
  expect_that( discrepancyLow, is_true() )
  
  # A kkt1 of True means that the final gradient was close to 0 (the optimizer found an extremum),
  #a kkt2 of True means that the Hessian is positive definite (it's a minimum). Both should be True.
  expect_that( fit["L-BFGS-B","kkt1"], is_true() ) #means that the final gradient was close to 0 (the optimizer found an extremum)
}    
)

test_that("Fits entire experiment worth fine", {
  
  
  
  
  
  
  
}
)