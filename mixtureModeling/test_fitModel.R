# test of fitModel intended to be used with the testthat package
#To test, run test_file("mixtureModeling/test_fitModel.r")

#source("mixtureModeling/fitModel.R")
source("fitModel.R")

test_that("Decent estimates", {
  
  df<-readRDS(file="exampleSubject.Rdata")
  library(dplyr)
  df<- filter(df, condition==1 & target==1)
  
  startingParams<- parametersGuess( parametersLowerBound, parametersUpperBound )
  fit<- fitModel(df$SPE, minSPE, maxSPE, startingParams)

  #Check that standard fit method gives decent results
  expectedParamEstimates<- c(.84,.48,.99) # c(.37,1.2,.017)  #from L-BFGS-B
  LBFGSBparams<-  fit["L-BFGS-B",]
  discrepancy <- LBFGSBparams[1:3] - expectedParamEstimates
  discrepancyLow <- all( abs( discrepancy ) < .1 )
  expect_that( discrepancyLow, is_true() )
  
  # A kkt1 of True means that the final gradient was close to 0 (the optimizer found an extremum),
  #a kkt2 of True means that the Hessian is positive definite (it's a minimum). Both should be True.
  print(fit)
  expect_that( fit["L-BFGS-B","kkt1"], is_true() ) #means that the final gradient was close to 0 (the optimizer found an extremum)
}    
)