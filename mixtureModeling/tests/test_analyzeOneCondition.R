#To testthat, run test_file("mixtureModeling/tests/test_analyzeOneCondition.R")
#Compensate for path getting set to mixtureModeling/tests/
if (basename(getwd()) != "tests") {
  pathNeeded<- "mixtureModeling"
} else { 
  pathNeeded <- ".." 
}

source(file.path(pathNeeded,"analyzeOneCondition.R"))
source(file.path(pathNeeded,"parameterBounds.R"))

df<-readRDS(file.path(pathNeeded,"tests","exampleSubject.Rdata"))
library(dplyr)

#Test with a problematic dataset
test_that("Low-efficacy case", {
  
  data<- readRDS( file.path(pathNeeded,"tests","alexImportBackwardsPaper2E1.Rdata") ) #.mat file been preprocessed into melted long dataframe
  #data<- readRDS( file.path("mixtureModeling/tests", "alexImportBackwardsPaper2E1.Rdata") ) #.mat file been preprocessed into melted long dataframe
  library(dplyr)
  numItemsInStream<- length( data$letterSeq[1,] )  
  #It seems that to work with dplyr, can't have array field like letterSeq
  data$letterSeq<- NULL
  BA22 <- data %>% dplyr::filter(subject=="BA" & target==2 & condition==2)
  #Rather low efficacy dataset
  estimates<- analyzeOneCondition(BA22,numItemsInStream,parameterBounds())
  #print(estimates) # p1=.28, p2=.07, p3=.75

  expect_that( estimates$warnings == "None", is_true() )
  
  #Check estimates are what I expect
  expectedParamEstimates<- c(.28,.07,.75)
  discrepancy <- estimates[1:3] - expectedParamEstimates
  discrepancyLow <- all( abs( discrepancy ) < .1 )
  expect_that( discrepancyLow, is_true() )
}
)


