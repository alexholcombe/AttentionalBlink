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
test_that("Problematic cases", {
  
  data<- readRDS( file.path(pathNeeded,"tests","alexImportBackwardsPaper2E1.Rdata") ) #.mat file been preprocessed into melted long dataframe
  #data<- readRDS( file.path("mixtureModeling/tests", "alexImportBackwardsPaper2E1.Rdata") ) #.mat file been preprocessed into melted long dataframe
  library(dplyr)
  numItemsInStream<- length( data$letterSeq[1,] )  
  data<- data
  #It seems that to work with dplyr, can't have array field like letterSeq
  data$letterSeq<- NULL
  BA22 <- data %>% dplyr::filter(subject=="BA" & target==2 & condition==2)
  estimates<- analyzeOneCondition(BA22,numItemsInStream,parameterBounds())
  expect_that( estimates$warnings == "None", is_true() )
}
)


