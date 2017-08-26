#To testthat, run test_file("mixtureModeling/tests/test_likelihoodOneConditionGivenParams.R")
#Compensate for path getting set to mixtureModeling/tests/
if (basename(getwd()) != "tests") {
  pathNeeded<- "mixtureModeling"
} else { 
  pathNeeded <- ".." 
}

#Test
test_that("Basic test", {
  
  data<- readRDS( file.path(pathNeeded,"tests","alexImportBackwardsPaper2E1.Rdata") ) #.mat file been preprocessed into melted long dataframe
  #data<- readRDS( file.path("mixtureModeling/tests", "alexImportBackwardsPaper2E1.Rdata") ) #.mat file been preprocessed into melted long dataframe
  library(dplyr)
  numItemsInStream<- length( data$letterSeq[1,] )  
  data<- data
  #It seems that to work with dplyr, can't have array field like letterSeq
  data$letterSeq<- NULL
  BA22 <- data %>% dplyr::filter(subject=="BA" & target==2 & condition==2)
  
  params<- data.frame( p1=0.2796651, p2=.06975089, p3=.8007981 )

  l<- likelihoodOneConditionGivenParams(BA22, 24, params)
  
  expect_that( l, equals(319.4832, tolerance  = 0.01) )
  }
)
