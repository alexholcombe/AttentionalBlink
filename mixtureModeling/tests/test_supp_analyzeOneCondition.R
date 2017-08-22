# test of fitModel intended to be used with the testthat package

#To testthat, run test_file("mixtureModeling/tests/test_fitModel.r")
#Compensate for path getting set to mixtureModeling/tests/
if (basename(getwd()) != "tests") {
  pathNeeded<- "mixtureModeling"
} else { 
  pathNeeded <- ".." 
}
source(file.path(pathNeeded,"analyzeOneCondition.R"))

test_that("Fits entire experiment worth of data fine", {

  data<- readRDS( file.path(pathNeeded,"tests", "alexImportBackwardsPaper2E1.Rdata") ) #.mat file been preprocessed into melted long dataframe
  library(dplyr)
  numItemsInStream<- length( data$letterSeq[1,] )  
  data<- data
  #It seems that to work with dplyr, can't have array field like letterSeq
  data$letterSeq<- NULL

  estimates<-data %>% group_by(subject,target,condition) %>% 
              do(analyzeOneCondition(.,numItemsInStream))

  #round numeric columns so easier to view
  data.frame(lapply(estimates, function(y) if(is.numeric(y)) round(y, 2) else y)) 
  
  expect_that( all(estimates$warnings == "None"), is_true() )  

} 
)


test_that("Handles terrible subjects", {
  

  
} 
)



  