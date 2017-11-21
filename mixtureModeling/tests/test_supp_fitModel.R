#test of fitModel, intended to be used with the testthat package
#To testthat, run test_file("mixtureModeling/tests/test_fitModel.r")

#Compensate for path getting set to mixtureModeling/tests/
if (basename(getwd()) != "tests") {
  pathNeeded<- "mixtureModeling"
} else { 
  pathNeeded <- ".." 
}

source( file.path(pathNeeded,'parameterBounds.R') )

test_that("No warnings for any condition for subject AA in backwards paper2E1", {
  
  data<- readRDS( file.path(pathNeeded,"tests", "alexImportBackwardsPaper2E1.Rdata") ) #.mat file been preprocessed into melted long dataframe
  library(dplyr)
  numItemsInStream<- length( data$letterSeq[1,] )  
  #It seems that to work with dplyr, can't have array field like letterSeq
  data$letterSeq<- NULL
  df<- filter(data, subject =="AA")  #subject < "AC") #Otherwise will take long time to run the test 

  source(file.path(pathNeeded,"analyzeOneCondition.R"))
  estimates<-data %>% group_by(subject,target,condition) %>% 
    do(analyzeOneCondition(.,numItemsInStream,parameterBounds()))
  
  #See how variable the fits are across different starting positions?
  
}    
)


  