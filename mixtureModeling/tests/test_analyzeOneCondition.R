
#To test, run test_file("mixtureModeling/test_analyzeOneCondition.R")
#But that doesn't work because then the path gets set to mixtureModeling/
print(getwd())
if (basename(getwd()) != "mixtureModeling") {
  pathNeeded<- "mixtureModeling"
} else { 
  pathNeeded <- "." 
}
source(file.path(pathNeeded,"analyzeOneCondition.R"))

df<-readRDS(file.path(pathNeeded,"exampleSubject.Rdata"))
library(dplyr)
df<- filter(df, condition==1 & target==1)

estimates<- analyzeOneCondition(df, 24)
estimates

#Test with a problematic dataset
test_that("Problematic cases", {
  
  data<- readRDS( file.path(pathNeeded, "alexImportBackwardsPaper2E1.Rdata") ) #.mat file been preprocessed into melted long dataframe
  #data<- readRDS( file.path("mixtureModeling/tests", "alexImportBackwardsPaper2E1.Rdata") ) #.mat file been preprocessed into melted long dataframe
  library(dplyr)
  numItemsInStream<- length( data$letterSeq[1,] )  
  data<- data
  #It seems that to work with dplyr, can't have array field like letterSeq
  data$letterSeq<- NULL
  BA22 <- data %>% filter(subject=="BA" & target==2 & condition==2)
  estimates<- analyzeOneCondition(BA22,numItemsInStream)
  #plot histogram
  require(ggplot2)
  df<-BA22
  #sanity check
  minSPE<- -17; maxSPE<- 17


)


