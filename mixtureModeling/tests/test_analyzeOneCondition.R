
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
  df<- data
  #It seems that to work with dplyr, can't have array field like letterSeq
  df$letterSeq<- NULL
  BA22 <- df %>% filter(subject=="BA" & target==2 & condition==2)
  analyzeOneCondition(BA22,numItemsInStream)
  #plot histogram
  #plot dnorm and guessing distribution
)


#BO,1,1
estimates %>% filter(round(p1,3)==0.280) #BA,2,2 or BE,2,1
#1e-05  4 1e-05 334.385593546 but didn't end up being the winning replicate
#Inspect AD,2,1 and AI,1,2 because very poor fit
