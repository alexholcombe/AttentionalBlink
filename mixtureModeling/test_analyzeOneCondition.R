
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