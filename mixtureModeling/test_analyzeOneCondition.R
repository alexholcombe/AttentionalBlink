
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


#BO,1,1
estimates %>% filter(round(p1,3)==0.280) #BA,2,2 or BE,2,1
#1e-05  4 1e-05 334.385593546 but didn't end up being the winning replicate
#Inspect AD,2,1 and AI,1,2 because very poor fit
