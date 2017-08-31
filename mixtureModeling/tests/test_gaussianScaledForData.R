# test intended to be used with the testthat package

#To testthat, run test_file("mixtureModeling/tests/test_histogramPlotting.r")
#Compensate for path getting set to mixtureModeling/tests/
if (basename(getwd()) != "tests") {
  pathNeeded<- "mixtureModeling"
} else { 
  pathNeeded <- ".." 
}

source( file.path(pathNeeded,"gaussianScaledForData.R") ) #for calcFitDataframes

minSPE<- -17; maxSPE<- 17; 
efficacy <- 1
latency <- 0
precision<- 1
grain<-1
numTrials<-100
gaussianScaledForData(efficacy,latency,precision,numTrials,minSPE,maxSPE,grain)

#Add a formal test