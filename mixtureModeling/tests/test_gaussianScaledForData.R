# test intended to be used with the testthat package

#To testthat, run test_file("mixtureModeling/tests/test_gaussianScaledForData.R")
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

test_that("At least it runs", {

  freqs<- gaussianScaledForData(efficacy,latency,precision,numTrials,minSPE,maxSPE,grain)
  
  expect_that( nrow(freqs), equals( maxSPE-minSPE+1 ) )
  
} )


test_that("Maximum is at latency position", {
  
  latency<- 0
  
  freqs<- gaussianScaledForData(efficacy,latency,precision,numTrials,minSPE,maxSPE,grain)
  peak<- freqs[ which.max(freqs$gaussianFreq), ]
  
  expect_that( peak$x , equals(latency) )

  latency<- 2
  
  freqs<- gaussianScaledForData(efficacy,latency,precision,numTrials,minSPE,maxSPE,grain)
  peak<- freqs[ which.max(freqs$gaussianFreq), ]
  
  expect_that( peak$x , equals(latency) )  
} )




#Add a formal test