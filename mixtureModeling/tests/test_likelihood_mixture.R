library(testthat)
#To testthat, run test_file("mixtureModeling/tests/test_likelihood_mixture.R")
#Compensate for path getting set to mixtureModeling/tests/
if (basename(getwd()) != "tests") {
  pathNeeded<- "mixtureModeling"
} else { 
  pathNeeded <- ".." 
}

source( file.path(pathNeeded,"likelihood_mixture.R") )

minSPE<- -17; maxSPE<- 17; 
numItemsInStream<-24
numTrials<-100
trialsPerSerialPosition<-10
possibleSerialPositions<- c(7,9,11,14,18)
targetSerialPositions<-rep(possibleSerialPositions,trialsPerSerialPosition)

test_that("At least it runs", {
  
  binStart<- -17.5; binWidth<- 1
  latency<- 0; precision<- 1
  guessingDistro <- createGuessingDistribution(minSPE,maxSPE,targetSerialPositions,numItemsInStream)
  
  #Test areaUnderGaussianBinTapered
  expect_that(
      areaUnderGaussianBinTapered(binStart,binWidth,latency,precision,guessingDistro,minSPE,maxSPE) < exp(-30),
      is_true() )

  binStart<- -0.5
  expect_that(
     areaUnderGaussianBinTapered(binStart,binWidth,latency,precision,guessingDistro,minSPE,maxSPE),
     equals(19, tolerance=0.2) #A large number because the guessing distribution is not normalized at all
  )

  #Test areaUnderTruncatedGaussianTapered
  expect_that(
    areaUnderTruncatedGaussianTapered(latency,precision,guessingDistro,minSPE,maxSPE) < sum(guessingDistro),
    is_true() )
  
} )

test_that("likelihood_mixture does calculate some probabilities", {
  
  guessingDistro <- createGuessingDistribution(minSPE,maxSPE,targetSerialPositions,numItemsInStream)
  
  SPEs<- c(-8,-3,-1,0,1)
  p<-1
  mu<-0; sigma<-1

  expect_that( #Check output is correct length
    length( likelihood_mixture(SPEs,p,mu,sigma,minSPE,maxSPE,guessingDistro) ),
    equals( length(SPEs) )
  )
    
  
  probs<- likelihood_mixture(SPEs,p,mu,sigma,minSPE,maxSPE,guessingDistro) 
  expect_that( #Highly probable observation is somewhat probable
    probs[5] > exp(-2),
    is_true()
  )  
  
} )