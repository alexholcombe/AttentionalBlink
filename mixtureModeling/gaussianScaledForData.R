#Calculate the underlying continuous Gaussian, not discretized. But how high should it be?

#Given a dataset of 100 points. Take the Gaussian density, area under the curve is 1,
#and make the area under the curve = 100?
#But wait a second - the curve goes forever, but I'm only showing -17 to 17.
#No, but each of those 100 points is actually a grainsize portion of the curve.
#So, calculate the area of the Gaussian density for each bin. Then multiply by a normalizing
#factor so that the total area of all the bins equals the number of datapoints.
#This is neglecting that we're talking about a truncated Gaussian.

areaOfGaussianBin<- function(binStart,binWidth,latency,precision) {
  #Calculate area under the unit curve for that bin
  area <- pnorm(binStart+binWidth,latency,precision) - pnorm(binStart,latency,precision)
  return (area)
}
  
gaussianScaledForData<- function(efficacy,latency,precision,SPE,minSPE,maxSPE,grain) {
  
  domain<-seq(minSPE,maxSPE,grain)

  #Calculate the likelihood of each data point (each SPE observed)
  #The data is discrete SPEs, but the model is a continuous Gaussian.
  #The probability of an individual SPE 
  
  #Calculate area under the unit Gaussian curve for each bin
  binsStarts<-data.frame(SPE= seq(minSPE,maxSPE-grain,grain) )
  binAreasGaussian<- lapply(SPE,areaOfGaussianBin,grain,latency,precision)
  
  area <- pnorm(binEnd,latency,precision) - pnorm(binStart,latency,precision)
  
  gaussianThis<- dnorm(domain,latency,precision)
  #Calculate points at appropriate height for this data
  #print(paste0("lengthSPE=",length(SPE)))
  gaussianThis<- gaussianThis * efficacy * length(SPE)
  gaussianThis<-data.frame(x=domain, gaussianFreq=gaussianThis)
  return(gaussianThis)
}


#Below is mistaken old way that used plain density without integrating the area of each bin
gaussianScaledForDataOld<- function(efficacy,latency,precision,SPE,minSPE,maxSPE,grain) {
  domain<-seq(minSPE,maxSPE,grain)
  gaussianThis<- dnorm(domain,latency,precision)
  #Calculate points at appropriate height for this data
  #print(paste0("lengthSPE=",length(SPE)))
  gaussianThis<- gaussianThis * efficacy * length(SPE)
  gaussianThis<-data.frame(x=domain, gaussianFreq=gaussianThis)
  return(gaussianThis)
}