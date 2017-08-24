# Set PARAMETER BOUNDS. Pat apparently found these were needed to 
# prevent over-fitting to blips in the distributions. These
# values are about right in most cases, but might need some tweaking if
# e.g. you were analysing data with an unusually high or low item rate.

parameterBounds<- function() {
  muBound <- 4   #will only consider -4 to +4 for mu 
  sigmaBound <- 4 #will only consider 0 to 4 for sigma
  
  smallNonZeroNumber <- 10^-5# Useful number for when limits can't be exactly zero but can be anything larger
  #efficacy,          latency,    precision
  lowerBounds <- c(smallNonZeroNumber, -muBound, smallNonZeroNumber)
  upperBounds <- c(1,                   muBound, sigmaBound)
  
  bounds<- data.frame(lower= lowerBounds, upper= upperBounds)
  
  row.names(bounds)<- c("efficacy",'latency','precision')
  
  return( bounds )
}