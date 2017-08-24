# Randomise starting values for each parameter.
parametersGuess<- function( parametersLowerBound, parametersUpperBound ) {
  guess<- rep(0,3)
  for (i in 1:length(guess)) {
    #random value between min and max possible value
    guess[i] <- runif(n=1, min=parametersLowerBound[i], max=parametersUpperBound[i] ) 
  } 
  return (guess)
}