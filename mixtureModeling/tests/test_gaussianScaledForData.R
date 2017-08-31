# test intended to be used with the testthat package

#To testthat, run test_file("mixtureModeling/tests/test_histogramPlotting.r")
#Compensate for path getting set to mixtureModeling/tests/
if (basename(getwd()) != "tests") {
  pathNeeded<- "mixtureModeling"
} else { 
  pathNeeded <- ".." 
}

source( file.path(pathNeeded,"gaussianScaledForData.R") ) #for calcFitDataframes
