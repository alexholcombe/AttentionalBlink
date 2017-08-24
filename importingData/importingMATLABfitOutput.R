#In case you want to import some MATLAB mixture modeling output

#Import MATLAB fits still as MATLAB file
MATLABmixtureModelOutputPath<-"~/Google\ Drive/Backwards\ paper/secondPaper/E1/Data/MixModData"
estimatesFileName <- "Exp1_ModellingOutput_all_Ss.mat"
estimatesFile<- file.path( MATLABmixtureModelOutputPath, estimatesFile )

require(R.matlab)

MATLABfits=tryCatch( 
  readMat(estimatesFile), 
  error=function(e) { 
    stop( paste0("ERROR reading the file ",fname," :",e) )
  } )

#compiledErrors
#positionError
#allEstimates.byParticipant
#allLowerBound.byParticipant
#allUpperBounds.byParticipant

#Unfortunately none of those is the negative log likelihood, which is not preserved