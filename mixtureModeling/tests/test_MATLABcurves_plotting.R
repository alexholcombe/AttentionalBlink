df<- df %>% dplyr::filter(subject=="AA",stream=="Right", orientation=="Canonical")
source( file.path(mixModelingPath,"histogramPlotting.R") ) #for calcFitDataframes
source( file.path(mixModelingPath,"theme_apa.R") ) #for calcFitDataframes

#Add R parameter estimates to dataframe
df<- merge(df,estimates) 

fitDfs<- df %>% group_by(orientation,stream,subject) %>% 
  do(calcFitDataframes(.,minSPE,maxSPE,numItemsInStream))

#create MATLAB curves
estimates_M<- estimates_MATLAB %>% rename(efficacy=efficacy_M,latency=latency_M,precision=precision_M)
#create temporary dataframe with data plus MATLAB estimates to generate curves
dataStrippedOfEstimates<- select(df,-efficacy,-latency,-precision, -val)
dM<-merge( dataStrippedOfEstimates, estimates_M )
dM<- dM %>% rename( val = val_M ) #has to be called val so calcFitDataframes uses it rather than using R to calculate its own
curvesMATLAB<- dM %>% group_by(orientation,stream,subject) %>% 
  do(calcFitDataframes(.,minSPE,maxSPE,numItemsInStream))



finerGrainFactor = 10
finerGrainImaginaryDataSetLength<- length(dM$SPE)# * finerGrainFactor
gaussianFine<-gaussianScaledForData(estimates$efficacy,estimates$latency,estimates$precision,
                      1:finerGrainImaginaryDataSetLength,-17,17, 1/finerGrainFactor)

g=ggplot(dM, aes(x=SPE)) + facet_grid(subject+stream+orientation~.)
g<-g+geom_histogram(binwidth=1,color="grey90") + xlim(minSPE,maxSPE)
g<-g +theme_apa() 
sz=.8
#THE MATLAB BLUE AND YELLOW AND GREEN LOOKS WRONG
g<-g+ geom_point(data=curvesMATLAB,aes(x=x,y=combinedFitFreq),color="chartreuse3",size=sz*2.5)
g<-g+ geom_line(data=curvesMATLAB,aes(x=x,y=guessingFreq),color="yellow",size=sz)
g<-g+ geom_line(data=curvesMATLAB,aes(x=x,y=gaussianFreq),color="lightblue",size=sz)

g<-g+ geom_line(data=gaussianFine,aes(x=x,y=gaussianFreq),color="red",size=sz/2)
show(g)

fontSz = 80/30
g<-g + geom_text(data=curvesMATLAB,aes(x=-9,y=32, label = paste("-logLik==", round(val,1), sep = "")),  parse = TRUE,size=fontSz) +
  geom_text(data=curvesMATLAB,aes(x=-7,y=28, label = paste("plain(e)==", round(efficacy,2), sep = "")),  parse = TRUE,size=fontSz) +
  geom_text(data=curvesMATLAB,aes(x=-7,y=25, label = paste("mu==", round(latency,2), sep = "")),  parse = TRUE,size=fontSz)+
  geom_text(data=curvesMATLAB,aes(x=-7,y=22, label = paste("sigma==", round(precision,2), sep = "")),  parse = TRUE,size=fontSz)
show(g)