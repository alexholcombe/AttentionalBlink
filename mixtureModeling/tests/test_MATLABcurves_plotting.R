#Compensate for path getting set to mixtureModeling/tests/ by testthat
if (basename(getwd()) != "tests") { #directory of this file, which should be mixtureModeling/
  pathNeeded<- "mixtureModeling" 
} else { 
  pathNeeded <- ".." 
}
library(dplyr)

source( file.path(pathNeeded,"theme_apa.R")  )
source( file.path(pathNeeded,"calcCurvesDataframes.R")  )

#Import MATLAB fits

#Import raw data
#Experiment was administered by MATLAB
#.mat file been preprocessed into melted long dataframe by backwarsLtrsLoadRawData.R
rawDataPath<- file.path(pathNeeded,"tests")
data<- readRDS( file.path(rawDataPath, "alexImportBackwardsPaper2E1.Rdata") ) 

df<- data
df$letterSeq<- NULL #dplyr can't handle dataframes with this type of array in them
#df<- dplyr::filter(df, subject=="AA",stream=="Right", orientation=="Canonical")

#rename variables to be more understandable
df<-df %>% rename(stream = target, orientation = condition )
df <- df %>% mutate( stream =ifelse(stream==1, "Left","Right") )
#mutate condition to orientation
df <- df %>% mutate( orientation =ifelse(orientation==1, "Canonical","Inverted") )


#Load in MATLAB parameter estimates from Chris Bush run of MATLAB
#Import MATLAB fits
MATLABmixtureModelOutputPath<-"~/Google\ Drive/Backwards\ paper/secondPaper/E1/Data/MixModData"
importedToRbyChris <- "allParams.RData"
MATLABmixtureModelOutput<- file.path( MATLABmixtureModelOutputPath, importedToRbyChris )
load(MATLABmixtureModelOutput, verbose=FALSE)

#join Chris Bush's format into single long dataframe
estimates_MATLAB<- merge(efficacy.long,latency.long)
estimates_MATLAB<- merge(estimates_MATLAB,precision.long)
estimates_MATLAB<- data.frame(estimates_MATLAB)
names(estimates_MATLAB) <- tolower(names(estimates_MATLAB)) #lower case the column names
estimates_MATLAB<-estimates_MATLAB %>%
  mutate(efficacy=efficacy/100,latency=latency/100,precision=precision/100)


#create MATLAB curves
estimates_M<- estimates_MATLAB 
#create temporary dataframe with data plus MATLAB estimates to generate curves
dataStrippedOfEstimates<- df
dM<-merge( dataStrippedOfEstimates, estimates_M )

dM<- filter(dM, subject=="AA") #, orientation =="Canonical")

#calcFitDataframes should now use MATLAB estimates rather than using R to calculate its own,
# because it detects presence of efficacy variable
curvesMATLAB<- dM %>% group_by(orientation,stream,subject) %>% 
  do(calcCurvesDataframes(.,minSPE,maxSPE,numItemsInStream))


g=ggplot(dM, aes(x=SPE)) + facet_grid(subject+stream+orientation~.)
g<-g+geom_histogram(binwidth=1,color="grey90") + xlim(minSPE,maxSPE)
g<-g +theme_apa() 
sz=.8
#THE MATLAB BLUE AND YELLOW AND GREEN LOOKS WRONG
g<-g+ geom_point(data=curvesMATLAB,aes(x=x,y=combinedFitFreq),color="chartreuse3",size=sz*2.5)
g<-g+ geom_line(data=curvesMATLAB,aes(x=x,y=guessingFreq),color="yellow",size=sz)
g<-g+ geom_line(data=curvesMATLAB,aes(x=x,y=gaussianFreq),color="lightblue",size=sz)

fontSz = 80/30
g<-g + geom_text(data=curvesMATLAB,aes(x=-7,y=28, label = paste("plain(e)==", round(efficacy,2), sep = "")),  parse = TRUE,size=fontSz) +
  geom_text(data=curvesMATLAB,aes(x=-7,y=25, label = paste("mu==", round(latency,2), sep = "")),  parse = TRUE,size=fontSz)+
  geom_text(data=curvesMATLAB,aes(x=-7,y=22, label = paste("sigma==", round(precision,2), sep = "")),  parse = TRUE,size=fontSz)
show(g)
#Haven't calculated logLikelihood for MATLAB
#geom_text(data=curvesMATLAB,aes(x=-9,y=32, label = paste("-logLik==", round(val,1), sep = "")),  parse = TRUE,size=fontSz) 
grain<-.5 #.05
numObservations<- length(df$SPE)
#Scale up the numObservations to compensate for the smaller grain size
numObservations<- numObservations * 1/grain 
#Need to calculate Gaussian heights at this finer grain for each condition
#Each call to gaussianScaledForData returns a larger dataframe than was sent. 
# To do that with dplyr, 


gaussianScaledFromDataframe<- function(df, numObservations,minSPE,maxSPE,grain) {
  #ans<- data.frame(e=df$efficacy, x=c(1,2,3))
  print(df$efficacy)  
  ans<- merge(df, data.frame(x=c(1,2,3)), by=NULL)
  return (ans)
}
# merge(iris, data.frame(time=1:10), by=NULL) https://stackoverflow.com/questions/11693599/alternative-to-expand-grid-for-data-frames

estimates_M %>% dplyr::filter(subject=="AA") %>% group_by(orientation,stream,subject) %>% do( 
  gaussianScaledFromDataframe(.,numObservations,minSPE,maxSPE,grain) )


gaussianScaledFromDataframe<- function(df, numObservations,minSPE,maxSPE,grain) {
  #Should be sent a one-line dataframe with efficacy,latency,precision
  #Want to expand that into entire curve, with many different SPE values
  curve<- gaussianScaledForData(df$efficacy,df$latency,df$precision,numObservations,minSPE,maxSPE,grain)
  print(curve)
  #withParams<- merge(df, data.frame(x=c(1,2,3)), by=NULL)
  withParams<<- merge(df, curve, by=NULL)
  return(withParams)
}

estimates_M %>% dplyr::filter(subject=="AA") %>% group_by(orientation,stream,subject) %>% do( 
  gaussianScaledFromDataframe(.,numObservations,minSPE,maxSPE,grain) )
 
estimates_M %>% dplyr::filter(subject=="AA") %>% group_by(orientation,stream,subject) %>% do(print(.))
  


estimates_M %>% dplyr::filter(subject=="AA",stream=="Left",orientation=="Inverted") %>% group_by(orientation,stream,subject) %>% 
                gaussianScaledFromDataframe(.,numObservations,minSPE,maxSPE,grain)

dM %>% do(gaussianScaledForData(.$efficacy,.$latency,.$precision,numObservations,minSPE,maxSPE,grain))
          
gaussianFine<- gaussianScaledForData(efficacy,latency,precision,numObservations,minSPE,maxSPE,grain) 
g + geom_line(data=gaussianFine,aes(x=x,y=gaussianFreq),color="darkblue",size=1.2)
