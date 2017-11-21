#Compensate for path getting set to mixtureModeling/tests/ by testthat
if (basename(getwd()) != "tests") { #directory of this file, which should be mixtureModeling/
  pathNeeded<- "mixtureModeling" 
} else { 
  pathNeeded <- ".." 
}
library(dplyr)
require(ggplot2)
source( file.path(pathNeeded,"theme_apa.R")  )
source( file.path(pathNeeded,"calcCurvesDataframes.R")  )

#Import MATLAB fits

#Import raw data
#Experiment was administered by MATLAB
#.mat file been preprocessed into melted long dataframe by backwarsLtrsLoadRawData.R
rawDataPath<- file.path(pathNeeded,"tests")
data<- readRDS( file.path(rawDataPath, "alexImportBackwardsPaper2E1.Rdata") ) 
minSPE<- -17; maxSPE<- 17
df<- data
numItemsInStream<- length(data$letterSeq[1,]) #24
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

group_vars <- c("orientation", "stream", "subject")


#Calc nPerCond to each condition. This is needed only for scaling the fine-grained Gaussian
#Calc the number of observations for each condition, because gaussianScaledforData needs to know.
#To use variable names in variables with dplyr, https://stackoverflow.com/questions/43415475/how-to-parametrize-function-calls-in-dplyr-0-7/
dNum<- dataStrippedOfEstimates %>% group_by_at(.vars = group_vars) %>% summarise(nPerCond = n())
#add numObservations to MATLAB estimates, which will then be merged with raw data
estimates_M<- merge(estimates_M,dNum)
#merge MATLAB estimates with raw data
dM<-merge( dataStrippedOfEstimates, estimates_M )


dM<- filter(dM, subject=="AA", orientation =="Canonical")
estimates_M<- filter(estimates_M, subject=="AA", orientation =="Canonical")


#calcFitDataframes should now use MATLAB estimates rather than using R to calculate its own,
# because it detects presence of efficacy variable
curvesMATLAB<- dM %>% group_by_at(.vars = group_vars) %>% 
  do(calcCurvesDataframes(.,minSPE,maxSPE,numItemsInStream))


g=ggplot(dM, aes(x=SPE)) + facet_grid(subject+stream+orientation~.)
g<-g+geom_histogram(binwidth=1,color="grey90") + xlim(minSPE,maxSPE)
g<-g +theme_apa() 
sz=.8
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

#Now plot the underlying continuous Gaussian too, not just the discretized Gaussian, so can potentialy
# see where Pat's fit based on density height yields funky things.
grain<-.05
gaussianFine<- estimates_M %>% group_by_at(.vars = group_vars) %>% do( 
  gaussianScaledFromDataframe(.,minSPE,maxSPE,grain) )

g<-g + geom_line(data=gaussianFine,aes(x=x,y=gaussianFreq),color="darkblue",size=1.2)
#Wow , great illustration of the dangers of not integrating the whole bin, if that's the reason.
#In AA, right, canonical graph, 
# a salient difference is visible between the continuous Gaussian and the discretized predicted bin count.
# I think this is because the Gaussian is so steep (such low sigma) that integrating the bin area gives a 
# large number. But note that this Gaussian was fit by MATLAB, so it's actually the height of the Gaussian 
# at the center of the bin that's what was used for the fit.

show(g)
