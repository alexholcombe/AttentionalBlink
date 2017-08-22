
#Compensate for path getting set to mixtureModeling/tests/
if (basename(getwd()) != "tests") {
  pathNeeded<- "mixtureModeling"
} else { 
  pathNeeded <- ".." 
}

data<- readRDS( file.path(pathNeeded,"tests", "alexImportBackwardsPaper2E1excludedSs.Rdata") ) #.mat file been preprocessed into melted long dataframe
library(dplyr)
numItemsInStream<- length( data$letterSeq[1,] )  
#It seems that to work with dplyr, can't have array field like letterSeq
data$letterSeq<- NULL

source(file.path(pathNeeded,"histogramPlotting.R"))
source(file.path(pathNeeded,"analyzeOneCondition.R"))

#plot histogram
require(ggplot2)
minSPE<- -17; maxSPE<- 17

#Give conditions better names than 1 and 2
names(data)[names(data) == 'target'] <- 'stream'
data <- data %>% mutate( stream =ifelse(stream==1, "Left","Right") )
#mutate condition to Orientation
names(data)[names(data) == 'condition'] <- 'orientation'
data <- data %>% mutate( orientation =ifelse(orientation==1, "Canonical","Inverted") )


#Use ggplot to plot every condition*subject separately
df<-data 
#df<-data %>% filter(subject=="BE" | subject=="BA")

#So I actually need all fits in one dataframe,
# doesn't have to be the same dataframe as the SPEs (and can't be, because different format)
#After group_by, subsetted df needs to be fit
tit<-"subjs" #Calculate fit separately for each group
quartz(title=tit,width=12,height=6) 
#df<- data %>% filter(subject < "AO")
#df<- data %>% filter(subject >= "AO" & subject <= "BD")
df<- data %>% filter(subject > "BD")
fitDfs<- df %>% group_by(orientation,stream,subject) %>% 
  do(calcFitDataframes(.,minSPE,maxSPE,numItemsInStream))

gsz=.3
g<-g+ geom_line(data=fitDfs,aes(x=x,y=guessingFreq),color="yellow",size=sz)
g<-g+ geom_line(data=fitDfs,aes(x=x,y=gaussianFreq),color="lightblue",size=sz)
g<-g+ geom_point(data=fitDfs,aes(x=x,y=combinedFitFreq),color="green",size=sz)
g<-g + theme_bw() +theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank())# hide all gridlines.
g

#I should probably preserve the estimates, then text annotate them
#Examine estimates
#round numeric columns so easier to view
data.frame(lapply(estimates, function(y) if(is.numeric(y)) round(y, 2) else y)) 
g