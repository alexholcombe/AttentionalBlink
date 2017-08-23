
#Compensate for path getting set to mixtureModeling/tests/
if (basename(getwd()) != "tests") {
  pathNeeded<- "mixtureModeling"
} else { 
  pathNeeded <- ".." 
}

data<- readRDS( file.path(pathNeeded,"tests", "alexImportBackwardsPaper2E1excludedSs.Rdata") ) #.mat file been preprocessed into melted long dataframe
numItemsInStream<- length( data$letterSeq[1,] )  
#It seems that to work with dplyr, can't have array field like letterSeq
data$letterSeq<- NULL

source(file.path(pathNeeded,"histogramPlotting.R"))
source(file.path(pathNeeded,"analyzeOneCondition.R"))

#plot histogram
require(ggplot2)
library(dplyr)

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
#df<- data %>% filter(subject > "BD")

df<- data %>% dplyr::filter(subject <="AP") #dplyr::filter(subject <= "BD" & subject >="AP")
fitDfs<- df %>% group_by(orientation,stream,subject) %>% 
  do(calcFitDataframes(.,minSPE,maxSPE,numItemsInStream))

#plot data
g=ggplot(df, aes(x=SPE)) + facet_grid(orientation~subject+stream)
g<-g+geom_histogram(binwidth=1) + xlim(minSPE,maxSPE)
sz=.3
g<-g+ geom_line(data=fitDfs,aes(x=x,y=guessingFreq),color="yellow",size=sz)
g<-g+ geom_line(data=fitDfs,aes(x=x,y=gaussianFreq),color="lightblue",size=sz)
g<-g+ geom_point(data=fitDfs,aes(x=x,y=combinedFitFreq),color="green",size=sz)
g<-g + theme_bw() +theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank())# hide all gridlines.
numGroups<- length(table(df$orientation,df$subject,df$stream))
fontSz = 70/numGroups
#uses plotmath, not just string interpretation http://ggplot2.tidyverse.org/reference/geom_text.html
g +geom_text(data=fitDfs,aes(x=-8,y=30, label = paste("plain(e)==", round(efficacy,2), sep = "")),  parse = TRUE,size=fontSz) +
  geom_text(data=fitDfs,aes(x=-8,y=27, label = paste("mu==", round(latency,2), sep = "")),  parse = TRUE,size=fontSz)+
  geom_text(data=fitDfs,aes(x=-8,y=25, label = paste("sigma==", round(precision,2), sep = "")),  parse = TRUE,size=fontSz)





g + geom_text(data=fitDfs,aes(x=-5,y=30, label = paste("mu==", round(efficacy,2), sep = "")), parse = TRUE,size=fontSz)) +
    geom_text(data=fitDfs,aes(x=-5,y=27,    label = round(latency,2)),  parse = TRUE,size=fontSz) +
    geom_text(data=fitDfs,aes(x=-5,y=25,    label = round(precision,2)),  parse = TRUE,size=fontSz)


#Examine estimates
#They're in fitDfs but need to be collapsed
#round numeric columns so easier to view
estimates<-fitDfs %>% select(subject,orientation,stream,
data.frame(lapply(estimates, function(y) if(is.numeric(y)) round(y, 2) else y)) 
g