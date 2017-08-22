# test of histogramPlotting intended to be used with the testthat package

#To testthat, run test_file("mixtureModeling/tests/test_histogramPlotting.r")
#Compensate for path getting set to mixtureModeling/tests/
if (basename(getwd()) != "tests") {
  pathNeeded<- "mixtureModeling"
} else { 
  pathNeeded <- ".." 
}

data<- readRDS( file.path(pathNeeded,"tests", "alexImportBackwardsPaper2E1.Rdata") ) #.mat file been preprocessed into melted long dataframe
#data<- readRDS( file.path("mixtureModeling/tests", "alexImportBackwardsPaper2E1.Rdata") ) #.mat file been preprocessed into melted long dataframe
library(dplyr)
numItemsInStream<- length( data$letterSeq[1,] )  
#It seems that to work with dplyr, can't have array field like letterSeq
data$letterSeq<- NULL

source(file.path(pathNeeded,"histogramPlotting.R"))
source(file.path(pathNeeded,"analyzeOneCondition.R"))

#plot histogram
require(ggplot2)
minSPE<- -17; maxSPE<- 17

names(data)[names(data) == 'target'] <- 'stream'
data <- data %>% mutate( stream =ifelse(stream==1, "Left","Right") )
#mutate condition to Orientation
names(data)[names(data) == 'condition'] <- 'orientation'
data <- data %>% mutate( orientation =ifelse(orientation==1, "Canonical","Inverted") )


df<-data %>% filter(subject=="BE")

#This needs to be done for each condition. So I actually do need all conditions in one fit dataframe,
# doesn't have to be the same dataframe as the SPEs (and can't be, because different format)
#After group_by, subsetted df needs to be fit
fitDfs<- calcFitDataframes(df,minSPE,maxSPE,numItemsInStream)
g=ggplot(df, aes(x=SPE)) 
#plot data
g<-g+geom_histogram(binwidth=1) + xlim(minSPE,maxSPE)
g<-g+ geom_line(data=fitDfs,aes(x=x,y=guessingFreq),color="yellow",size=1.2)
g<-g+ geom_line(data=fitDfs,aes(x=x,y=gaussianFreq),color="blue",size=1.2)
g<-g+ geom_point(data=fitDfs,aes(x=x,y=combinedFitFreq),color="green",size=1.2)
g
#also need to annotate with subject name


df<-data 

tit<-"subjs" #Calculate fit separately for each group
quartz(title=tit,width=6,height=6) 

df<- data %>% filter(subject < "BD")
fitDfs<- df %>% group_by(orientation,stream,subject) %>% 
  do(calcFitDataframes(.,minSPE,maxSPE,numItemsInStream))

g=ggplot(df, aes(x=SPE)) + facet_grid(orientation~subject + stream)
#plot data
g<-g+geom_histogram(binwidth=1) + xlim(minSPE,maxSPE)
sz=1
g<-g+ geom_line(data=fitDfs,aes(x=x,y=guessingFreq),color="yellow",size=sz)
g<-g+ geom_line(data=fitDfs,aes(x=x,y=gaussianFreq),color="blue",size=sz)
g<-g+ geom_point(data=fitDfs,aes(x=x,y=combinedFitFreq),color="green",size=sz)
g


df<-data %>% filter(subject=="BE" | subject=="BA")

tit<-"subjs" #Calculate fit separately for each group
quartz(title=tit,width=6,height=6) 

fitDfs<- df %>% group_by(orientation,stream,subject) %>% 
  do(calcFitDataframes(.,minSPE,maxSPE,numItemsInStream))

g=ggplot(df, aes(x=SPE)) + facet_grid(orientation~subject + stream)
#plot data
g<-g+geom_histogram(binwidth=1) + xlim(minSPE,maxSPE)
sz=1
g<-g+ geom_line(data=fitDfs,aes(x=x,y=guessingFreq),color="yellow",size=sz)
g<-g+ geom_line(data=fitDfs,aes(x=x,y=gaussianFreq),color="blue",size=sz)
g<-g+ geom_point(data=fitDfs,aes(x=x,y=combinedFitFreq),color="green",size=sz)
g

df<-data %>% filter(subject=="BE")


tit<-"groups" #Calculate fit separately for each group
quartz(title=tit,width=2.8,height=3.1) 

fitDfs<- df %>% group_by(orientation,stream) %>% 
            do(calcFitDataframes(.,minSPE,maxSPE,numItemsInStream))
  
g=ggplot(df, aes(x=SPE)) + facet_grid(orientation~stream)
#plot data
g<-g+geom_histogram(binwidth=1) + xlim(minSPE,maxSPE)
sz=1
g<-g+ geom_line(data=fitDfs,aes(x=x,y=guessingFreq),color="yellow",size=sz)
g<-g+ geom_line(data=fitDfs,aes(x=x,y=gaussianFreq),color="blue",size=sz)
g<-g+ geom_point(data=fitDfs,aes(x=x,y=combinedFitFreq),color="green",size=sz)
g


estimates<- analyzeOneCondition(df,numItemsInStream)
g<- plotHistWithFit(df$SPE,minSPE,maxSPE,df$targetSP,numItemsInStream,estimates$p1,estimates$p2,estimates$p3)
show(g)



BA22 <- data %>% filter(subject=="BA" & target==2 & condition==2)


df<-BA22
estimates<- analyzeOneCondition(df,numItemsInStream)
g<- plotHistWithFit(df$SPE,minSPE,maxSPE,df$targetSP,numItemsInStream,estimates$p1,estimates$p2,estimates$p3)
show(g)

# BE,2,1
df<- data %>% filter(subject=="BE" & target==2 & condition==1) 
estimates<- analyzeOneCondition(df,numItemsInStream)
g<- plotHistWithFit(df$SPE,minSPE,maxSPE,df$targetSP,numItemsInStream,estimates$p1,estimates$p2,estimates$p3)
show(g) #Looks like efficacy wrong. Problem is that can fit the histogram very well with a tiny precision.
#This looks like a general problem that will often cause precision to be underestimated.

#BO,1,1
df<- data %>% filter(subject=="BO" & target==1 & condition==1) 
estimates<- analyzeOneCondition(df,numItemsInStream)
g<- plotHistWithFit(df$SPE,minSPE,maxSPE,df$targetSP,numItemsInStream,estimates$p1,estimates$p2,estimates$p3)
show(g) #How did BO do it? Never guessed, almost, only twice
print((df$SPE))

fitAndPlotHist(df,minSPE,maxSPE,numItemsInStream)

df %>% do(fitAndPlotHist(.,minSPE,maxSPE,numItemsInStream))

#How do I facet_grid this thing?
#I can certainly plot the histogram - but how to add the curve fits? I guess I need to add them to the
#dataframe before plotting
df<-df %>% filter(subject=="BO")

estimates %>% filter(round(p1,3)==0.280) #BA,2,2 or BE,2,1
#1e-05  4 1e-05 334.385593546 but didn't end up being the winning replicate
#Inspect AD,2,1 and AI,1,2 because very poor fit

