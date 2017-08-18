
#current directory should be analysisInR_postLizzy
#E1 is data.frame created by below file
if (!exists('E')) {
  source('backwardsLtrsLoadRawData.R')
}
E1<-E #don't screw up raw data, as running successively could screw up raw data

#get rid of some cluttering useless variables
E1$condName<-E1$condition
E1$condName[E1$condName == "1"] <- "Canonical"
E1$condName[E1$condName == "2"] <- "Backwards"

#Problem,implausibly high peaks in the across-Ss histogram at -3 , +4, 
#SANITY CHECK LATENCY before gathering to trace the error
#Does response1 refer to the serial position in the stream, or is it a code for a letter,
#with serial position recoverable from letterOrder
require(ggplot2)
#sanity check
g=ggplot(E1,   aes(x=SPE))  
g<-g+facet_grid(condName~.)
g<-g+geom_histogram()
g

require(ggplot2)
#sanity check
g=ggplot(E1,   aes(x=SPE))  
g<-g+facet_grid(condName~target)
g<-g+geom_histogram()
g

#Calculate latency as function of condition
require(dplyr)
s <- E1 %>%
  group_by(condName,target) %>%
  summarise(mean=mean(SPE), se=sd(SPE)/n())
s
#Do statistics on it
require(ez)

aa <- ezANOVA(data=E1, dv=s, between=Group, wid=WID)
cat("F=", aa$ANOVA$F, " p=", aa$ANOVA$p, "\n", sep="")


dodgeAmt=.4
g<-g+stat_summary(fun.data="mean_cl_boot",geom="errorbar",width=.3,conf.int=.95,
                  width=5,size=1) 
g<-g+geom_point(  aes(x=factor(condName), y=thre, 
                      shape=factor(subject), color=factor(ringToQuery)), 
                  position=position_dodge(dodgeAmt), size =2.9   )
g<-g+geom_line(aes(group=interaction(subject,factor(ringToQuery))),
               position=position_dodge(width=dodgeAmt))