#!/usr/bin/env Rscript
library(ggplot2)
library(dplyr)
library(gridExtra)
source('parse.R')

## ALL DATA
all.all <- getAllData() 
#all.all <-  read.table('img/all.csv',sep=",",header=T)

## 
# look at incorrect and correct trials?
# NOTE: need to rerun normdf, rtsumdf , and agentdiffdf  if switching
CORRECTONLY <- T

# change what we plot based on if we only look at correct trials
all.coronly <- onlyCorrect(all.all,'first') 

d.all <- function(){
  if(CORRECTONLY)
     all.coronly
  else
     all.all
}

normdf <- function(){
 d.all() %>%
 group_by(subj,agegrp,runno,agency,seqno) %>%
 arrange(subj,runno,trial) %>% 
 mutate( firstrt4=first(rt4), normrt4= rt4 - firstrt4  )

}

# TODO: consider removing outlier RTs
# with isin2sd(rt4) 


## collapse across like blocks for accuracy
accdf <- 
 all.all %>%
 group_by(subj,age,sex,agegrp,
          agency,
          nseenseq) %>%
 arrange(subj,runno,trial) %>%
 summarise(
    acc=mean(allcor,na.rm=T),
    mrt4=mean(rt4,na.rm=T),
    n   = n()
 )

### plot ###

# ACCURACY
p1.acc <-
 ggplot(accdf) +
 aes(x=nseenseq,y=acc,group=agency,color=agency) +
 geom_jitter(alpha=.2,width=.2,height=0) + 
 geom_smooth(method='lm') +
 facet_grid(agegrp~.) + 
 ggtitle('accuracy') + 
 theme_bw()
if(!CORRECTONLY) savimg(p1.acc) # acc doesn't matter if only looking at correct

# as a histogram
p1.acc.hist <- 
 ggplot(all.all %>% group_by(agegrp,agency,nseenseq) %>% summarise(avgacc=sum(allcor,na.rm=T)/n()) ) +
 aes(x=nseenseq,fill=agency,y=avgacc) +
 geom_bar(stat='identity',position='dodge') +
 theme_bw() +
 facet_grid(agegrp~.)
#print(p1.acc.hist)

## RT

p1.rt4 <-
 ggplot(d.all()) +
 aes(x=nseenseq,y=rt4,group=agency,color=agency) +
 geom_jitter(alpha=.2,width=.2,height=0) + 
 geom_smooth(method='lm') +
 facet_grid(agegrp~.) + 
 ggtitle('RT4 (last push)') + 
 theme_bw()
savimg(p1.rt4)

## look at difference between first and last push
p2.rt4.rt1 <-
 ggplot(d.all()) +
 aes(x=nseenseq,y=rt4-rt1,group=agency,color=agency) +
 geom_jitter(alpha=.2,width=.2,height=0) + 
 geom_smooth(method='lm') +
 facet_grid(agegrp~.) + 
 ggtitle('RT4 - RT1') + 
 theme_bw()
savimg(p2.rt4.rt1)

## "normalize" RT by subtracting first RT time
#  first rt for each seqno in each agency block in each run

p3.rt4.norm <-
 ggplot(normdf()) +
 aes(x=nseenseq,y=normrt4,group=agency,color=agency) +
 #geom_jitter(alpha=.2,width=.2,height=0) + 
 geom_smooth(method='lm') +
 facet_grid(agegrp~.) + 
 ggtitle('RT4 - first RT4') + 
 theme_bw()
savimg(p3.rt4.norm)


p3.rt4.norm.nofacet <-
 ggplot(normdf()) +
 aes(x=nseenseq,y=normrt4,color=agegrp,linetype=agency,group=paste(agency,agegrp)) +
 #geom_jitter(alpha=.2,width=.2,height=0) + 
 geom_smooth(method='lm') +
 ggtitle('RT4 - first RT4') + 
 theme_bw()
print(p3.rt4.norm.nofacet)
savimg(p3.rt4.norm.nofacet)

## slopes
rtsumdf <- 
 normdf() %>%
 group_by(subj,age,sex,agegrp,
          agency,
          nseenseq) %>%
 arrange(subj,runno,trial) %>%
 summarise( mnormrt4=mean(normrt4,na.rm=T)) 

p4.rt4.norm.nofacet.mean <-
 ggplot(rtsumdf) +
 aes(x=nseenseq,y=mnormrt4,color=agegrp,linetype=agency,group=paste(agency,agegrp)) +
 #geom_jitter(alpha=.2,width=.2,height=0) + 
 geom_smooth(method='lm') +
 ggtitle('mean RT4 - first RT4') + 
 theme_bw()
savimg(p4.rt4.norm.nofacet.mean)

# make yoked and choice their own column
# so we can easily subtract them
agentdiffdf <-
 rtsumdf %>%
 spread(agency,mnormrt4)

p4.slope.diff <-
 ggplot(agentdiffdf ) + 
 aes(x=nseenseq,y=choice-yoked,color=agegrp) +
 geom_jitter(alpha=.3,width=.2,height=0) + 
 stat_smooth(method='lm') +
 ggtitle('rt diff per trial in mean RT4 - first RT4') + 
 theme_bw()
savimg(p4.slope.diff)



## bea scatter plots

rtrate <- 
 normdf() %>%
 select(subj,age,agegrp,sex,runno,agency,nseenseq,trial,rt4,seqno,firstrt4,normrt4) %>%
 group_by(subj,age,sex,agegrp,
          runno,agency,seqno) %>%
 filter(nseenseq == max(nseenseq) & nseenseq > 1 )

## slopes
getslope <- function(y,x) {
 lm(data=data.frame(y,x),y~x)$coefficients[2]
}
rtrate.slope <- 
 normdf() %>%
 filter(!is.na(firstrt4)) %>%
 select(subj,age,agegrp,sex,runno,agency,nseenseq,trial,rt4,seqno,firstrt4,normrt4) %>%
 group_by(subj,age,agegrp, sex,
          agency) %>%
 summarise(slope=getslope(normrt4,nseenseq) )

### 

#
p5.ratebar <- 
 ggplot(rtrate) +
 aes(x=agegrp,y=normrt4,color=agency) +
 #stat_summary(fun.y = mean, geom = "bar",position="dodge",aes(fill=agency)) +
 geom_boxplot()+
 geom_jitter(position=position_jitterdodge(.9,jitter.width=.2,jitter.height=0),alpha=.2)  +
 #scale_y_continuous(limits=c(-1,1)) +
 ggtitle('RT learning rate: rep last-first')+
 theme_bw()
print(p5.ratebar)

#
p5.ratebar.slope <- 
 ggplot(rtrate.slope) +
 aes(x=agegrp,y=slope,color=agency) +
 #stat_summary(fun.y = mean, geom = "bar",position="dodge",aes(fill=agency)) +
 geom_boxplot()+
 geom_jitter(position=position_jitterdodge(.9,jitter.width=.2,jitter.height=0),alpha=.2)  +
 ggtitle('RT learning rate: slope rt4 ~ rep') +
 #scale_color_manual(values=c("black","black"))+
 theme_bw()
print(p5.ratebar.slope )
savimg(p5.ratebar.slope )

p5.learrate.rt4norm.slopediff <- grid.arrange(p5.ratebar,p5.ratebar.slope)
savimg(p5.learrate.rt4norm.slopediff)


###  ACCURACY

## data
accrate.slope <- 
 accdf  %>%
 #select(subj,age,agegrp,sex,agency,nseenseq,trial,rt4,seqno,firstrt4,normrt4) %>%
 group_by(subj,age,agegrp, sex,
          agency) %>%
 summarise(slope=getslope(acc,nseenseq) )

# TODO: dont collapse accross seqno ?
accrate <- 
 accdf %>%
 select(-mrt4,-n) %>%
 group_by(subj,age,sex,agegrp,
          agency) %>%
 filter(nseenseq == max(nseenseq) | nseenseq == 1 ) %>%
 mutate(nseenseq = ifelse(nseenseq==1,'first','last')) %>%
 spread(nseenseq,acc)

## plots

p5.ratebar.acc <- 
 ggplot(accrate) +
 aes(x=agegrp,y=last-first,color=agency) +
 #stat_summary(fun.y = mean, geom = "bar",position="dodge",aes(fill=agency)) +
 geom_boxplot()+
 geom_jitter(position=position_jitterdodge(.9,jitter.width=.2,jitter.height=0),alpha=.2)  +
 #scale_y_continuous(limits=c(-1,1)) +
 ggtitle('Acc learning rate: rep last-first')+
 theme_bw()
print(p5.ratebar.acc)

#
p5.ratebar.slope.acc <- 
 ggplot(accrate.slope) +
 aes(x=agegrp,y=slope,color=agency) +
 #stat_summary(fun.y = mean, geom = "bar",position="dodge",aes(fill=agency)) +
 geom_boxplot()+
 geom_jitter(position=position_jitterdodge(.9,jitter.width=.2,jitter.height=0),alpha=.2)  +
 ggtitle('Acc learning rate: slope acc ~ rep') +
 #scale_color_manual(values=c("black","black"))+
 theme_bw()
print(p5.ratebar.slope.acc )
savimg(p5.ratebar.slope.acc )


p5.learrate.acc.slopediff <- grid.arrange(p5.ratebar.acc,p5.ratebar.slope.acc)
savimg(p5.learrate.acc.slopediff)


######
## box plots of the same thing

p6.bar.ratebar <- 
 ggplot(rtrate) +
 aes(x=agegrp,y=normrt4,color=agency) +
 stat_summary(fun.y = mean, geom = "bar",position="dodge",aes(fill=agency)) +
 #geom_jitter(position=position_jitterdodge(.9,jitter.width=.2,jitter.height=0),alpha=.2)  +
 #scale_y_continuous(limits=c(-1,1)) +
 ggtitle('RT learning rate: rep last-first')+
 theme_bw()
print(p6.bar.ratebar)

#
p6.bar.ratebar.slope <- 
 ggplot(rtrate.slope) +
 aes(x=agegrp,y=slope,color=agency) +
 stat_summary(fun.y = mean, geom = "bar",position="dodge",aes(fill=agency)) +
 #geom_jitter(position=position_jitterdodge(.9,jitter.width=.2,jitter.height=0),alpha=.2)  +
 ggtitle('RT learning rate: slope rt4 ~ rep') +
 #scale_color_manual(values=c("black","black"))+
 theme_bw()
print(p6.bar.ratebar.slope )
#savimg(p6.bar.ratebar.slope )

p6.bar.learrate.rt4norm.slopediff <- grid.arrange(p6.bar.ratebar,p6.bar.ratebar.slope)
savimg(p6.bar.learrate.rt4norm.slopediff)


# ACCURACY

p6.bar.ratebar.acc <- 
 ggplot(accrate) +
 aes(x=agegrp,y=last-first,color=agency) +
 stat_summary(fun.y = mean, geom = "bar",position="dodge",aes(fill=agency)) +
 #geom_boxplot()+
 #geom_jitter(position=position_jitterdodge(.9,jitter.width=.2,jitter.height=0),alpha=.2)  +
 #scale_y_continuous(limits=c(-1,1)) +
 ggtitle('Acc learning rate: rep last-first')+
 theme_bw()
print(p6.bar.ratebar.acc)

#
p6.bar.ratebar.slope.acc <- 
 ggplot(accrate.slope) +
 aes(x=agegrp,y=slope,color=agency) +
 stat_summary(fun.y = mean, geom = "bar",position="dodge",aes(fill=agency)) +
 #geom_jitter(position=position_jitterdodge(.9,jitter.width=.2,jitter.height=0),alpha=.2)  +
 ggtitle('Acc learning rate: slope acc ~ rep') +
 #scale_color_manual(values=c("black","black"))+
 theme_bw()
print(p6.bar.ratebar.slope.acc )
#savimg(p6.bar.ratebar.slope.acc )


p6.bar.learrate.acc.slopediff <- grid.arrange(p6.bar.ratebar.acc,p6.bar.ratebar.slope.acc)
savimg(p6.bar.learrate.acc.slopediff)
