library(ggplot2)
source('seq.R')
all <- getFullMatSeqLunaDate()

plotdf <- all %>% filter(test=='ret') %>% group_by(runno,corseq,agency,subj) %>% mutate(withseq.trial=1:n())

 

p1.allseq <- 
 ggplot(plotdf) +
 aes(x=withseq.trial,y=finalrt,color=subj,group=paste(subj,runno,corseq),) +
 geom_line() +
 geom_point(aes(size=nfingercor,shape=as.factor(allcor)),size=3) +
 facet_grid(agency~subj) +
 theme_bw()
print(p1.allseq)



plotdf.sum <-
 plotdf %>% 
 group_by(subj,agency,withseq.trial) %>% 
 summarise(
  mrt=mean(finalrt,na.rm=T),
  acc=sum(allcor,na.rm=T)/n(),
  corfinger=mean(nfingercor),
  corfingerany=mean(totalfingercor,na.rm=T)
 )


p2.mrt <- 
 ggplot(plotdf.sum) +
 aes(x=withseq.trial,y=mrt,color=agency) +
 geom_line() +
 geom_point(aes(size=acc)) +
 facet_grid(subj~.) +
 theme_bw()
print(p2.mrt)



p3.acc <- 
 ggplot(plotdf.sum) +
 aes(x=withseq.trial,y=acc,color=agency) +
 geom_line() +
 geom_point(aes(size=mrt)) +
 facet_grid(subj~.) +
 theme_bw()
print(p3.acc)


## diff yoke and chontrol
plotdf.contrast<-
  plotdf.sum %>% 
  group_by( subj,withseq.trial) %>%
  arrange(agency) %>%
  summarise_each(funs(diff),-agency)


p4.acc.con <- 
 ggplot(plotdf.contrast) +
 aes(x=withseq.trial,y=acc,color=subj) +
 geom_line() +
 geom_point(aes(size=mrt)) +
 facet_grid(subj~.) +
 ggtitle('choice-yoked constrast') +
 theme_bw()
print(p4.acc.con)



p5.rt.con <- 
 ggplot(plotdf.contrast) +
 aes(x=withseq.trial,y=mrt,color=subj) +
 geom_line() +
 geom_point(aes(size=acc)) +
 facet_grid(subj~.) +
 ggtitle('choice-yoked constrast') +
 theme_bw() +
 scale_y_continuous(limits=c(-1.5,1.5))
print(p5.rt.con)

plotdf.contrast %>% group_by(subj) %>% summarise_each(funs(mean))
