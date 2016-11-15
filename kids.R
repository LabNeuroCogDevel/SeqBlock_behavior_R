library(ggplot2)
source('seq.R')

# get data, remove junk subject
all <- getFullMatSeqLunaDate() %>%
 # remove junk subject
 filter(subj!='11579_20161111'
      # remove outliers (maybe make per subj sd)
      #,isin2sd(rt4) 
 )


testdf <- all %>% filter(test=='test.ret') 

p1.testacc <-
 ggplot(testdf %>% tidyr::gather(finger,rt,rt1:rt4,rtdiff) ) +
 aes(x=as.factor(seqno),y=rt,color=subj,shape=as.factor(allcor),group=paste(seqno,subj))  +
 geom_boxplot() +
 theme_bw() + facet_grid(finger~allcor) +
 ggtitle('correct vs incorrect per subject for each trial')
print(p1.testacc)
ggsave('img2/p1_testacc.png')


# add a column for if test is correct
plotdf <- all %>% addtestcor  

p1.allseq <- 
 ggplot(plotdf) +
 aes(x=nseenseq,y=rt4,color=as.factor(testcor),group=paste(subj,runno,corseq)) +
 geom_line() +
 geom_point(aes(size=nfingercor,shape=as.factor(allcor)),size=3) +
 stat_smooth(data=plotdf%>%filter(allcor==1),method='lm',aes(color=NULL,group=NULL)) +
 facet_grid(agency~subj) +
 theme_bw() +
 scale_y_continuous(limits=c(1,3)) +
 ggtitle('rt4 by seq appearance across subj for each block')
print(p1.allseq)
ggsave('img2/p1_allseq.png')


#plotdf.cnt <- plotdf %>% group_by(subj,agency,seengap,allcor,seqno) %>% summarise(cnt=n())
p1.seengap <-
 ggplot(plotdf) +
 aes(x=seengap,fill=as.factor(allcor)) +
 geom_histogram(position='dodge') +
 facet_grid(agency*seqno~subj) +
 theme_bw() +
 scale_y_continuous(breaks=seq(0,10,2)) +
  ggtitle('as nback')
print(p1.seengap)
ggsave('img2/p1_seengap.png')



plotdf.sum <-
 plotdf %>% 
 group_by(subj,agency,nseenseq) %>% 
 summarise(
  mrt=mean(rt4,na.rm=T),
  acc=sum(allcor,na.rm=T)/n(),
  corfinger=mean(nfingercor),
  corfingerany=mean(totalfingercor,na.rm=T)
 )


p2.mrt <- 
 ggplot(plotdf.sum) +
 aes(x=nseenseq,y=mrt,color=agency) +
 geom_line() +
 geom_point(aes(size=acc)) +
 facet_grid(subj~.) +
 theme_bw() + ggtitle('mean rt4')
print(p2.mrt)
ggsave('img2/p2_meanrt.png')



p3.acc <- 
 ggplot(plotdf.sum) +
 aes(x=nseenseq,y=acc,color=agency) +
 geom_line() +
 geom_point(aes(size=mrt)) +
 facet_grid(subj~.) +
 theme_bw() + ggtitle('mean acc')
print(p3.acc)
ggsave('img2/p3_meanacc.png')


## diff yoke and chontrol
plotdf.contrast<-
  plotdf.sum %>% 
  group_by( subj,nseenseq) %>%
  arrange(agency) %>%
  summarise_each(funs(diff),-agency)


p4.acc.con <- 
 ggplot(plotdf.contrast) +
 aes(x=nseenseq,y=acc,color=subj) +
 geom_line() +
 geom_point(aes(size=mrt)) +
 facet_grid(subj~.) +
 ggtitle('choice-yoked contrast') +
 theme_bw()
print(p4.acc.con)
ggsave('img2/p4_contract_acc.png')



p5.rt.con <- 
 ggplot(plotdf.contrast) +
 aes(x=nseenseq,y=mrt,color=subj) +
 geom_line() +
 geom_point(aes(size=acc)) +
 facet_grid(subj~.) +
 ggtitle('choice-yoked contrast') +
 theme_bw() +
 scale_y_continuous(limits=c(-1.5,1.5))
print(p5.rt.con)
ggsave('img2/p5_contract_mrt4.png')

plotdf.contrast %>% group_by(subj) %>% summarise_each(funs(mean))


smry <- 
 plotdf %>% 
 group_by(subj,age,sex,testcor,runno,agency ) %>% 
 summarise(
   n=length(na.omit(rt4)),
   acc=mean(allcor,na.rm=T),
   mrt4=mean(rt4,na.rm=T),
   mrt1=mean(rt1,na.rm=T),
   rtsd=sd(rt4,na.rm=T)
   ) %>% 
 ungroup %>%
 mutate(testcor=factor(testcor,labels=list("0"="failedtest","1"="correcttest")))
print(smry)

p6.smry_age <- 
 ggplot(smry %>% tidyr::gather(sumstat,val,mrt4,mrt1,acc) ) + 
 aes(x=as.factor(age),y=val)+
 geom_boxplot() + geom_jitter(height=0,width=.4,aes(color=subj,size=n,shape=sex)) +
 theme_bw() +
 facet_grid(sumstat~testcor,scales="free") +
 ggtitle('summary stats by age')
print(p6.smry_age)
ggsave('img2/p6_summaryByAge.png')



