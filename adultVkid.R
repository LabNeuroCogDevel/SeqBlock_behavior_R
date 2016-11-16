#!/usr/bin/env Rscript

source('parse.R')

# quick function to save images
savimg <- function(p) {
 n<-gsub('\\.','_',deparse(substitute(p)))
 print(p)
 ggsave(file=sprintf('img/kidsvadult/%s.png',n))
}

## ALL DATA
pilot <- getPilots()
kids <- getFullMatSeqLunaDate() %>%
 # remove junk subject
 filter(subj!='11579_20161111'
      # remove outliers (maybe make per subj sd)
      #,isin2sd(rt4) 
 )
all <- rbind(as.data.frame(kids),pilot)
all$agegrp <- cut(as.numeric(all$age),breaks=c(0,18,Inf),labels=c('kid','adult'))

all <- all %>% filter(agency!='control', test=='ret')

## collapse accross like blocks for accuracy
accdf <- 
 all %>%
 group_by(subj,age,sex,agegrp,
          agency,
          nseenseq) %>%
 arrange(subj,runno,trial) %>%
 summarise(acc=mean(allcor,na.rm=T))

### plot ###

## not corrected
# accuracy using summary
p1.acc <-
 ggplot(accdf) +
 aes(x=nseenseq,y=acc,group=agency,color=agency) +
 geom_jitter(alpha=.2,width=.2,height=0) + 
 geom_smooth(method='lm') +
 facet_grid(agegrp~.) + 
 ggtitle('accuracy') + 
 theme_bw()
savimg(p1.acc)

p1.rt4 <-
 ggplot(all) +
 aes(x=nseenseq,y=rt4,group=agency,color=agency) +
 geom_jitter(alpha=.2,width=.2,height=0) + 
 geom_smooth(method='lm') +
 facet_grid(agegrp~.) + 
 ggtitle('RT4 (last push)') + 
 theme_bw()
savimg(p1.rt4)

## look at difference between first and last push
p2.rt4.rt1 <-
 ggplot(all) +
 aes(x=nseenseq,y=rt4-rt1,group=agency,color=agency) +
 geom_jitter(alpha=.2,width=.2,height=0) + 
 geom_smooth(method='lm') +
 facet_grid(agegrp~.) + 
 ggtitle('RT4 - RT1') + 
 theme_bw()
savimg(p2.rt4.rt1)

## "normalize" RT by subtracting first RT time
#  first rt for each seqno in each agency block in each run
normdf <-
 all %>%
 group_by(subj,agegrp,runno,agency,seqno) %>%
 arrange(subj,runno,trial) %>% 
 mutate( firstrt4=first(rt4), normrt4= rt4 - firstrt4  )


p3.rt4.norm <-
 ggplot(normdf) +
 aes(x=nseenseq,y=normrt4,group=agency,color=agency) +
 geom_jitter(alpha=.2,width=.2,height=0) + 
 geom_smooth(method='lm') +
 facet_grid(agegrp~.) + 
 ggtitle('RT4 - first RT4') + 
 theme_bw()
savimg(p3.rt4.norm)
