library(reshape2)
library(ggplot2)
library(magrittr)
library(tidyr)
library(dplyr)

######## FUNCTIONS
source('parse.R')

#d <- parselog("../logs/log_sophie2_2016.10.25-14.09.44.txt")
#exampleplot(d)

pilot1 <- getSeq2Pilots()
pilot2 <- getSeq3Pilots()
pilotkids <- getFullSeqLunaDate()

#d.all <- rbind(d.all,d.all.prev)
d.all <- rbind(pilot2,pilotkids)


## rt by nseen for each image across btype
p <- 
  ggplot(d.all %>% mutate(RT=RT4/4) )+ 
  aes(x=seenno,y=RT,color=subj,group=paste(imgno,subj),shape=Cor) +
  geom_line()  +
  #geom_smooth(aes(group=BType),method='lm')+
  geom_point(size=3) +
  facet_grid(BType~.) +
  theme_bw() +ggtitle('RT4/4')
print(p)
ggsave('img/allrt.png',p)




#### choice vs yoke
#### we want to add a new fake block type yoked-control
#   and only want the first 4 trials of that
# 1) probably unnecessary: break out the image for each block as either
#    first seen or second seen
d.diff.init <-
  d.all %>% 
  filter(RType=='ret',
         seenno<=4,
         BType%in%c('choice','yoked')) %>%
  group_by(subj,Block,Run,subj) %>%
  # first imgno seen in block 
  mutate(imgsetno= (first(imgno)!=imgno)+1) %>%
  # tidy up a bit
  ungroup() %>%
  select(RT=RT4,Cor,seenno,subj,BType,imgsetno)


# seenno    subj         acc           mRT
# acc and mRT are of the choice-yoke
d.diff <- 
 d.diff.init %>% 
 group_by(seenno,subj,BType) %>% 
 summarise(mRT=mean(RT,na.rm=T), acc=mean(Cor==1) ) %>%
 gather(measure,val,-seenno,-subj,-BType) %>% 
 spread(BType,val) %>% 
 mutate(diff=choice-yoked) %>% 
 select(-choice,-yoked) %>% 
 spread(measure,diff) %>%
 mutate(BType='choiceMinusYoke') 

## individual differences
p.d.acc <- ggplot(d.diff) +
 aes(x=seenno,y=acc,color=subj) +
 geom_line() +
 theme_bw() +
 geom_smooth(aes(color=NULL))+
 ggtitle('Acc: choice-yoked (higher: better on choice)')
print(p.d.acc)
ggsave('img/acc_diff.png')


p.d.RT <- ggplot(d.diff) +
 aes(x=seenno,y=-1*mRT,color=subj) +
 geom_line() +
 geom_smooth(aes(color=NULL))+
 theme_bw() +
 ggtitle('RT: yoked-choice (higher: better on choice)') +
 labs(y=expression(Delta*' RT'))
print(p.d.RT)
ggsave('img/mRT_diff.png')




### accuracy
# by seen number # accross btype (yoked,control,..) for each subject
acc <- d.all %>% 
  filter(RType=='ret') %>% 
  group_by(seenno,subj,BType) %>% 
  summarise(acc=mean(Cor==1,na.rm=T))


p.a <- 
 ggplot(acc) + 
 aes(x=seenno,y=acc,color=subj) + #,linetype=BType) + 
 #geom_smooth(aes(group=BType),method='lm')+
 geom_line() + #size=4)+ 
 facet_grid(BType~.) +
 theme_bw() +ggtitle('Accuracy')
print(p.a)
ggsave("acc.png",p.a)


## add see accuracy for all blocks AND diff
accAndDiff <- 
 d.diff %>% 
 select(-mRT) %>%
 rbind(acc)     

p.a.a <- 
 ggplot(accAndDiff) +
 aes(x=seenno,y=acc,color=subj) + #,linetype=BType) + 
 #geom_smooth(aes(group=BType),method='lm')+
 geom_line() + #size=4)+ 
 facet_grid(BType~.) +
 theme_bw() +ggtitle('Accuracy')
print(p.a.a)
ggsave("acc_allAndDiff.png",p.a.a)
 

#### RT

rtavg <- d.all %>% 
  filter(RType=='ret') %>% 
  group_by(seenno,subj,BType) %>% 
  summarise(RT=mean(RT4,na.rm=T))


p.rt <- 
 ggplot(rtavg) + 
 aes(x=seenno,y=RT,color=subj) + #,linetype=BType) + 
 geom_smooth(aes(group=BType))+
 geom_line() + #size=4)+ 
 facet_grid(BType~.) +
 theme_bw()+ggtitle('Avg RT4')
print(p.rt)
ggsave("img/avg_rt.png",p.rt)


RTAndDiff <- 
 d.diff %>% 
 select(-acc,RT=mRT) %>%
 rbind(rtavg)     
p.rt.a <- 
 ggplot(RTAndDiff) + 
 aes(x=seenno,y=RT,color=subj) + #,linetype=BType) + 
 #geom_smooth(aes(group=BType))+
 geom_line() + #size=4)+ 
 facet_grid(BType~.) +
 theme_bw()+ggtitle('Avg RT4')
print(p.rt.a)
ggsave("img/avg_rt.png",p.rt.a)

