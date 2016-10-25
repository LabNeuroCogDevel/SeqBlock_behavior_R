library(reshape2)
library(ggplot2)
library(magrittr)
library(tidyr)
library(dplyr)

#
# use perl and a little mutating to get the log file into a dataframe
#  - BType   -- 'control','yolked', and 'control'
#  - RType   -- 'ret' or 'ret_test'
#  - imgno   -- unique image number (and also key press sequence) identification
#  - seenno  -- the number of times that unique image has been seen -- different from trial number. test always the last 2 seenno's
#
parselog <- function(fname) {
  #fname <- "../../SeqBlock2/logs/log_sophie2_2016.10.25-14.09.44.txt"
  cmd<-sprintf('./parselog.pl < %s',fname)
  d <-  
   (p<-pipe(cmd,open="r" )) %>%
   #(p<-pipe("./parselog.pl < ../../SeqBlock2/logs/log_Scott_2016.10.25-12.21.42.txt",open="r" )) %>%
   read.table %>% 
   set_colnames(c('BType','RType','Run','Block','Trial','Seq','Resp','Cor','RT1','RT2','RT3','RT4')) %>%
   mutate(Cor=as.factor(Cor),
          imgno=as.factor( (Run-1)*6 + (Block-1)*2 + Seq )
          ) %>%
   group_by(imgno) %>% arrange(Run,Block,RType,Trial) %>%
   mutate(seenno=1:n())
  # close pipe
  close(p)
  # and logfile name to dataframe so we can concat with other data.frames
  d$log <- gsub('.txt','',basename(fname))
  # make sure we are sending the full dataframe back
  return(d)
}


d <- parselog("../../SeqBlock2/logs/log_sophie2_2016.10.25-14.09.44.txt")

# # long format for each RT
# d.m <- d %>% 
#  melt(id.vars=c('BType','RType','Run','Block','Trial','Seq','Resp','Cor','seenno','imgno'),variable.name='pushno',value.name='RT') %>%
#  mutate(pushno=as.numeric(gsub('RT','',pushno)) )


d.plot <- d %>% mutate(RT=RT4)# %>% filter(RType=='ret') 
p <- 
 ggplot(d.plot ) + 
 aes(x=seenno,y=RT,color=imgno,group=imgno,shape=Cor) +
 geom_line()  +
 geom_smooth(aes(group=BType),method='lm')+
 geom_point(size=3) +
 facet_grid(BType~.) +
 theme_bw()
print(p)
