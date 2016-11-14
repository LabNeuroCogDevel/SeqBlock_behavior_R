### parselog ###

# use perl and a little mutating to get the log file into a dataframe
#  - BType   -- 'control','yoked', and 'control'
#  - RType   -- 'ret' or 'ret_test'
#  - imgno   -- unique image number (and also key press sequence) identification
#  - seenno  -- the number of times that unique image has been seen -- different from trial number. test always the last 2 seenno's
require(magrittr)
require(dplyr)

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
  # fix spelling (or conceptual) mistake
  d$BType = gsub('yolked','yoked',d$BType)

  # get subject from logfile name 
  d$subj <- gsub('.*log_(.*)_2016.*','\\1',d$log)


  # make sure we are sending the full dataframe back
  return(d)

  # # long format for each RT
  # d.m <- d %>% 
  #  melt(id.vars=c('BType','RType','Run','Block','Trial','Seq','Resp','Cor','seenno','imgno'),
  #       variable.name='pushno',
  #       value.name='RT') %>%
  #  mutate(pushno=as.numeric(gsub('RT','',pushno)) )
}

## example plot
# RT by seenno for each block type
exampleplot <- function(d) {
 d.plot <- d %>% mutate(RT=RT4)# %>% filter(RType=='ret') 
 p <- 
  ggplot(d.plot ) + 
  aes(x=seenno,y=RT,color=imgno,group=imgno,shape=Cor) +
  geom_line()  +
  geom_smooth(aes(group=BType),method='lm')+
  geom_point(size=3) +
  facet_grid(BType~.) +
  theme_bw()+ggtitle('RT4')
 print(p)
}
##########

# where can we find the text logs
getlogdir <- function(versionno='3') {
  # where are logs stored
  # -- if we are running from Finn's personal dir, then its back a dir
  #    otherwise use the mount on will's computer                    

  hostname <- Sys.info()['nodename']
  beares <- switch(hostname,
    "reese-loeff114"="/mnt/B/bea_res",
    "//oacres1/rcn1/bea_res"
  )
   
  abslogpath <- sprintf("%s/Personal/Finn/SeqBlock%s/logs/",beares,versionno)

  # can we use a relative path, or do we need to try full absolute path?
  taskdirpatt <- sprintf('SeqBlock%s/[^/]*$',versionno) 
  areintaskdir <-  grepl(taskdirpatt,getwd())
  logdir <- ifelse(areintaskdir, '../logs/', abslogpath)
  
  if(!file.exists(logdir) ) { stop('logdir does not exist: ',logdir) }

  return(logdir)
}

parseall <- function(wanttxtlist) {
  d <- 
   lapply(wanttxtlist,FUN=parselog) %>% 
   bind_rows() %>% 
   filter(BType != 'control')
}

alltxtfiles <- function(dir) {
  alltxts  <- Sys.glob(paste0(dir,"*txt"))
}
allmatfiles <- function(dir) {
  alltxts  <- Sys.glob(paste0(dir,"*mat"))
}

getSeq <- function(logdir,pattern) {
  # get list of all text files (logs)
  alltxts  <- alltxtfiles(logdir)

  # only take the ones that are good
  wanttxt <- grep(pattern,alltxts,value=T)

  # parse each log, bind them all together, remove all Controls
  parseall(wanttxt)
}

getFullSeqLunaDate <- function() {
 wanttxt <- Filter( 
  function(x){
   file.info(x)$size>10*5 & 
   !grepl('test',x) &
   grepl('[0-9]{5}_[0-9]{8}',x)
   }, 
   alltxtfiles(getlogdir('3'))
 )
 parseall(wanttxt)
}

getFullMatSeqLunaDate <- function() {
 want <- Filter( 
  function(x){
   file.info(x)$size>2000 & 
   !grepl('test',x) &
   grepl('[0-9]{5}_[0-9]{8}',x)
   }, 
   allmatfiles(getlogdir('3'))
 )
 lapply(FUN=readmat,want) %>% bind_rows() %>% perfinger2pertrial()
}



getSeq2Pilots <- function() {
  d<-getSeq(getlogdir('2'),'deepu|will|Scott|julia|sophie2|jen')
  #write.table(wanttxt.prev,file="taskFilesToRead.txt",row.names=F,col.names=F)
  #logdir <- getlogdir('2')
  ## get list of all text files (logs)
  #alltxt.prev  <- Sys.glob(paste0(logdir,"*txt"))
  ## only take the ones that are good
  #wanttxt.prev <- grep('deepu|will|Scott|julia|sophie2|jen',alltxt.prev,value=T)

  #write.table(wanttxt.prev,file="taskFilesToRead.txt",row.names=F,col.names=F)
  #
  #d.all.prev <- 
  # lapply(wanttxt.prev,FUN=parselog) %>% 
  # bind_rows() %>% 
  # filter(BType != 'control')
}

getSeq3Pilots <- function(patt) {
  d<-getSeq(getlogdir('3'),'hemali|Ruth')
}



ages <- rbind(
 # kid pilots
 c('11579_20161111', 12), 
 # lab people pilot
 c('will',   30 ), 
 c('deepu',  31 ), # *
 c('Scott',  31 ), # *
 c('julia',  25 ), # *
 c('sophie2',23 ), # *
 c('jen',    25 ), # *
 c('hemali', 19 ), # *
 c('Ruth',   19 )  # *
) %>% data.frame %>% set_colnames(c('subj','age'))



addnas <- function(v,n) {
 spad <- sapply(1:n,function(x){ifelse(is.na(v[x]),NA,v[x])})
}
splitnums <- function(seq,n=NULL){
 s<-strsplit(as.character(seq),'')[[1]]
 if(is.null(n))  n <- length(s)
 spad <- addnas(s,n)
 return(spad)
}

#matfile <- '/mnt/B/bea_res/Personal/Finn/SeqBlock3/logs/log_will_2016.10.25-10.09.25.mat'
#matfile <- '/mnt/B/bea_res/Personal/Finn/SeqBlock3/logs/log_11579_20161111_2016.11.11-10.17.51.mat'
readmat <- function(matfile,subjid=NULL) {
 #require('R.matlab')
 m<-R.matlab::readMat(matfile)
 #  in maltab:
 # respseq    = m.tasklog.response(4,2,8).ret.response, 
 # trialseqno = m.tasklog.response(4,2,8).ret.sequence
 # correctseq = m.s.sequences(4,2,trialseqno),
 # blocktype  = m.tasklog.blockSeq{5}(2)


 seqidx  <- which(grepl('sequences',rownames(m$s)))
 fingerseqs <- m$s[[seqidx ]]
 #same as: seq <- m$s[9]

 blockidx   <- which(grepl('blockSeq',rownames(m$tasklog)))
 blocktypes <- m$tasklog[[blockidx]]


 # bulk of the info is with 'response'
 respidx <- which(grepl('response',rownames(m$tasklog)))
 pushseq <- m$tasklog[[respidx]]
 #
 # dim(pushseq)
 #  2 5 2 8
 #  | | | \__ within block trial num (for 'test.ret' [1st dim.], only 2 of these instead of 8)
 #  | | \____ yoked or control (mini-block)
 #  | \______ block (run) number
 #  \________ view/test type (ret, or ret.test)

 # order by block,min-block,trial,view: 2,3,4,1

 # pushseq
 #   , , 1, 1
 #            [,1]   [,2]   [,3]   [,4]   [,5]  
 #   ret      List,4 List,4 List,4 List,4 List,4
 #   test.ret List,4 List,4 List,4 List,4 List,4
 #
 #
 # pushseq[,1,1,1]$test.ret
 #  , , 1
 #  
 #           [,1]     
 #  response "5231"   
 #  respeval 0        
 #  rtvec    Numeric,4
 #  sequence 1  



 # all combinations of 1:dimlen for each dimension
 # --same as: idxs <- as.matrix(do.call(expand.grid,sapply(dim(pushseq),function(x){list(1:x)})))
 idxsall <- dim(pushseq) %>% sapply(function(x){list(1:x)} ) %>% do.call(what=expand.grid) %>% as.matrix
 # need to order it how matlab would
 idxordered <- order(
   idxsall[,2]*10^3+
   idxsall[,3]*10^2+
   idxsall[,1]*10^1+
   idxsall[,4]*10^0+
   0 # here to make moving things easier (always have + at the end)
   ) # order by block,min-block,view,trial: 2,3,1,4
 
 idxs <- idxsall[idxordered , ]

 # remove empty test blocks
 # NB hardcoded testblocks==2 and number of tests==2 should be okay
 #    done so trial count is continous
 idxs <- idxs[ ! (idxs[,1]==2 & idxs[,4]>2 ) , ]

 # and get all of the responses
 allpushes <- pushseq[idxs]

 bigdf<- lapply(1:dim(idxs)[1],function(di){


  # 'ret' or 'ret.test': name of trial '
  viewtype <- names(  pushseq[ idxs[di,1],idxs[di,2],idxs[di,3],idxs[di,4]] ) 

  # this trial's keypresses
  x <- allpushes[[di]] # same as: x <- pushseq[t(idxs[di,])]
  #print(x)

  # if this never happend (i.e. 3:8 on ret.test) skip along
  if(is.null(x)) return(NULL)

  seqidxnum <- x[[4]][1] # 1 or 2
  iscor     <- x[[2]][1] # 0 or 1

  # see pushseq dim description
  tinfo <- data.frame(t(unlist(idxs[di,])))
  names(tinfo) <- c('viewno','runno','agencyno','trialno') 
  #                   1/2      1:5     1:2       1:8


  # in what order should fingers have been pushed
  # stored in 3-d matrix
  #corseq <- fingerseqs[ idxs[di,2], idxs[di,1], seqidxnum ]
  # called from matlab like: s.sequences(runnum, blocki, thisTrialSeq) -- where here fingerseq == sequences
  corseq <- fingerseqs[ tinfo$runno , tinfo$agencyno , seqidxnum ]
  # turn 1234 into 1,2,3,4
  corseql <- splitnums( corseq )

  # should always be 4, but maybe we'll change it at some point
  # redudant to do this for every trial
  numInSeq <- length(corseql)

  
  # what was the order of actual finger pushes
  # NAs for no pushes
  respseq <- splitnums( x[[1]][1], numInSeq )
  # how fast did we push the buttons?
  resprt <- addnas(x[[3]], numInSeq )
 
  btype <- unlist(blocktypes[[ tinfo$runno ]])[tinfo$agencyno ]

  # put all this info into a data frame
  d <- data.frame(
        # repeated for each finger press
        trial     = di,
        runno     = tinfo$runno,
        trial.within=tinfo$trialno,
        #typeno    = tinfo$agencyno, # numeric version of to btype
        #viewno    = tinfo$viewno,   # numeric version of viewtype
        agency    = btype, 
        test      = viewtype,  # see also: tinfo$viewno or unname(idxs[di,1]),
        seqidxnum = seqidxnum, # 1 or 2 -- probably not important
        corseq    = corseq,
        allcor    = iscor,
        # unique to each finger press
        finger.no    = 1:numInSeq,
        finger.corseq= corseql,
        finger.rt    = resprt,
        finger.resp  = respseq,
        finger.cor   = (corseql==respseq)
 )
 }) %>% bind_rows()
 
 # add subj id
 if(is.null(subjid)) subjid<- gsub('.*([0-9]{5}_[0-9]{8}).*','\\1',matfile)

 bigdf$subj <- subjid


 return(bigdf)

 ## CHECK -- not run
 testdf <-  
  bigdf %>% 
  group_by(runno,agency,test,,corseq,allcor,trial.within,trial) %>% 
  summarise(
    allcor2=all(finger.cor)*1,
    corseq2=paste(collapse="",finger.corseq), 
    #rt=max(finger.rt,na.rm=T),
    pushedseq=paste(collapse="",na.omit(finger.resp))
  ) %>% 
  arrange(trial) 
 print.data.frame(testdf,row.names=F)

}

# take the per finger dataframe from readmat
# and make it a per trial
perfinger2pertrial <- function(bigdf) {

 d <-  
  bigdf %>% 
  group_by(subj,runno,agency,test,corseq,allcor,trial.within,trial) %>% 
  summarise(
    finalrt=max(finger.rt,na.rm=T),
    allrts=paste(collapse=",",finger.rt),
    pushedseq=paste(collapse="",na.omit(finger.resp)),
    nfingercor= na.omit(c(which( finger.corseq != ifelse(is.na(finger.resp),0,finger.resp) ),5))[1] - 1,
    totalfingercor= sum(finger.cor,na.rm=T)
  ) %>% 
  arrange(trial) 

 #d <- reshape2::dcast(bigdf, subj+trial+runno+trial.within+agency+test+seqidxnum+corseq+allcor ~ finger.no,value.var=c('finger.rt'))
 # d<- data.table::dcast(data.table::setDT(bigdf), subj+trial+runno+trial.within+agency+test+seqidxnum+corseq+allcor ~ finger.no,value.var=c('finger.rt','finger.resp','finger.cor'))

 #print.data.frame(df,row.names=F)
 return(d)
}