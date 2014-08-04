#######
#created Sep 2013, M. Beck
#normalization of tb hirsch model

#######

#import data, salinity grid loaded automatically
rm(list=ls())

######
#normalize estimates by independent variables

###
#normalize by salinity
#uses back-trans data based on Moyer et al. 2012

segs<-unique(tb.dat$seg)

fit<-NULL

strt<-Sys.time()

for(seg in segs){
  
  dat.in<-tb.dat[tb.dat$seg==seg,]
  
  seg.fit<-NULL

  for(row in 1:nrow(dat.in)){
  
    cat(paste(seg,row,'of',nrow(dat.in)),'\n')
    flush.console()
    
    #observation to normalize
    ref.in<-dat.in[row,]
  
    #variable values to normalize by, sal.ref for each year of given month
    norm.vals<-dat.in[dat.in$month.num == ref.in$month.num,'sal.ref']
    
    #get salinity grid date
    grd.in<-sal.grd[sal.grd$seg == seg & round(sal.grd$dec.time,10)==round(ref.in$dec.time,10),]
    
    #matching closest values in grid to actual input
    grd.mtch<-sapply(
      norm.vals,
      function(x) which.min(abs(grd.in$sal.grid-x))
      )

    #values to normalize for each obs./row for a given segment
    out.nrm<-grd.in[grd.mtch,c('bt.md','bt.hi','bt.lo')]
      
    #output for segment
    seg.fit<-rbind(seg.fit,cbind(ref.in[rep(1,nrow(out.nrm)),],norm.vals,out.nrm))
    
    }
  
  #output for all
  fit<-rbind(fit,seg.fit)
  
  }

Sys.time()-strt

#export

sal.nrm.all<-fit
names(sal.nrm.all)[(ncol(sal.nrm.all)-2):ncol(sal.nrm.all)]<-c('norm.md','norm.hi','norm.lo')

sal.nrm<-aggregate(cbind(norm.md,norm.hi,norm.lo)~dec.time+seg,
  mean,data=sal.nrm.all)

sal.nrm<-merge(epc.est,sal.nrm,by=c('dec.time','seg'))

save(sal.nrm,file='sal_nrm.RData')


