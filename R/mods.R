# #created Aug 2013 by M. Beck
# #Hirsch model for EPC chlorophyll data
# #estimated data are obtained - from interpolation grid and actual data
# 
# ######
# #get estimated data from interpolation grids
# #add obs column to sal.grd to indicate which is matching
# 
# # salinity interpolation grid, from 'interp_grid.r'
# load(file='salwt_grd.RData')
# 
# obs<-numeric(nrow(sal.grd))
# 
# segs<-unique(tb.dat$seg)
# 
# fit<-NULL
# 
# strt<-Sys.time()
# 
# for(seg in segs){
#   
#   dat.in<-tb.dat[tb.dat$seg==seg,]
#   
#   seg.fit<-NULL
# 
#   for(row in 1:nrow(dat.in)){
#   
#     cat(paste(seg,row,'of',nrow(dat.in)),'\n')
#     flush.console()
#     
#     #observation to normalize
#     ref.in<-dat.in[row,]
#     
#     #get inteprolation grid data
#     grd.in<-sal.grd[sal.grd$seg == seg & sal.grd$dec.time==ref.in$dec.time,]
#     
#     #matching closest values in grid to actual input
#     grd.mtch<-grd.in[which.min(abs(grd.in$sal.grid-ref.in$sal.ref)),c('fit.md','fit.hi','fit.lo','b.md','b.hi','b.lo','var.md','var.hi','var.lo','bt.md','bt.hi','bt.lo')]
#     
#     obs[as.numeric(row.names(grd.mtch))]<-1
#     
#     #output for segment
#     seg.fit<-rbind(seg.fit,cbind(ref.in,grd.mtch))
#     
#     }
#   
#   #output for all
#   fit<-rbind(fit,seg.fit)
#   
#   }
# 
# Sys.time()-strt
# 
# epc.est<-fit
# 
# save(epc.est,file='est.RData')
# 
# #save obs to sal.grd
# sal.grd<-data.frame(
#   sal.grd[,c('seg','year','month.num','dec.time','sal.grid')],
#   obs,
#   sal.grd[,c('fit.md','fit.hi','fit.lo','b.md','b.hi','b.lo','var.md','var.hi','var.lo','bt.md','bt.hi','bt.lo')]
#   )
# 
# save(sal.grd,file='salwt_grd.RData')
# 
# ######
# #get actual model estimates, not from interpolation grid
# #output appended to mods.out
# #also includes residuals (obs-pred) since this does not make sense for the interp grids
# #i.e., there is no observed chlorophyll for each unique sal, time combo,
# #only for observed sal, time values\
# #also creates residual variance and back-transformed estimates following Moyer et al. 2012
#   
# segs<-unique(tb.dat$seg)
# 
# mods.out<-matrix(ncol=12,nrow=nrow(tb.dat))
# 
# for(seg in segs){
# 
#   dat.in<-tb.dat[tb.dat$seg==seg,]
#   
#   vecs<-c('fit.md','fit.hi','fit.lo','res.md','res.hi','res.lo','var.md',
#     'var.hi','var.lo','bt.md','bt.hi','bt.lo')
#   sapply(vecs,function(x) assign(x,numeric(nrow(dat.in)),envir=.GlobalEnv))
# 
#   strt<-Sys.time()
#   for(row in 1:nrow(dat.in)){
#     
#     ref.in<-dat.in[row,]
# 
#     ref.wts<-wt.fun(ref.in,dat.in,wt.vars=c('month.num','year','sal.ref'))
#     
#     cat(paste(row,' of ',nrow(dat.in),', ',sum(ref.wts>0),' > 0',sep=''),'\n')
#     flush.console()
#     
#     mod.md<-lm(
#       Chla_ugl~dec.time + sal.ref + sin(2*pi*dec.time) + cos(2*pi*dec.time),
#       weights=ref.wts,
#       data=dat.in
#       )
#     
#     mod.hi<-rq(
#       Chla_ugl~dec.time + sal.ref + sin(2*pi*dec.time) + cos(2*pi*dec.time),
#       weights=ref.wts,
#       tau=0.9,
#       data=dat.in
#       )
#   
#     mod.lo<-rq(
#       Chla_ugl~dec.time + sal.ref + sin(2*pi*dec.time) + cos(2*pi*dec.time),
#       weights=ref.wts,
#       tau=0.1,
#       data=dat.in
#       )
#     
#     fit.md[row]<-fitted(mod.md)[row]
#     fit.hi[row]<-fitted(mod.hi)[row]
#     fit.lo[row]<-fitted(mod.lo)[row]
#     res.md[row]<-resid(mod.md)[row]
#     res.hi[row]<-resid(mod.hi)[row]
#     res.lo[row]<-resid(mod.lo)[row]
#     var.md[row]<-res.var.fun(mod.md)
#     var.hi[row]<-res.var.fun(mod.hi)
#     var.lo[row]<-res.var.fun(mod.lo)
#     bt.md[row]<-bt.fun(mod.md)[row]
#     bt.hi[row]<-bt.fun(mod.hi)[row]
#     bt.lo[row]<-bt.fun(mod.lo)[row]
#     
#     }
#     
#   cat(Sys.time()-strt,'\n')
#   
#   mods.out[tb.dat$seg==seg,]<-cbind(sapply(vecs,get))
#   
#   }
# 
# out<-data.frame(tb.dat,mods.out,stringsAsFactors=F)
# names(out)[grep('X',names(out))]<-vecs
# epc.est.act<-out
# 
# save(epc.est.act,file='est_act.RData')
