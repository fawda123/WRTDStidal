######
#created Sep 2013, M. Beck
#creates interpolation grids for TB hirsch model
#used to create heat maps and to get normalization data

######
#salinity interpolation grids
#includes beta estimates for weighted regressions for each obs
#includes back-transformation from Moyer et al. 2012

sal.div<-20 #no. of divisions, range is different for each segment

mods.out<-NULL

segs<-unique(tb.dat$seg)

strt<-Sys.time()

for(seg in segs){
  
  dat.in<-tb.dat[tb.dat$seg==seg,]
  
  #salinity grid data specific to each segment
  sal.grid<-seq(min(dat.in$sal.ref),max(dat.in$sal.ref),length=sal.div)

  seg.out<-NULL
  
  for(row in 1:nrow(dat.in)){
    
    row.out<-NULL
    
    ref.in<-dat.in[row,]
    
    cat(as.character(seg), nrow(dat.in) - row,'\n')
    flush.console()
    
    for(sal in sal.grid){
      
      ref.in$sal.ref<-sal
      ref.wts<-wt.fun(ref.in,dat.in,wt.vars=c('month.num','year','sal.ref'))
      
      #OLS wtd model
      mod.md<-lm(
        Chla_ugl~dec.time + sal.ref + sin(2*pi*dec.time) + cos(2*pi*dec.time),
        weights=ref.wts,
        data=dat.in
        )
      
      fit.md<-predict(
        mod.md,
        newdata=data.frame(sal.ref=sal,dec.time=ref.in$dec.time)
        )
      
      b.md<-mod.md$coefficients['sal.ref']
      
      var.md<-res.var.fun(mod.md)
      
      bt.md<-exp(var.md/2)*exp(fit.md)
      
      #QT0.9 wtd model
      mod.hi<-rq(
        Chla_ugl~dec.time + sal.ref + sin(2*pi*dec.time) + cos(2*pi*dec.time),
        weights=ref.wts,
        tau=0.9,
        data=dat.in
        )
      
      fit.hi<-predict(
        mod.hi,
        newdata=data.frame(sal.ref=sal,dec.time=ref.in$dec.time)
        )
      
      b.hi<-mod.hi$coefficients['sal.ref']
      
      var.hi<-res.var.fun(mod.hi)
      
      bt.hi<-exp(var.hi/2)*exp(fit.hi)
      
      #QT0.1 wtd model
      mod.lo<-rq(
        Chla_ugl~dec.time + sal.ref + sin(2*pi*dec.time) + cos(2*pi*dec.time),
        weights=ref.wts,
        tau=0.1,
        data=dat.in
        )
      
      fit.lo<-predict(
        mod.lo,
        newdata=data.frame(sal.ref=sal,dec.time=ref.in$dec.time)
        )
      
      b.lo<-mod.lo$coefficients['sal.ref']
        
      var.lo<-res.var.fun(mod.lo)
      
      bt.lo<-exp(var.lo/2)*exp(fit.lo)
      
      #append to row out for each unique sal
      row.out<-rbind(
        row.out,
        cbind(fit.md,fit.hi,fit.lo,b.md,b.hi,b.lo,
         var.md,var.hi,var.lo,bt.md,bt.hi,bt.lo)
        )
      
      }
    
    wt.fits<-suppressWarnings(data.frame(
      year=ref.in$year,
      month.num=ref.in$month.num,
      date.f=ref.in$date.f,
      dec.time=ref.in$dec.time,
      seg,
      sal.grid,
      row.out
      ))
    
    seg.out<-rbind(seg.out,wt.fits)
    
    }
  
  mods.out<-rbind(mods.out,seg.out)
  
  }
  
Sys.time()-strt

sal.grd<-mods.out

save(sal.grd,file='salwt_grd.RData')

######
#plot interpolation grids

###
#by salinity

ylabs<-expression(paste('chl  ',italic(a),' (',italic(mu),'g',l^-1,')'))

#min, max sal.ref vals to plot....
lim.vals<-aggregate(
  sal.ref~month.num+seg,
  FUN=function(x) cbind(quantile(x,0.05),quantile(x,0.95)),
  data=epc.est
  )
names(lim.vals)[2]<-'seg'

to.plo<-merge(int.grd,lim.vals,by=c('month.num','seg'),all.x=T)

#interp grid removing extreme values
p<-ggplot(to.plo,aes(x=dec.time,y=sal.grid)) + 
  geom_tile(aes(fill=exp(wt.fit.md)),width=0.1) + #adjust this to fill gaps
#   scale_fill_brewer(type='div',palette = 'BuGn') +
   scale_fill_gradient2(name=ylabs,low='blue',mid='lightgreen',high='red',midpoint=20) +
   geom_line(aes(x=dec.time,y=sal.ref[,2])) +
   geom_line(aes(x=dec.time,y=sal.ref[,1])) +
  facet_wrap(~seg,nrow=2,ncol=2) +
  theme_bw() +
  scale_x_continuous(
    breaks=seq(1974,2012,by=2),
    name='Date',
    expand = c(0,0)
    ) + 
  scale_y_continuous(name='Proportion freshwater',expand = c(0,0)) +
  theme(
    axis.text.x=element_text(angle = 90, vjust=0.5,hjust=1)
    )

pdf('sal_grd.pdf',width=11,height=6.5,family='serif')
print(p)
dev.off()
