# ######
# #plot interpolation grids
# 
# ###
# #by salinity
# 
# ylabs<-expression(paste('chl  ',italic(a),' (',italic(mu),'g',l^-1,')'))
# 
# #min, max sal.ref vals to plot....
# lim.vals<-aggregate(
#   sal.ref~month.num+seg,
#   FUN=function(x) cbind(quantile(x,0.05),quantile(x,0.95)),
#   data=epc.est
#   )
# names(lim.vals)[2]<-'seg'
# 
# to.plo<-merge(int.grd,lim.vals,by=c('month.num','seg'),all.x=T)
# 
# #interp grid removing extreme values
# p<-ggplot(to.plo,aes(x=dec.time,y=sal.grid)) + 
#   geom_tile(aes(fill=exp(wt.fit.md)),width=0.1) + #adjust this to fill gaps
# #   scale_fill_brewer(type='div',palette = 'BuGn') +
#    scale_fill_gradient2(name=ylabs,low='blue',mid='lightgreen',high='red',midpoint=20) +
#    geom_line(aes(x=dec.time,y=sal.ref[,2])) +
#    geom_line(aes(x=dec.time,y=sal.ref[,1])) +
#   facet_wrap(~seg,nrow=2,ncol=2) +
#   theme_bw() +
#   scale_x_continuous(
#     breaks=seq(1974,2012,by=2),
#     name='Date',
#     expand = c(0,0)
#     ) + 
#   scale_y_continuous(name='Proportion freshwater',expand = c(0,0)) +
#   theme(
#     axis.text.x=element_text(angle = 90, vjust=0.5,hjust=1)
#     )
# 
# pdf('sal_grd.pdf',width=11,height=6.5,family='serif')
# print(p)
# dev.off()