#created Aug 2013
#functions for WQ models, following Hirsch model

######
#function for getting regression weights
#'wt.vars' is name of three variables to weight
#'ref.in' is row of dat.in that is used as reference
#'dat.in' is data to get weights from
#'wins' are the windows for the three wt.vars
#'all' will return all weights, rather than the product of all three
#'min.obs' uses window widening if less than 100 non-zero weights are found
wt.fun<-function(ref.in,dat.in,
  wt.vars=c('month.num','year','sal.ref'),
  wins=list(0.5,10,NULL),
  all=F,
  min.obs=T){
  
  #sanity check
  if(sum(wt.vars %in% names(dat.in)) != length(wt.vars))
    stop('Weighting variables must be named in "dat.in"')
  
  #windows for each of three variables
  wins_1<-wins[[1]]
  wins_2<-wins[[2]]
  wins_3<-wins[[3]]
  
  if(is.null(wins[[3]])) wins_3<-diff(range(dat.in[,wt.vars[3]]))/2
  
  #weighting tri-cube function
  wt.fun.sub<-function(dat.cal,ref,win,mo=F){
    
    dist.val<-abs(dat.cal-ref)
    
    if(mo){
      dist.val<-pmin(
        ref+1-dat.cal,
        dat.cal+1-ref,
        dist.val
        )
      }
    
    if(dist.val<=win) return((1-(dist.val/win)^3)^3)
    
    else return(0)
      
    }

  #reference (starting) data
  ref_1<-as.numeric(ref.in[,wt.vars[1]])
  ref_2<-as.numeric(ref.in[,wt.vars[2]])
  ref_3<-as.numeric(ref.in[,wt.vars[3]])

  #weights for each observation in relation to reference
  wts_1<-sapply(as.numeric(dat.in[,wt.vars[1]]),wt.fun.sub,ref=ref_1,win=wins_1,mo=T)
  wts_2<-sapply(as.numeric(dat.in[,wt.vars[2]]),wt.fun.sub,ref=ref_2,win=wins_2)
  wts_3<-sapply(as.numeric(dat.in[,wt.vars[3]]),wt.fun.sub,ref=ref_3,win=wins_3)
  out<-wts_1*wts_2*wts_3
  
  gr.zero<-sum(out>0)
  #cat('   Number of weights greater than zero =',gr.zero,'\n')
  
  while(gr.zero<100){
    
    wins_1<-0.1*wins_1 + wins_1
    wins_2<-0.1*wins_2 + wins_2
    wins_3<-0.1*wins_3 + wins_3
    
    #weights for each observation in relation to reference
    wts_1<-sapply(as.numeric(dat.in[,wt.vars[1]]),wt.fun.sub,ref=ref_1,win=wins_1,mo=T)
    wts_2<-sapply(as.numeric(dat.in[,wt.vars[2]]),wt.fun.sub,ref=ref_2,win=wins_2)
    wts_3<-sapply(as.numeric(dat.in[,wt.vars[3]]),wt.fun.sub,ref=ref_3,win=wins_3)
    
    out<-wts_1*wts_2*wts_3
    
    gr.zero<-sum(out>0)
    
    #cat('   Number of weights greater than zero',gr.zero,'\n')
    
    }
  
  #return all weights if T
  if(all){
    out<-data.frame(wts_1,wts_2,wts_3)
    names(out)<-wt.vars
    return(out)
    }
  
  #final weights are product of all three
  out
  
  }

######
#r.squared function
#created for data from weighted regression, but works for all models
#residuals are observed - predicted, but taken from model objects (see 'epc_mods.R')
rsq.fun<-function(resid,obs){
  
  require(Metrics)
  
  ssr<-sum(resid^2)
  sst<-sum(se(obs,mean(obs)))
  
  return(1 - (ssr/sst))

}

######
#pseudo-rsquared for quantile regression
#rho is like sums of squares for OLS
#the 'objective function' is summed to get rho and is minimized for qt fit
#sum of individual rho values for non-conditional response is same as rho value
#that fits formula y~0, e.g. mod1$rho
#or the same as sum of rho.fun applied to observed values at given tau
#qt residuals are observed - predicted (not predicted - observed)
#see Koenker and Machado 1999 (top right p. 1297 for narrative description)
#see https://stat.ethz.ch/pipermail/r-help/2006-August/110386.html
rsq.rq.fun<-function(resid,obs,tau){
  
  require(quantreg)
  
  rho.fun<-function(u,tau.val) u*(tau.val-(u<0))
  
  V1<-sum(rho.fun(resid,tau)) #minimum sum of deviations,
  V0<-rq(obs~1,tau=tau)$rho #null model
  
  return(1-V1/V0)
  
  }

######
#function for getting variance of model residuals
#used to correct for re-transformation bias
#second function does back-trans following Moyer et al. 2012
res.var.fun<-function(mod.in){
  sum(mod.in$weights*resid(mod.in)^2)/(sum(mod.in$weights>0)-5)
  }
bt.fun<-function(mod.in) exp(res.var.fun(mod.in)/2)*exp(fitted(mod.in))
