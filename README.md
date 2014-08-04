Files are those required to produce weighted regression results for Tampa Bay, Florida.

All data from <a href=http://www.tampabay.wateratlas.usf.edu/>http://www.tampabay.wateratlas.usf.edu/</a>, available by bay segment in tb_dat.RData

To evaluate --

1) run 'interp_grd.r' to create 'salwt_grd.RData', this is interpolation grid

2) run 'mods.r' to create 'est.RData' and 'est_act.RData', uses inteprolation grid from last step

3) run 'norm.r' to create 'sal_nrm.RData', these are salinity-normalized estimates
