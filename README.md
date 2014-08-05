<H2>README</H2>

Files are used to produce weighted regression results for Tampa Bay, Florida.  Currently in development...

All data from <a href=http://www.tampabay.wateratlas.usf.edu/>http://www.tampabay.wateratlas.usf.edu/</a>

<H3>To evaluate</H3>

<ol>

<li>run 'interp_grd.r' to create 'salwt_grd.RData', this is interpolation grid</li>

<li>run 'mods.r' to create 'est.RData' and 'est_act.RData', uses inteprolation grid from last step</li>

<li>run 'norm.r' to create 'sal_nrm.RData', these are salinity-normalized estimates</li>

</ol>

<H3>Data files</H3>

tb_dat.RData - raw data from above link with extensive pre-processing, represent averages of wq stations by each bay segment

salwt_grd.RData - interpolation grid of predicted chl for each obs across range of fraction of freshwater

est.RData - predicted chlorophyll from models using interpolation grid

est_act.RData - predicted chlorophyll from models using actual fraction of freswhater values

sal_nrm.RData - salinity (fraction of freshwater) normalized chlorophyll values
