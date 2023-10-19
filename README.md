# WRTDStidal: evaluating long-term water quality trends in tidal waters

### Marcus W. Beck, mbeck@tbep.org

[![R-CMD-check](https://github.com/fawda123/WRTDStidal/workflows/R-CMD-check/badge.svg)](https://github.com/fawda123/WRTDStidal/actions)
[![pkgdown](https://github.com/fawda123/WRTDStidal/workflows/pkgdown/badge.svg)](https://github.com/fawda123/WRTDStidal/actions)
[![CRAN status](https://www.r-pkg.org/badges/version/WRTDStidal)](https://CRAN.R-project.org/package=WRTDStidal)
[![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/WRTDStidal)](https://cran.rstudio.com/package=WRTDStidal)
[![DOI](https://zenodo.org/badge/22622714.svg)](https://zenodo.org/badge/latestdoi/22622714)

This is the development repository for the WRTDStidal package.  Functions within this package can be used to model water quality time series from coastal monitoring data.  The approach follows on previous methods described in the [EGRET](https://github.com/USGS-R/EGRET) package developed by USGS for non-tidal waters.  

Additional details: 

*Beck MW, Hagy JD. 2015. Adaptation of a weighted regression approach to evaluate water quality trends in an Estuary. Environmental Modelling and Assessment. 20(6):637-855. [http://dx.doi.org/10.1007/s10666-015-9452-8](http://dx.doi.org/10.1007/s10666-015-9452-8)*

The original method for streams and rivers is described here:

*Hirsch RM, De Cicco L. 2014. User guide to Exploration and Graphics for RivEr Trends (EGRET) and dataRetrieval: R packages for hydrologic data. Techniques and Methods book 4, ch. A10, US Geological Survey, Reston, Virginia. http://pubs.usgs.gov/tm/04/a10/*

*Hirsch RM, Moyer DL, Archfield SA. 2010. Weighted regressions on time, discharge, and season (WRTDS), with an application to Chesapeake Bay river inputs. Journal of the American Water Resources Association. 46(5):857-880. [DOI: 10.1111/j.1752-1688.2010.00482.x](http://onlinelibrary.wiley.com/doi/10.1111/j.1752-1688.2010.00482.x/abstract)*

## Installing the package

The stable release can be installed from CRAN:

```
install.packages('WRTDStidal')
library(WRTDStidal)
```

The development version of this package can be installed as follows:

```
install.packages('WRTDStidal', repos = c('https://fawda123.r-universe.dev', 'https://cloud.r-project.org'))
```

## Package description

Read the complete package description [here](http://fawda123.github.io/WRTDStidal/articles/overview.html). 
