#' Add date columns and fill missing values in the interpolation grids
#' 
#' Add date, year, month, and day columns to the interpolation grids using dates from \code{dat_in}. The \code{\link[akima]{interp}} function is used after splitting the interpolation matrix by month to fill missing values.  Function is used in \code{\link{wrtds}}.
#'
#' @param grd_in interpolation grid to fill, either a single mean grid or an individual quantile grid created within \code{\link{wrtds}}
#' @param dat_in \code{\link{tidal}} or \code{\link{tidalmean}} object
#' @param interp logical for interpolation
#' 
#' @import akima dplyr
#' 
fill_grd <- function(grd_in, dat_in, interp = FALSE){

  # infinite to NA, this means a really bad fit
  grd_in[is.infinite(grd_in)] <- NA

  # fill missing values
  if(interp){
    
    # interpolate NA values by month
    mos <- as.numeric(format(dat_in$date, '%m'))
    bymo <- sapply(1:12, function(x){
      
      # split fit grid by months, 
      # remove missing values in long format
      sel <- mos %in% x
      dts <- dat_in$date[sel]
      out <- grd_in[sel, ]
      out <- data.frame(date = dts, out) %>% 
        tidyr::gather('xvar', 'val', -date) %>% 
        na.omit %>% 
        mutate(xvar = as.numeric(gsub('X', '', xvar)))
      
      # interpolate each month for original grd space
      newvals <- interp(
        x = out$xvar, 
        y = as.numeric(out$date),
        z = out$val, 
        xo = 1:ncol(grd_in),
        yo = as.numeric(dts)
        ) %>% 
        .$z %>% 
        t %>% 
        data.frame(date = dts, .)
      
      newvals
      
    }, simplify =F )
    
    # combine all months, sort by row
    # back to original grd format
    grd_in <- do.call('rbind', bymo) %>% 
      arrange(date) %>% 
      select(-date) %>% 
      as.matrix

  }
  
  # add year, month, day from dat_in to interp grid
  out <- data.frame(
    date = dat_in$date, 
    year = dat_in$year, 
    month = dat_in$month, 
    day = as.numeric(strftime(dat_in$date, '%d')),
    grd_in
    )

  return(out)
  
}