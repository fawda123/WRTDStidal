#' Evaluate half-window width combinations 
#'
#' Evaluate a grid of half-window width combinations to use for weighted regression
#' 
#' @param dat_in input data object to use with weighted regression
#' @param grid_in optional input matrix of half-window widths created with \code{\link{createsrch}}, a default search grid is used if no input

#' @param ... arguments passed to or from other methods
#' 
#' @export
#' 
#' @seealso \code{\link{createsrch}}, \code{\link{wrtdscv}}, \code{\link{winsrch_optim}}
#' 
#' @details Processing time can be reduced by setting up a parallel backend, as in the examples.  Note that this is not effective for small k-values (e.g., < 4) because each fold is sent to a processor, whereas the window width combinations in \code{grid_in} are evaluated in sequence.  
#' 
#' This function should only be used to view the error surface assocatied with finite combinations of window-width combinations.  A faster function to identify the optimal window widths is provided by \code{\link{winsrch_optim}}. 
#' 
#' @return A data frame of the search grid with associated errors for each cross-validation result.  Errors for each grid row are averages of all errors for each fold used in cross-validation.
#' 
#' @examples 
#' \dontrun{
#' ##
#' # setup parallel backend
#' library(doParallel)
#' ncores <- detectCores() - 2 
#' registerDoParallel(cores = ncores)
#' 
#' # run search function using default search grid - takes a while
#' res <- winsrch_grid(tidobjmean)
#' 
#' # view the error surface 
#' library(ggplot2)
#' ggplot(res, aes(x = factor(mos), y = factor(yrs), fill = err)) +
#'    geom_tile() + 
#'    facet_wrap(~ flo) + 
#'    scale_x_discrete(expand = c(0, 0)) +
#'    scale_y_discrete(expand = c(0,0)) +
#'    scale_fill_gradientn(colours = gradcols()) 
#' 
#' # optimal combo
#' res[which.min(res$err), ]
#' 
#' ##
#' # create a custom search grid, e.g. years only
#' grid_in <- createsrch(mos = 1, yrs = seq(1, 10), flo = 1)
#' 
#' res <- winsrch_grid(tidobjmean, grid_in)
#' 
#' }
winsrch_grid <- function(dat_in, ...) UseMethod('winsrch_grid')

#' @rdname winsrch_grid
#'
#' @export
#' 
#' @import foreach
#' 
#' @method winsrch_grid default
winsrch_grid.default <- function(dat_in, grid_in = NULL, ...){
  
  if(is.null(grid_in)) grid_in <- createsrch()
  
  strt <- Sys.time()
  
  res <-  rep(NA, length = nrow(grid_in))
  for(i in 1:nrow(grid_in)){
    
    cvslamb <- wrtdscv(dat_in, wins = grid_in[i, ], ...)
    
    res[i] <- cvslamb
    
  }
  
  out <- data.frame(grid_in, err = res)
  
  return(out)
  
}
