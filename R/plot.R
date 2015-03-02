#' Plot a tidal object
#' 
#' Plot a tidal object to view chlorophyll observations, predictions, and normalized results.
#' 
#' @param tidal_in input tidal object
#' @param annuals logical indicating if plots are annual aggregations of results
#' @param logspace logical indicating if plots are in log space
#' @param pretty logical indicating if my subjective idea of plot aesthetic is applied, otherwise the \code{\link[ggplot2]{ggplot}} default themes are used
#' @param ... arguments passed to other methods
#' 
#' @import ggplot2
#' 
#' @export
#' 
#' @return A \code{\link[ggplot2]{ggplot}} object that can be further modified
#' 
#' @examples
#' 
#' ## load a fitted tidal object
#' data(tidfit)
#' 
#' # plot
#' fitplot(tidfit)
#' 
fitplot <- function(tidal_in, ...) UseMethod('fitplot')

#' @rdname fitplot
#' 
#' @export 
#' 
#' @method fitplot tidal
fitplot.tidal <- function(tidal_in, tau = NULL, annuals = TRUE, logspace = FALSE, pretty = TRUE,...){
 
  # conver to df for plotting
  to_plo <- data.frame(tidal_in)
  
  # plot all quantile fits if null
  if(is.null(taus)){
     
  }
  
  # bare bones plot
  p <- ggplot(to_plo, aes(x = date, y = chla)) + 
    geom_point() + 
    geom_line(aes(y = fit0.5))
  
  if(!pretty) return(p)
  
  return(p)
  
}