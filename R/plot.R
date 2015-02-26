#' Plot a tidal object
#' 
#' Plot a tidal object to view chlorophyll observations, predictions, and normalized results.
#' 
#' @rdname plot
#' 
#' @param tidal_in input tidal object
#' @param annuals logical indicating if plots are annual aggregations of results
#' @param logspace logical indicating if plots are in log space
#' 
#' @import ggplot2
#' 
#' @export
#' 
#' @return A \code{\link[ggplot2]{ggplot}} object that can be further modified
#'
#' @method plot tidal
#' 
#' @examples
#' 
#' ## load a fitted tidal object
#' data(tidfit)
#' 
#' # plot
#' plot(tidfit)
plot.tidal <- function(tidal_in, annuals = TRUE, logspace = FALSE){
 
  to_plo <- data.frame(tidal_in)
  
  p <- ggplot(to_plo, aes(x = date, y = chla)) + 
    geom_point() + 
    geom_line(aes(y = fit0.5))
  
  return(p)
  
}