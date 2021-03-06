#' Get colors for plots
#' 
#' Gets colors used for WRTDS plots
#' 
#' @param col_vec chr string of plot colors to use, typically passed to \code{\link[ggplot2]{scale_colour_gradientn}} for shading. Any color palette from RColorBrewer can be used as a named input. Palettes from grDevices must be supplied as the returned string of colors for each palette.
#' 
#' @details This is a convenience function for retrieving a color palette that is used by most of the plotting functions.   Palettes from RColorBrewer will use the maximum number of colors.  The default palette is 'Spectral'. 
#' 
#' @export
#' 
#' @seealso \code{\link{dynaplot}}, \code{\link{gridplot}}, \code{\link{wtsplot}}
#' 
#' @return A character vector of colors in hexadecimal notation.
#' 
#' @examples
#' 
#' ## defaults
#' gradcols()
#' 
#' ## another RColorBrewer palette
#' gradcols('Pastel2')
#' 
#' ## a silly example
#' gradcols(rainbow(7))
gradcols <- function(col_vec = NULL){

  cols <- RColorBrewer::brewer.pal(11, 'Spectral')
    
  # color ramp for pts
  if(!is.null(col_vec)){
 
    # get color palette if provided, otherwise user-supplied
    chk_cols <- row.names(RColorBrewer::brewer.pal.info)
    
    if(any(chk_cols %in% col_vec)){
      
      col_vec <- chk_cols[which(chk_cols %in% col_vec)][1]
      max_cols <- RColorBrewer::brewer.pal.info[col_vec, 'maxcolors']
      cols <- RColorBrewer::brewer.pal(max_cols, col_vec)
      
    } else {
      
      cols <- col_vec
      
    }
   
  }
  
  return(cols)

}