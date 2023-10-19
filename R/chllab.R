#' Chlorophyll axis label
#' 
#' Get the chlorophyll axis label for observed or log-space, including units
#' 
#' @param logspace logical indicating if chlorophyll is in log space, otherwise observed
#' 
#' @export
#' 
#' @return An expression indicating (log)-chlorophyll in the appropriate units
#' 
#' @examples
#' 
#' ## default
#' chllab()
chllab <- function(logspace = TRUE){

  # default
  label <- expression(
    paste('log-Chloropyhll-',italic(a),' (',italic(mu),'g ',L^-1,')')
    )

  # back-transform if needed
  if(!logspace){
    
    label <- expression(
      paste('Chloropyhll-',italic(a),' (',italic(mu),'g ',L^-1,')')
    )
  }
  
  return(label)
  
}
  
  