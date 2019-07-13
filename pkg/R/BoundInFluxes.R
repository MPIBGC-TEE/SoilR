BoundInFlux.new=function
(t_start, 
 t_end,   
 f        
 ){
   warning("The function is deprecated, constructors are now called as the class. To get rid of this warning please use BoundInFluxes() in the future.")
   obj=BoundInFluxes(f,t_start,t_end) 
return(obj)
}
setClass(
   Class="BoundInFluxes",
   contains=c("InFluxes","TimeMap"),
)






#' constructor for BoundInFluxes
#' 
#' The method internally calls \code{\link{TimeMap}} and expects the same kind
#' of arguments
#' 
#' 
#' @param ... passed on to \code{\link{TimeMap}}
BoundInFluxes <- function 
  (
   ... 
  )
      {
      return(as(TimeMap(...),'BoundInFluxes'))
}
