BoundInFlux.new=function
(t_start, 
 t_end,   
 f        
 ){
   warning("The function is deprecated, constructors are now called as the class. To get rid of this warning please use BoundInFlux() in the future.")
   obj=BoundInFlux(f,t_start,t_end) 
return(obj)
}
setClass(
   Class="BoundInFlux",
   contains=c("InFlux","TimeMap"),
)






#' constructor for BoundInFlux
#' 
#' The method internally calls \code{\link{TimeMap}} and expects the same kind
#' of arguments
#' 
#' 
#' @param ... passed on to \code{\link{TimeMap}}
BoundInFlux <- function 
  (
   ... 
  )
      {
      return(as(TimeMap(...),'BoundInFlux'))
}
