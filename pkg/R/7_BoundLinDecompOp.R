#
# vim:set ff=unix expandtab ts=2 sw=2:

setClass(# a decomposition operator described by a matrix valued function of time
    Class="BoundLinDecompOp",
    contains=c("DecompOp","TimeMap"),   
   )
correctnessOfBoundDecompOp <- function(obj){
  tr <- getTimeRange(obj)
  t_min <- tr[['t_min']]
  matFunc <- getFunctionDefinition(obj)
  testVal <- matFunc(t_min)
  valDims <- dim(testVal)
  if (length(valDims)!=2){
    stop(sprintf('The function must return a 2-dimensional object (like a Matrix). Your input leads to a function that return an object with dim(object)=%s',valDims))}
  if (valDims[[1]]!=valDims[[2]]){
    stop(sprintf('The function must return a quadratic object (qudratic Matrix). Your input leads to a function that return an object with dim(object)=%s',valDims))}
}
#---------------------------------------------------------------------
setMethod(
      f="BoundLinDecompOp",
      signature=c(map="UnBoundLinDecompOp"),
      definition=function # convert a UnBoundLinDecompOp to a BoundLinDecompOp
      ### The method creates a BoundLinDecompOp consisting of a constant time dependent function 
      ### and the limits of its domain (starttime and endtime) set to -Inf and Inf respectively
      (map,
       starttime=-Inf,
       endtime=Inf
       ){
      f=getFunctionDefinition(map)
      return(BoundLinDecompOp(map=f,starttime,endtime))
     }
)
#---------------------------------------------------------------------
setMethod(
      f="BoundLinDecompOp",
      signature=c(map="ANY",starttime='missing',endtime='missing'),
      definition=function # a constructor 
  ### Creates a BoundLinDecompOp Object. 
  (
    map, ##<< anything that can be used as a single \code{map} parameter 
  lag=0,        ##<< a scalar describing the time lag. Positive Values shift the argument of the interpolation function forward in time. (retard its effect)
  interpolation=splinefun  ##<< the interpolation function to be used
  ){
    ##<<details
    ##The function  will first call \code{\link{TimeMap}} on its argument
    ##which means that it can handle the same combinations of parameters. 
    ##The TimeMap object thus created might be further examined.
    ##It will only be accepted if the function defined by the TimeMap 
    ##object has quadratic matrices as values.
    if (inherits(map,'TimeMap')){
     tm <-map
    }else{
      tm <- TimeMap(map,lag=lag,interpolation=interpolation)
    }
    obj <- as(tm,"BoundLinDecompOp")
    return(obj)
    ### A BoundLindDecompOp object.
  }
)
#---------------------------------------------------------------------
setMethod(
      f="BoundLinDecompOp",
      signature=c(map="ANY"),
      definition=function # a constructor 
  ### Creates a BoundLinDecompOp Object. 
  (
    map, ##<< anything that can be used as the \code{map} parameter 
    starttime, ##<<  for \code{\link{TimeMap}}
    endtime, ##<<  for \code{\link{TimeMap}}
    lag=0,        ##<< a scalar describing the time lag. Positive Values shift the argument of the interpolation function forward in time. (retard its effect)
  interpolation=splinefun  ##<< the interpolation function to be used
  ){
    ##<<details
    ##The function  will first call \code{\link{TimeMap}} on its arguments which means that it can handle the same combinations of parameters. 
    ##The TimeMap object thus created will then be further examined.
    ##It will only be accepted if the function defined by the TimeMap 
    ##object has quadratic matrices as values.
    obj <- as(TimeMap(map,starttime,endtime,lag,interpolation),"BoundLinDecompOp")
    return(obj)
    ### A BoundLindDecompOp object.
  }
)
