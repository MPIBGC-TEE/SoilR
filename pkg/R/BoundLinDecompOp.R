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
############################constructors####################
setMethod(
      f="BoundLinDecompOp",
      signature=c(map="UnBoundLinDecompOp"),
      ### The method creates a BoundLinDecompOp consisting of a constant time dependent function 
      ### and the limits of its domain (starttime and endtime) set to -Inf and Inf respectively
      definition=function # convert a UnBoundLinDecompOp to a BoundLinDecompOp
      (map,
       starttime=-Inf, ##<< the left hand boundary of the valid time interval
       endtime=Inf   ##<< the right hand boundary of the valid time interval
       ){
      f=getFunctionDefinition(map)
      return(BoundLinDecompOp(map=f,starttime,endtime))
     }
)
#---------------------------------------------------------------------
setMethod(
      f="BoundLinDecompOp",
      signature=signature(map="ANY"),
      definition=function # a constructor 
  ### Creates a BoundLinDecompOp Object. 
  (
    map, ##<< passed on to TimeMap
    ...  ##<< passed on to TimeMap
  ){
    ##<<details
    ##The function  will first call \code{\link{TimeMap}} on its arguments
    ##which means that it can handle the same combinations of parameters. 
    ##The TimeMap object thus created might be further examined.
    ##It will only be accepted if the function defined by the TimeMap 
    ##object has quadratic matrices as values and is compartmental.
    tm <- TimeMap(map,...)
    obj <- as(tm,"BoundLinDecompOp")
    return(obj)
    ### A BoundLindDecompOp object.
  }
)
