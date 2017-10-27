#
# vim:set ff=unix expandtab ts=2 sw=2:

setClass(# a decomposition operator described by a matrix valued function of time
    Class="BoundLinDecompOp",
    contains=c("DecompOp","TimeMap"),   
   )
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
    map ##<< anything that can be used as a single \code{map} parameter 
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
      tm <- TimeMap(map)
    }
    return(as(tm,"BoundLinDecompOp"))
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
    endtime ##<<  for \code{\link{TimeMap}}
  ){
    ##<<details
    ##The function  will first call \code{\link{TimeMap}} on its arguments which means that it can handle the same combinations of parameters. 
    ##The TimeMap object thus created will then be further examined.
    ##It will only be accepted if the function defined by the TimeMap 
    ##object has quadratic matrices as values.
    as(TimeMap(map,starttime,endtime),"BoundLinDecompOp")
    ### A BoundLindDecompOp object.
  }
)
