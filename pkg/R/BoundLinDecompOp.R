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

#' A converter 
#' 
#' The distinction between the classes BoundLinDecompOp and UnboundLinDecompOp exist for those functions, that should be only defined for objects of class UnBoundLinDecomp.
#'  
#' Many functions however do not need extra methods for objects of class 
#' UnBoundLinDecompOp and just treat it as a BoundLinDecompOp which is defined
#' on the complete timeline (-Inf,+Inf). 
#' With its default arguments this function converts its map argument to a BoundLinDecompOp with just this domain. 
#' This is the most frequent internal use case. 
#' If starttime and endtime are provided the domain of the operator will be restricted [starttime,endtime].
#'
#' @param map An object of class UnBoundLinDecompOp
#' @param starttime  Begin of time interval map will be restricted to
#' @param endtime End of time interval map will be restricted to
setMethod(
      f="BoundLinDecompOp",
      signature=c(map="UnBoundLinDecompOp"),
      definition=function 
      (map,
       starttime=-Inf, 
       endtime=Inf   
       ){
      f=getFunctionDefinition(map)
      return(BoundLinDecompOp(map=f,starttime,endtime))
     }
)

#' automatic title
#' 
#' @param map no manual documentation
#' @param ... no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
      f="BoundLinDecompOp",
      signature=signature(map="ANY"),
      definition=function 
  (
    map, 
    ...  
  ){
    tm <- TimeMap(map,...)
    obj <- as(tm,"BoundLinDecompOp")
    return(obj)
  }
)

#' automatic title
#' 
#' @param object no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
   f= "getCompartmentalMatrixFunc",
      signature(object="BoundLinDecompOp"),
      definition=function(object){
          getFunctionDefinition(object)
   }
)
