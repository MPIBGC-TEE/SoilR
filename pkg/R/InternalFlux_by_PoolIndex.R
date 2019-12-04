#setMethod(
#  f="InternalFlux_by_PoolIndex",
#  signature=c(func='function'),
#  def=function(func,...){
#    pc<-InternalFlux_by_PoolIndex(...)
#    intFl=as(pc,'InternalFlux_by_PoolIndex')
#    intFl@func<-func
#    return(intFl)
#  }
#)


#' constructor from an ordered pair of PoolIndex (integer like) objects and a function with vector argument 
#' @param func A function f(X,t) where X is a vector of the state variables. 
#' This form is required internally by the solvers and supported for backward compatibility with earlier versions of SoilR.
#' Note that the function func given in this form can not be transformed to a different ordering of state variables, since the location of a state variable in the vector argument depends on a specific order and will be 'hardcoded' into your function. 
#' See \code{\link{InternalFlux_by_PoolName}} for the new more powerful interface which allows subsequent reordering of the state variables by using the names of the state variables as formal arguments for \code{func}. In this case SoilR can infer (and later adapt) the
#' vector argument form needed for the solvers.

#' constructor from an ordered pair of PoolIndex (integer like) objects 
setMethod(
  f="InternalFlux_by_PoolIndex",
  signature=c(
    func='function'
    ,sourceIndex='numeric'
    ,destinationIndex='numeric'
    ,src_to_dest='missing'),
  def=function(func,sourceIndex,destinationIndex){

    new(
        'InternalFlux_by_PoolIndex'
        ,sourceIndex=PoolIndex(sourceIndex)
        ,destinationIndex=PoolIndex(destinationIndex)
        ,func=func
    )
  }
)



#' automatic title
#' 
#' @param object no manual documentation
#' @param timeSymbol no manual documentation
#' @param poolNames no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
    f='getFunctionDefinition'
    ,signature=c(object='InFluxList_by_PoolName')
    ,definition=function(object){

    }
)
