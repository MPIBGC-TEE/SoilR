#' constructor from a PoolIndex (integer like) objects and a function with vector argument 
#' @param func A function f(X,t) where X is a vector of the state variables. 
#' @param sourceIndex index of source pool
#' This form is required internally by the solvers and supported for backward compatibility with earlier versions of SoilR.
#' Note that the function func given in this form can not be transformed to a different ordering of state variables, since the location of a state variable in the vector argument depends on a specific order and will be 'hardcoded' into your function. 
#' See \code{\link{OutFlux_by_PoolName}} for the new, more powerful interface which allows subsequent reordering of the state variables by using the names of the state variables as formal arguments for \code{func}. In this case SoilR can infer (and later adapt) the
#' vector argument form needed for the solvers.
#' constructor from an ordered pair of PoolIndex (integer like) objects 
setMethod(
  f="OutFlux_by_PoolIndex",
  signature=c(
    func='function'
    ,sourceIndex='numeric'
  )
  ,def=function(func,sourceIndex){
    new(
        'OutFlux_by_PoolIndex'
        ,sourceIndex=PoolIndex(sourceIndex)
        ,func=func
    )
  }
)
#setMethod(
#  f="OutFlux_by_PoolIndex",
#  signature=c(source='missing',destination='missing',src_to_dest='character',func='function'),
#  def=function(src_to_dest,func){
#    OutFlux_by_PoolIndex(
#        source=getSender(src_to_dest)
#        ,destination=getRecipient(src_to_dest)
#        ,func=func
#    )
#  }
#)
