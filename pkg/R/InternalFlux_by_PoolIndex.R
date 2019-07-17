setClass(
  Class="InternalFlux_by_PoolIndex",
  # contains="PoolConnection", we do not want to iherit mehtods...
  slots=c(sourceIndex='PoolIndex',destinationIndex='PoolIndex',func='function')
)

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
#' @param func A function f(X,t) where X is a vector of the state varaibles. 
#' This form is required internally by the solvers and supported for backward compatibility with earlier versions of SoilR.
#' Note that the function func given in this form can not be tranformed to a different ordering of state variables, since the location of a state variable in the vector argument depends on a specific orderand will be 'hardcoded' into your function. 
#' See \link{\code{InternalFlux_by_PoolNames}} for the new more powerful interface which allows subsequent reordering of the state variables by using the names of the state variables as formal arguments for \code{func}. In this case SoilR can infer (and later adapt) the
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
#setMethod(
#  f="InternalFlux_by_PoolIndex",
#  signature=c(source='missing',destination='missing',src_to_dest='character',func='function'),
#  def=function(src_to_dest,func){
#    InternalFlux_by_PoolIndex(
#        source=getSender(src_to_dest)
#        ,destination=getRecipient(src_to_dest)
#        ,func=func
#    )
#  }
#)
