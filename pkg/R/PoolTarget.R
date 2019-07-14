setClass(
   Class="PoolTarget",
   slots=c(destinationId='PoolId')
)

#' constructor
PoolTarget=function(destination){
  new('PoolTarget',destinationId=GeneralPoolId(destination))
}

#' convert the source pool id to a number if necessary
setMethod(
  f="by_PoolIndex",
  signature=c(obj='PoolTarget'),
  def=function(obj,poolNames){
    new(
        'PoolTarget'
        ,destinationId=PoolIndex(id=obj@destinationId,poolNames)
    )
  }
)

#' convert the source pool id to a name if necessary
setMethod(
  f="by_PoolName",
  signature=c(obj='PoolTarget'),
  def=function(obj,poolNames){
    new(
        'PoolTarget'
        ,destinationId=PoolName(id=obj@destinationId,poolNames)
    )
  }
)
