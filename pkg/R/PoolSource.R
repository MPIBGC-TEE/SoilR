setClass(
   Class="PoolSource",
   slots=c(sourceId='PoolId')
)

#' constructor
PoolSource=function(source){
    new('PoolSource',sourceId=GeneralPoolId(id=source))
}

#' convert the source pool id to a number if necessary
setMethod(
  f="by_PoolIndex",
  signature=c(obj='PoolSource'),
  def=function(obj,poolNames){
    new(
        'PoolSource'
        ,sourceId=PoolIndex(id=obj@sourceId,poolNames)
    )
  }
)

#' convert the source pool id to a name if necessary
setMethod(
  f="by_PoolName",
  signature=c(obj='PoolSource'),
  def=function(obj,poolNames){
    new(
        'PoolSource'
        ,sourceId=PoolName(id=obj@sourceId,poolNames)
    )
  }
)
