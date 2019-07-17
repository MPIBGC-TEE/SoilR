setClass(
   Class="PoolSource",
   slots=c(sourceId='PoolId')
)

#' constructor
PoolSource=function(source){
    new('PoolSource',sourceId=GeneralPoolId(id=source))
}

#' new object with the source pool id converted to a PoolIndex if necessary 
setMethod(
  f="by_PoolIndex",
  signature=c(obj='PoolSource'),
  def=function(obj,poolNames){
      obj@sourceId=PoolIndex(id=obj@sourceId,poolNames)
      obj
  }
)

#' new object with the source pool id converted to a PoolName if necessary 
setMethod(
  f="by_PoolName",
  signature=c(obj='PoolSource'),
  def=function(obj,poolNames){
      obj@sourceId=PoolName(id=obj@sourceId,poolNames)
      obj
  }
)
