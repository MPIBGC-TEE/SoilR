setClass(
  Class="ConstantOutFluxRate",
  slots=c(sourceId='PoolId',rate_constant='numeric')
)

ConstantOutFluxRate<-function(sourceId,rate_constant){
    if (rate_constant<0){
      stop(
        "Negative rate constant. 
        A rate_constant defines a flux F with F = rate_constant*pool_content. 
        Since fluxes have to be positive and pool contents are positive
        rate constants have to be positive too."
      )
    }
    new('ConstantOutFluxRate',sourceId=GeneralPoolId(id=sourceId),rate_constant=rate_constant)
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
