setClass(
   Class="ConstantInFluxRate_by_PoolName",
   slots=c(destinationName='PoolName',rate_constant='numeric')
)

ConstantInFluxRate_by_PoolName<-function(destinationName,rate_constant){
    if (rate_constant<0){
      stop(
        "Negative rate constant. 
        A rate_constant defines a flux F with F = rate_constant*pool_content. 
        Since fluxes have to be positive and pool contents are positive
        rate constants have to be positive too."
      )
    }
    new(
        'ConstantInFluxRate_by_PoolName'
        ,destinationName=PoolName(id=destinationName)
        ,rate_constant=rate_constant
    )
}
#' new object with the source pool id converted to a PoolIndex if necessary 
setMethod(
  f="by_PoolIndex",
  signature=c(obj='ConstantInFluxRate_by_PoolName'),
  def=function(obj,poolNames){
      new(
        "ConstantInFluxRate_by_PoolIndex"
        ,destinationIndex=PoolIndex(id=obj@destinationName,poolNames)
        ,rate_constant=obj@rate_constant
      )
  }
)
