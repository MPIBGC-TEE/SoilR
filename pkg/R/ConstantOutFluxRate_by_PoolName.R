setClass(
  Class="ConstantOutFluxRate_by_PoolName",
  slots=c(sourceName='PoolName',rate_constant='numeric')
)

ConstantOutFluxRate_by_PoolName<-function(sourceName,rate_constant){
    if (rate_constant<0){
      stop(
        "Negative rate constant. 
        A rate_constant defines a flux F with F = rate_constant*pool_content. 
        Since fluxes have to be positive and pool contents are positive
        rate constants have to be positive too."
      )
    }
    new(
        'ConstantOutFluxRate_by_PoolName'
        ,sourceName=PoolName(id=sourceName)
        ,rate_constant=rate_constant
    )
}
#' new object with the source pool id converted to a PoolIndex if necessary 
setMethod(
  f="by_PoolIndex",
  signature=c(obj='ConstantOutFluxRate_by_PoolName'),
  def=function(obj,poolNames){
      new(
          "ConstantOutFluxRate_by_PoolIndex"
           ,sourceIndex=PoolIndex(id=obj@sourceName,poolNames)
           ,rate_constant=obj@rate_constant
      )
  }
)

