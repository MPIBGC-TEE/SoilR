setClass(
  Class="ConstantInternalFluxRate",
  contains="Internal",
  slots=c(rate_constant='numeric')
)
setMethod(
  f="ConstantInternalFluxRate",
  signature=c(source='numeric',destination='numeric',rate_constant='numeric'),
  def=function(source,destination,rate_constant){
    source_ind=PoolIndex(source)
    destination_ind=PoolIndex(destination)
    if (rate_constant<0){
      error(
        "Negative rate constant. 
        A rate_constant defines a flux = rate_constant*pool_content. 
        Since fluxes have to be positive and pool contents are positive
        rate constants have to be positive too."
      )
    }
    return(new('PoolIndex',number))
  }
)
