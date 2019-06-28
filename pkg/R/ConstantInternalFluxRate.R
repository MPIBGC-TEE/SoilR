setClass(
  Class="ConstantInternalFluxRate",
  contains="PoolConnection",
  slots=c(rate_constant='numeric')
)
setMethod(
  f="ConstantInternalFluxRate",
  signature=c(source='numeric',destination='numeric',src_to_dest='missing',rate_constant='numeric'),
  def=function(source,destination,rate_constant){
    source_ind=PoolIndex(source)
    destination_ind=PoolIndex(destination)
    if (rate_constant<0){
      stop(
        "Negative rate constant. 
        A rate_constant defines a flux = rate_constant*pool_content. 
        Since fluxes have to be positive and pool contents are positive
        rate constants have to be positive too."
      )
    }
    return(new('ConstantInternalFluxRate',source=source_ind,destination=destination_ind,rate_constant=rate_constant))
  }
)
setMethod(
  f="ConstantInternalFluxRate",
  signature=c(source='missing',destination='missing',src_to_dest='character',rate_constant='numeric'),
  def=function(src_to_dest,rate_constant){
    source<-getSender(src_to_dest)
    destination<-getRecipient(src_to_dest)
    return(ConstantInternalFluxRate(source=source,destination=destination,rate_constant=rate_constant))
  }
)
