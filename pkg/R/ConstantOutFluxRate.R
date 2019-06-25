setClass(
  Class="ConstantOutFluxRate",
  contains="PoolSource",
  slots=c(rate_constant='numeric')
)
# constructors
setMethod(
  f="ConstantOutFluxRate",
  signature=c(source='numeric',rate_constant='numeric'),
  def=function(source,rate_constant){
    source_ind=PoolIndex(source)
    if (rate_constant<0){
      stop(
        "Negative rate constant. 
        A rate_constant defines a flux = rate_constant*pool_content. 
        Since fluxes have to be positive and pool contents are positive
        rate constants have to be positive too."
      )
    }
    return(
      new(
        'ConstantOutFluxRate'
        ,source=source_ind
        ,rate_constant=rate_constant
      )
    )
  }
)
setMethod(
  f="ConstantOutFluxRate",
  signature=c(source='character',rate_constant='numeric'),
  def=function(source,rate_constant){
    src_ind<-as.integer(source)
    # call the main constructor after converting the name to an int
    return(ConstantOutFluxRate(src_ind,rate_constant))
  }
)
