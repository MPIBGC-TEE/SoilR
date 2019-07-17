setClass(
  Class="ConstantInFluxRate",
  contains="PoolTarget",
  slots=c(rate_constant='numeric')
)

ConstantInFluxRate<-function(rate_constant,...){
    if (rate_constant<0){
      stop(
        "Negative rate constant. 
        A rate_constant defines a flux F with F = rate_constant*pool_content. 
        Since fluxes have to be positive and pool contents are positive
        rate constants have to be positive too."
      )
    }
    ps<-PoolTarget(...)
    cofr=as(ps,'ConstantInFluxRate')
    cofr@rate_constant<-rate_constant
    return(cofr)
}

