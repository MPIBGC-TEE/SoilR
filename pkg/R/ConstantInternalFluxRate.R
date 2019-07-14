setClass(
  Class="ConstantInternalFluxRate",
  contains="PoolConnection",
  slots=c(rate_constant='numeric')
)
# constructor
ConstantInternalFluxRate <- function(rate_constant,...){
  pc<-PoolConnection(...)
  cifr=as(pc,'ConstantInternalFluxRate')
  cifr@rate_constant<-rate_constant
  return(cifr)
}
