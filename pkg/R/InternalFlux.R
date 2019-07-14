setClass(
  Class="InternalFlux",
  contains="PoolConnection",
  slots=c(func='function')
)

setMethod(
  f="InternalFlux",
  signature=c(map='function'),
  def=function(map,...){
    pc<-PoolConnection(...)
    intFl=as(pc,'InternalFlux')
    intFl@func<-map
    return(intFl)
  }
)
