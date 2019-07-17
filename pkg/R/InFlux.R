setClass(
  Class="InFlux",
  contains="PoolTarget",
  slots=c(func='function')
)

setMethod(
  f="InFlux",
  signature=c(map='function'),
  def=function(map,...){
    pc<-PoolTarget(...)
    fl=as(pc,'InFlux')
    fl@func<-map
    return(fl)
  }
)
