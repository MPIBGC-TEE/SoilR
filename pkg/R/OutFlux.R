setClass(
  Class="OutFlux",
  contains="PoolSource",
  slots=c(func='function')
)

setMethod(
  f="OutFlux",
  signature=c(map='function'),
  def=function(map,...){
    pc<-PoolSource(...)
    fl=as(pc,'OutFlux')
    fl@func<-map
    return(fl)
  }
)
