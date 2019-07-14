#' Objects that have a source and a destination
#'
#' Examples are internal Fluxex and Fluxrates
#' Many sanity checks are implemented here rather than 
#' in every function that uses Fluxes or rates
#' The methods are also essential for the translation from (internal) 
#' flux lists
#' to the respective parts of compartmental matrices and back
setClass(
   Class="PoolConnection",
   contains=c("PoolSource","PoolTarget"),
)

#' constructor from an ordered pair of PoolId objects
setMethod(
  f="PoolConnection",
  signature=c(src_to_dest='missing'),
  def=function(source,destination){
    new(
        'PoolConnection'
        ,sourceId=PoolId(source)
        ,destinationId=PoolId(destination)
    )
  }
)

#' constructor from strings of the form '1_to_2'
setMethod(
  f="PoolConnection",
  signature=c(source='missing',destination='missing',src_to_dest='character'),
  def=function(src_to_dest){
    PoolConnection(
        source=getSender(src_to_dest)
        ,destination=getRecipient(src_to_dest)
    )
  }
)
setMethod(
  f="check_pool_ids",
  signature=c(obj='PoolConnection',pools='integer'),
  def=function(obj,pools){
     np=pools
     dest<-PoolIndex(obj@destinationId)
     src<-PoolIndex(obj@sourceId)
     if (dest> np){stop("The index of the destination pool must be smaller than the number of pools")}
     if (src> np){stop("The index of the source pool must be smaller than the number of pools")}
  }
)
