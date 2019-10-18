#' Objects that have a source and a destination described by integer like objects  ( of class PoolIndex)
#'
#' Examples are internal Fluxes and Fluxrates
#' Their 'topologic' part and many related sanity checks are implemented here rather than 
#' in every function that uses fluxes or rates
#' The methods are also essential for the translation from (internal) 
#' flux lists
#' to the respective parts of compartmental matrices and back
setClass(
   Class="PoolConnection_by_PoolIndex",
   slots=c(sourceIndex='PoolIndex',destinationIndex='PoolIndex')
)

#' constructor from an ordered pair of PoolId objects
setMethod(
  f="PoolConnection_by_PoolIndex",
  signature=c(src_to_dest='missing'),
  def=function(source,destination){
    new(
        'PoolConnection_by_PoolIndex'
        ,sourceIndex=GeneralPoolId(source)
        ,destinationIndex=GeneralPoolId(destination)
    )
  }
)

#' constructor from strings of the form '1_to_2'
setMethod(
  f="PoolConnection_by_PoolIndex",
  signature=c(source='missing',destination='missing',src_to_dest='character'),
  def=function(src_to_dest){
    PoolConnection_by_PoolIndex(
        source=getSender(src_to_dest)
        ,destination=getRecipient(src_to_dest)
    )
  }
)

#' @auto

#' @auto

#' @auto
setMethod(
  f="check_pool_ids",
  signature=c(obj='PoolConnection_by_PoolIndex',pools='integer'),
  def=function(obj,pools){
     np=pools
     dest<-PoolIndex(obj@destinationIndex)
     src<-PoolIndex(obj@sourceIndex)
     if (dest> np){stop("The index of the destination pool must be smaller than the number of pools")}
     if (src> np){stop("The index of the source pool must be smaller than the number of pools")}
  }
)




##' new object with the source pool id and the destination pool id guaranteed to be of class PoolName 
##'
##' converts the ids if necessary otherwise returns an 
##' identical object
#setMethod(
#  f="by_PoolName",
#  signature=c(obj='PoolConnection_by_PoolIndex'),
#  def=function(obj,poolNames){
#    new(
#        'PoolConnection_by_PoolName'
#        ,sourceName=PoolName(id=obj@sourceId,poolNames)
#        ,destinationName=PoolName(id=obj@destinationIndex,poolNames)
#    )
#  }
#)
#
##' for lists and vectors check if the elements are PoolConnection_by_PoolIndexs
#elements_are_PoolConnections_by_PoolIndex<-function(vec_or_list){
#      all(
#        as.logical(
#            lapply(
#                vec_or_list
#                ,function(el){inherits(el,'PoolConnection_by_PoolIndex')}
#            )
#        )
#      )
#}
#elements_are_Indexed_by_PoolIndex<-function(vec_or_list){
#      all(
#        as.logical(
#            lapply(
#                vec_or_list
#                ,function(el){
#                        is.numeric(el@sourceId)&is.numeric(el@destinationIndex)
#                 }
#            )
#        )
#      )
#}
