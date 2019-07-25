#' Objects that have a source and a destination determined by a string like object of class PoolName
#'
#' Examples are internal Fluxex and Fluxrates
#' Their 'topologic' part and many related sanity checks are implemented here rather than 
#' in every function that uses fluxes or rates
#' The methods are also essential for the translation from (internal) 
#' flux lists
#' to the respective parts of compartmental matrices and back
setClass(
   Class="PoolConnection_by_PoolName",
   slots=c(sourceName='PoolName',destinationName='PoolName')
)

#' constructor from an ordered pair of PoolName objects
setMethod(
  f="PoolConnection_by_PoolName",
  signature=c(src_to_dest='missing'),
  def=function(source,destination){
    new(
        'PoolConnection_by_PoolName'
        ,sourceName=PoolName(source)
        ,destinationName=PoolName(destination)
    )
  }
)

##' constructor from strings of the form 'x->y'
#setMethod(
#  f="PoolConnection_by_PoolName",
#  signature=c(source='missing',destination='missing',src_to_dest='character'),
#  def=function(src_to_dest){
#    PoolConnection_by_PoolName(
#         sourceName=getSender(src_to_dest)
#        ,destinationName=getRecipient(src_to_dest)
#    )
#  }
#)
#setMethod(
#  f="check_pool_ids",
#  signature=c(obj='PoolConnection_by_PoolName',pools='character'),
#  def=function(obj,pools){
#     np=pools
#     dest<-PoolIndex(obj@destinationName)
#     src<-PoolIndex(obj@sourceName)
#     if (dest> np){stop("The index of the destination pool must be smaller than the number of pools")}
#     if (src> np){stop("The index of the source pool must be smaller than the number of pools")}
#  }
#)



#' new object with the source pool id and the destination pool id guranteed to be of class PoolIndex
#'
#' converts the ids if necessary otherwise returns an 
#' identical object
setMethod(
  f="by_PoolIndex",
  signature=c(obj='PoolConnection_by_PoolName'),
  def=function(obj,poolNames){
      new(
            'PoolConnection_by_PoolIndex'
            ,sourceId       =PoolIndex(id=obj@sourceName,poolNames)
            ,destinationId  =PoolIndex(id=obj@destinationName,poolNames)
      ) 
  }
)

#' for lists and vectors check if the elements are PoolConnections_by_PoolName
#elements_are_PoolConnections_by_PoolName<-function(vec_or_list){
#      all(
#        as.logical(
#            lapply(
#                vec_or_list
#                ,function(el){inherits(el,'PoolConnection_by_PoolName')}
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
#                        is.numeric(el@sourceId)&is.numeric(el@destinationId)
#                 }
#            )
#        )
#      )
#}
