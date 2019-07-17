#' Objects that have a source and a destination
#'
#' Examples are internal Fluxex and Fluxrates
#' Their 'topologic' part and many related sanity checks are implemented here rather than 
#' in every function that uses fluxes or rates
#' The methods are also essential for the translation from (internal) 
#' flux lists
#' to the respective parts of compartmental matrices and back
setClass(
   Class="PoolConnection",
   slots=c(sourceId='PoolId',destinationId='PoolId')
)

#' constructor from an ordered pair of PoolId objects
setMethod(
  f="PoolConnection",
  signature=c(src_to_dest='missing'),
  def=function(source,destination){
    new(
        'PoolConnection'
        ,sourceId=GeneralPoolId(source)
        ,destinationId=GeneralPoolId(destination)
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



#' new object with the source pool id and the destination pool id guranteed to be of class PoolIndex
#'
#' converts the ids if necessary otherwise returns an 
#' identical object
setMethod(
  f="by_PoolIndex",
  signature=c(obj='PoolConnection'),
  def=function(obj,poolNames){
    obj@sourceId=PoolIndex(id=obj@sourceId,poolNames)
    obj@destinationId=PoolIndex(id=obj@destinationId,poolNames)
    obj
  }
)

#' new object with the source pool id and the destination pool id guranteed to be of class PoolName 
#'
#' converts the ids if necessary otherwise returns an 
#' identical object
setMethod(
  f="by_PoolName",
  signature=c(obj='PoolConnection'),
  def=function(obj,poolNames){
    obj@sourceId=PoolName(id=obj@sourceId,poolNames)
    obj@destinationId=PoolName(id=obj@destinationId,poolNames)
    obj
  }
)

#' for lists and vectors check if the elements are PoolConnections
elements_are_PoolConnections<-function(vec_or_list){
      all(
        as.logical(
            lapply(
                vec_or_list
                ,function(el){inherits(el,'PoolConnection')}
            )
        )
      )
}
elements_are_Indexed_by_PoolIndex<-function(vec_or_list){
      all(
        as.logical(
            lapply(
                vec_or_list
                ,function(el){
                        is.numeric(el@sourceId)&is.numeric(el@destinationId)
                 }
            )
        )
      )
}
