
setClass(
  Class = "InternalFluxList_by_PoolIndex",
  contains=c("list")
)

#' constructor from a normal list
#'
#' after checking the elememts
setMethod("InternalFluxList_by_PoolIndex",
    signature=signature(object="list"),
    definition=function(object){
        checkTargetClassOfElements(l,targetClassName='InternalFlux_by_PoolIndex')
        as(l,'InternalFluxList_by_PoolIndex')
    }
)
