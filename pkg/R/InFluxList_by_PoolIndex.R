setClass(
  Class = "InFluxList_by_PoolIndex",
  contains=c("list")
)

#' constructor from a normal list
#'
#' after checking the elememts
setMethod(
    "InFluxList_by_PoolIndex"
    ,signature=signature(object="list")
    ,definition=function(object){
        checkTargetClassOfElements(object,targetClassName='InFlux_by_PoolIndex')
        as(object,'InFluxList_by_PoolIndex')
    }
)
