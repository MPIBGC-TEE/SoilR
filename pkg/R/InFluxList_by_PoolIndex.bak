setClass(
  Class = "InFluxList_by_PoolIndex",
  contains=c("list")
)

#' constructor from a normal list
#'
#' after checking the elememts
setMethod(
    "InFluxList_by_PoolIndex"
    ,signature=signature(l="list")
    ,definition=function(l){
        checkTargetClassOfElements(l,targetClassName='InFlux_by_PoolIndex')
        as(l,'InFluxList_by_PoolIndex')
    }
)
