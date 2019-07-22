setClass(
  Class = "OutFluxList_by_PoolIndex",
  contains=c("list")
)

#' constructor from a normal list
#'
#' after checking the elememts
setMethod(
    "OutFluxList_by_PoolIndex"
    ,signature=signature(l="list")
    ,definition=function(l){
        checkTargetClassOfElements(l,targetClassName='OutFlux_by_PoolIndex')
        as(l,'OutFluxList_by_PoolIndex')
    }
)
