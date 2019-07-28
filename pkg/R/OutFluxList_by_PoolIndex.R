setClass(
  Class = "OutFluxList_by_PoolIndex",
  contains=c("list")
)

#' constructor from a normal list
#'
#' after checking the elememts
setMethod(
    "OutFluxList_by_PoolIndex"
    ,signature=signature(object="list")
    ,definition=function(object){
        checkTargetClassOfElements(object,targetClassName='OutFlux_by_PoolIndex')
        as(object,'OutFluxList_by_PoolIndex')
    }
)
